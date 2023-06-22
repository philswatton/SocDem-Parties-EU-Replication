# Dependencies
if (!"tidyverse" %in% .packages()) library(tidyverse)
if (!"mlogit" %in% .packages()) library(mlogit)
if (!"texreg" %in% .packages()) library(texreg)
if (!"xtable" %in% .packages()) library(xtable)
if (!"ggpubr" %in% .packages()) library(ggpubr)
if (!"ggrepel" %in% .packages()) library(ggrepel)
if (!"haven" %in% .packages()) library(haven)
if (!"labelled" %in% .packages()) library(labelled)
if (!"survey" %in% .packages()) library(survey)
if (!"srvyr" %in% .packages()) library(srvyr)

# Vars ----
phue <- c("#00AEEF", "#E4003B", "#FBB347", "#6AB023", "purple")




# Tables of results ----

write_model_tex <- function(model_list, file="reg.tex", caption="Regression Results") {
  texreg(model_list,
         file=str_c("tables/",file),
         custom.coef.map=list("eu"="EU Distance",
                              "redist"="Redistribution Distance",
                              "euSameSide"="EU Same Side",
                              "redistSameSide"="Redistribution Same Side",
                              "winProb"="Win Probability",
                              "like"="Like Party",
                              "leaderLike"="Like Leader",
                              "lagChosenTRUE"="Previously Voted For",
                              "chosenIDTRUE"="Party ID'd With",
                              "PartyCon"="Conservative Dummy",
                              "PartyLab"="Labour Dummy",
                              "PartyLD"="Liberal Democrat Dummy",
                              "PartyGreeb"="Green Dummy",
                              "PartyNon"="Non-Voter Dummy"),
         custom.model.names=c("Proximity", "Proximity + Categorisation"),
         caption=caption,
         caption.above=T,
         longtable=T,
         use.packages=F,
         float.pos="H")
}





# Cross Validation ----

write_cv_table <- function(results, name) {
  
  # Test data
  test <- results$models$test
  
  # Get Models
  p_model <- results$models$p_model
  c_model <- results$models$c_model
  
  # Predict Probabilities
  predProbs_p <- predict(p_model, test)
  predProbs_c <- predict(c_model, test)
  
  # Calculate predicted vote shares for each party in the sample
  temp_p <- test %>%
    mutate(prob = c(t(predProbs_p))) %>%
    select(id, vote19, Party, prob) %>%
    pivot_wider(names_from = Party,
                values_from = prob)
  temp_c <- test %>%
    mutate(prob =  c(t(predProbs_c))) %>%
    select(id, vote19, Party, prob) %>%
    pivot_wider(names_from = Party,
                values_from = prob)
  
  # Calculate real vote shares for each party in the sample
  preds_all <- temp_p %>%
    select(3:8) %>%
    colSums()
  
  # Build Table
  prop_p <- prop.table(table(temp_p$vote19)) %>% as.data.frame() %>% rename(Party = Var1, `Prop P`=Freq)
  prop_c <- prop.table(table(temp_c$vote19)) %>% as.data.frame() %>% rename(Party = Var1, `Prop C`=Freq)
  prop_true <- (preds_all/sum(preds_all)) %>% as.data.frame()
  names(prop_true) <- c("Sample")
  prop_true$Party <- rownames(prop_true)
  rownames(prop_true) <- NULL
  table <- left_join(left_join(prop_p, prop_c), prop_true) %>% 
    mutate(across(where(is.numeric), ~round(.x, 2)))
  
  # Save
  colnames(table)[2:4] <- c("Proximity", "Proximity + Categorisation", "True")
  table <- table[c(1,4,2:3)]
  xtable(table, caption="Test Sample Proportions") %>% 
    print(caption.placement="top",
          include.rownames=F,
          table.placement="H") %>%
    capture.output() %>%
    writeLines(str_c("tables/", name, ".tex"), sep="\n")
  
}




# Coef Plots ----

get_coefs <- function(model, model_name, coef_range){
  coefs <- summary(model)$CoefTable[coef_range,1:2] %>%
    as.data.frame() %>%
    rename(est = Estimate,
           se = `Std. Error`) %>%
    mutate(conf_low = est - 1.96*se,
           conf_high = est + 1.96*se,
           model = model_name,
           var = rownames(.))
  return(coefs)
}

make_coef_plot <- function(model_list, file_name="coef.pdf") {
  # Get Coefs
  coef_p <- get_coefs(model_list$p_model, model_name="Proximity", coef_range=1:2)
  coef_c <- get_coefs(model_list$c_model, model_name="Proximity + Categorisation", coef_range=1:4)
  
  # Put them together
  coef <- rbind(coef_p, coef_c, make.row.names=F) %>%
    mutate(var = case_when(var == "redist" ~ "Redistribution Distance",
                           var == "eu" ~ "EU Distance",
                           var == "redistSameSide" ~ "Redistribution Same Side",
                           var == "euSameSide" ~ "EU Same Side"))
  
  # Make plot
  ggplot(coef, aes(y=est, x=var, color=model, shape=model, label=round(est,2))) +
    geom_point(size=2) +
    geom_text_repel(color="black") +
    geom_linerange(aes(ymin=conf_low, ymax=conf_high), size=1) +
    scale_y_continuous(limits = c(-1.5,1.5), breaks=seq(-1.5,1.5,0.5)) +
    scale_color_manual("Model", values=c(phue[1:2])) +
    scale_shape_discrete("Model") +
    geom_hline(yintercept=0, color="black") +
    coord_flip() +
    labs(title="Coefficients", y="Estimate", x="Variable") +
    theme_classic() +
    theme(aspect.ratio=1,
          plot.title=element_text(hjust=0.5),
          panel.grid.major=element_line(colour="gray"),
          panel.grid.minor=element_line(colour="gray"))
  
  # save
  ggsave(str_c("plots/", file_name), width=6, height=3)
}





# Simulation Plots ----

# Function to build a single plot
sim_plot <- function(sim_result, parties, plot_title) {
  
  sim_sample <- map_dfr(1:length(labsim), function (x) {
    means <- colMeans(sim_result[,,x])
    return(means)
  })
  
  p <- ggplot() +
    geom_line(aes(y=Lab, x=labsim), sim_sample, color=phue[2], size=1) +
    geom_line(aes(y=Con, x=labsim), sim_sample, color=phue[1], size=1) +
    geom_line(aes(y=LD, x=labsim), sim_sample, color=phue[3], size=1) +
    geom_line(aes(y=Brexit, x=labsim), sim_sample, color=phue[4], size=1) +
    geom_line(aes(y=Green, x=labsim), sim_sample, color=phue[5], size=1) +
    geom_line(aes(y=Non, x=labsim), sim_sample, color="black", size=1) +
    geom_vline(xintercept=parties$eu[2], size=1) +
    geom_vline(xintercept=labsim[which.max(sim_sample$Lab)], size=1, linetype="dashed", color="#E4003B") +
    geom_vline(xintercept=labsim[which.max(sim_sample$Lab - sim_sample$Con)], size=1, linetype="twodash", color="#00AEEF") +
    scale_y_continuous(limits=c(0,0.5), expand=c(0,0)) +
    scale_x_continuous(expand=c(0,0)) +
    labs(title=str_c(plot_title, " Simulation"), y="Estimated Vote Share", x="Simulated Labour EU Position") +
    theme_classic() +
    theme(aspect.ratio=1,
          plot.title=element_text(hjust=0.5))
  
  return(p)
  
}

# Function to build the combined plot and save it
make_sim_plots <- function(sim_list, file_name, parties) {
  sim_plot_p <- sim_plot(sim_list$simProbs_p, parties, "Proximity")
  sim_plot_c <- sim_plot(sim_list$simProbs_c, parties, "Proximity + Categorisation")
  ggarrange(sim_plot_p, sim_plot_c, ncol=2)
  ggsave(str_c("plots/", file_name), width=6, height=3)
}




# Swing Plots ----

# UNS Calculation Function
uns <- function(num, prob_array) {
  
  # Set up df
  df <- left_join(prob_array[,,num] %>% as.data.frame() %>% mutate(id = as.numeric(rownames(.))), id) %>%
    filter(complete.cases(.)) %>%
    select(-region, -id)
  
  # Convert to survey object
  srv <- as_survey_design(df, id=1, weight=wt)
  
  # Get predictions and % non-voting
  pred <- srv %>%
    summarise(across(.fns=survey_mean)) %>%
    select(-matches("se"), -wt)
  non <- pred[[6]]
  turnout <- 1-non
  
  # Get vote share predictions
  pred <- pred[1:5]
  sum_pred <- sum(pred)
  pred <- pred/sum_pred
  pred$Other <- 0
  
  # Calculate swing
  swing <- national - pred
  
  # Estimate vote shares, assume same turnout for all practical purposes
  est <- const17 %>%
    mutate(Con = Con - swing[[1]],
           Lab = Lab - swing[[2]],
           LD = LD - swing[[3]],
           Green = Green - swing[[4]],
           Brexit = Brexit - swing[[5]],
           Other = Other - swing[[6]],
           across(where(is.numeric), ~case_when(.x < 0 ~ 0,
                                                T ~ .x)),
           
           sum = Con + Lab + LD + Green + Brexit + Other,
           Con = Con/sum,
           Lab = Lab/sum,
           LD = LD/sum,
           Green = Green/sum,
           Brexit = Brexit/sum,
           Other = Other/sum,
           
           ConVote = Con*Total,
           LabVote = Lab*Total,
           LDVote = LD*Total,
           GreenVote = Green*Total,
           BrexitVote = Brexit*Total,
           OtherVote = Other*Total)
  
  # Calculate national vote share
  votes <- est %>%
    select(matches("Vote")) %>%
    colSums()
  names(votes) <- names(votes) %>% gsub("Vote", "", .)
  voteshare <- votes/sum(votes)
  
  
  # Calculate seat winners
  shares <- est %>%
    select(Con, Lab, LD, Green, Brexit, Other)
  seats <- matrix(numeric(nrow(shares)*(ncol(shares))), nrow=nrow(shares),ncol=ncol(shares))
  for (i in 1:nrow(seats)) {
    for (j in 1:ncol(seats)) {
      if (max(shares[i,]) == shares[i,j]) seats[i,j] <- 1
    }
  }
  seatcount <- colSums(seats)
  names(seatcount) <- names(voteshare)
  seatshare <- seatcount / sum(seatcount)
  
  return(list(voteshare=voteshare, seatcount=seatcount, seatshare=seatshare))
  
}

# URS Calculation Function
urs <- function(num, prob_array) {
  
  # Set up df
  df <- left_join(prob_array[,,num] %>% as.data.frame() %>% mutate(id = as.numeric(rownames(.))), id) %>%
    filter(complete.cases(.)) %>%
    select(-id)
  
  # Convert to survey object
  srv <- as_survey_design(df, id=1, weight=wt)
  
  # Get predictions and % non-voting
  pred <- srv %>%
    group_by(region) %>%
    summarise(across(.fns=survey_mean)) %>%
    select(-matches("se"), -wt, -Non)
  
  # Get vote share predictions
  pred <- pred %>%
    mutate(sum = Con + Lab + LD + Brexit + Green,
           Con = Con/sum,
           Lab = Lab/sum,
           LD = LD/sum,
           Brexit = Brexit/sum,
           Green = Green/sum,
           Other = 0) %>%
    select(-sum)
  
  # Calculate swing
  swing <- cbind(pred %>% select(region), as.matrix(region[2:7]) - as.matrix(pred[2:7]))
  names(swing)[2:7] <- str_c(names(swing[2:7]), "_Swing")
  
  # Estimate vote shares, assume same turnout for all practical purposes
  est <- const17 %>%
    left_join(swing) %>%
    mutate(Con = Con - Con_Swing,
           Lab = Lab - Lab_Swing,
           LD = LD - LD_Swing,
           Green = Green - Green_Swing,
           Brexit = Brexit - Brexit_Swing,
           Other = Other - Other_Swing,
           across(where(is.numeric), ~case_when(.x < 0 ~ 0,
                                                T ~ .x)),
           
           sum = Con + Lab + LD + Green + Brexit + Other,
           Con = Con/sum,
           Lab = Lab/sum,
           LD = LD/sum,
           Green = Green/sum,
           Brexit = Brexit/sum,
           Other = Other/sum,
           
           ConVote = Con*Total,
           LabVote = Lab*Total,
           LDVote = LD*Total,
           GreenVote = Green*Total,
           BrexitVote = Brexit*Total,
           OtherVote = Other*Total)
  
  # Calculate national vote share
  votes <- est %>%
    select(matches("Vote")) %>%
    colSums()
  names(votes) <- names(votes) %>% gsub("Vote", "", .)
  voteshare <- votes/sum(votes)
  
  
  # Calculate seat winners
  shares <- est %>%
    select(Con, Lab, LD, Green, Brexit, Other)
  seats <- matrix(numeric(nrow(shares)*(ncol(shares))), nrow=nrow(shares),ncol=ncol(shares))
  for (i in 1:nrow(seats)) {
    for (j in 1:ncol(seats)) {
      if (max(shares[i,]) == shares[i,j]) seats[i,j] <- 1
    }
  }
  seatcount <- colSums(seats)
  names(seatcount) <- names(voteshare)
  seatshare <- seatcount / sum(seatcount)
  
  return(list(voteshare=voteshare, seatcount=seatcount, seatshare=seatshare))
  
}

# Seat Calculation Fn
calc_seat <- function(result) {
  map_dfr(result, function(x) {
    x$seatcount
  }) %>% mutate(labsim = labsim)
}

# Seats plot function
swing_plot <- function(predicted_seats, title) {
  p <- ggplot() +
    geom_line(aes(y=Lab, x=labsim), predicted_seats, color=phue[2], size=1) +
    geom_line(aes(y=Con, x=labsim), predicted_seats, color=phue[1], size=1) +
    geom_line(aes(y=LD, x=labsim), predicted_seats, color=phue[3], size=1) +
    geom_line(aes(y=Brexit, x=labsim), predicted_seats, color=phue[4], size=1) +
    geom_line(aes(y=Green, x=labsim), predicted_seats, color=phue[5], size=1) +
    geom_vline(xintercept=parties$eu[2], size=1) +
    geom_rect(aes(xmin=labsim[min(which(predicted_seats$Lab == max(predicted_seats$Lab)))],
                  xmax=labsim[max(which(predicted_seats$Lab == max(predicted_seats$Lab)))],
                  ymin=0, ymax=533),
              fill=phue[2],
              alpha=0.5) +
    geom_rect(aes(xmin=labsim[min(which(predicted_seats$Lab - predicted_seats$Con == max(predicted_seats$Lab - predicted_seats$Con)))],
                  xmax=labsim[max(which(predicted_seats$Lab - predicted_seats$Con == max(predicted_seats$Lab - predicted_seats$Con)))],
                  ymin=0, ymax=533),
              fill=phue[1],
              alpha=0.5) +
    scale_y_continuous(limits=c(0,533), expand=c(0,0)) +
    scale_x_continuous(expand=c(0,0)) +
    labs(title=title, y="Estimated Seat Count", x="Simulated Labour EU Position") +
    theme_classic() +
    theme(aspect.ratio=1,
          plot.title=element_text(hjust=0.5))
  return(p)
}

# Swing Plot Function
make_swing_plots <- function(sim_list, swing_func, func_name, extra=NULL){
  
  # Extract
  prob_p <- sim_list$simProbs_p
  prob_c <- sim_list$simProbs_c
  
  # Calculate Swings
  swing_p <- map(1:400, ~swing_func(.x, prob_p))
  swing_c <- map(1:400, ~swing_func(.x, prob_c))
  
  # Calculate Seats
  seats_p <- calc_seat(swing_p)
  seats_c <- calc_seat(swing_c)
  
  # Make Plots
  plot_p <- swing_plot(seats_p, str_c(func_name, " Proximity"))
  plot_c <- swing_plot(seats_c, str_c(func_name, " Proximity + Categorisation"))
  
  # Arrange and save
  ggarrange(plot_p, plot_c, ncol=2)
  ggsave(str_c("plots/", tolower(func_name), "_seat", extra, ".pdf"), width=6, height=3)
  
}

# Wrappers
make_uns_plots <- function(sim_list, extra=NULL) {
  make_swing_plots(sim_list=sim_list, swing_func=uns, func_name="UNS", extra=extra)
}
make_urs_plots <- function(sim_list, extra=NULL) {
  make_swing_plots(sim_list=sim_list, swing_func=urs, func_name="URS", extra=extra)
}










