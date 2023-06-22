if (!"tidyverse" %in% .packages()) library(tidyverse)
if (!"mlogit" %in% .packages()) library(mlogit)



# Data Reshaping ----

# Same side function
same_side = function(x, y, m=0) {
  
  out <- case_when(x > m & y > m ~ 1,
                   x < m & y < m ~ 1,
                   x > m & y < m ~ 0,
                   x < m & y > m ~ 0,
                   T ~ NA_real_)
  return(out)
  
}

# Data prep function
bes_to_long <- function(bes, square=F, shift_center=NULL) {
  
  # Squared dist option
  if (square) {
    bes <- bes %>%
      mutate(across(matches("Dist"), function(x) x^2))
  }
  
  # Shift center of categorisation effects
  if (!is.null(shift_center)) {
    bes <- bes %>%
      mutate(euSameSideCon = same_side(euSelf, parties$eu[rownames(parties) == "Con"], m=shift_center),
             euSameSideLab = same_side(euSelf, parties$eu[rownames(parties) == "Lab"], m=shift_center),
             euSameSideLD = same_side(euSelf, parties$eu[rownames(parties) == "LD"], m=shift_center),
             euSameSideGreen = same_side(euSelf, parties$eu[rownames(parties) == "Green"], m=shift_center),
             euSameSideBrexit = same_side(euSelf, parties$eu[rownames(parties) == "Brexit"], m=shift_center))
  }
  
  # Code Vars
  data <- bes %>%
    filter(complete.cases(.)) %>%
    mutate(winConstituencyNon = 0,
           likeNon = 0,
           leaderLikeNon = 0,
           redistSameSideNon = 0,
           euSameSideNon = 0,
           redistDistNon = 0,
           euDistNon = 0,
           rownum = 1:n())
  
  # Prepare variable names for reshape
  names(data) <- names(data) %>% 
    gsub("winConstituency", "winProbDist", .) %>%
    gsub("Like", "LikeDist", .) %>%
    gsub("like", "likeDist", .) %>% 
    gsub("Side", "SideDist", .) #makes things easier in the reshape
  
  # Reshaping
  dist <- data %>%
    pivot_longer(c(5:9,12:(ncol(data)-1)), names_to=c("dimension","Party"), names_sep="Dist", values_to="values") %>%
    pivot_wider(names_from=dimension, values_from="values") %>%
    mutate(chosen = Party == vote19,
           lagChosen = Party == vote17,
           chosenID = Party == partyID)
  
  # Sanity check
  if (!nrow(data) * length(unique(data$vote19)) == nrow(dist)) stop("Wrong dimensions")
  
  # Return
  return(dist)
}



# Running models ----

# Function to split data into train and test
train_test_split <- function(data) {
  s <- sample(data$rownum, size=round(0.6*length(data$rownum)), replace=F)
  train <- data[data$rownum %in% s,]
  test <- data[!data$rownum %in% s,]
  return(list(train=train, test=test))
}

# Function to generate formula from main vars and controls
controls_to_fomula <- function(iv, controls=NULL) {
  if (is.null(controls)) plus = NULL else plus = " + "
  f <- str_c("chosen ~ ",
             str_c(iv, collapse=" + "),
             plus,
             str_c(controls, collapse=" + "),
             " + Party | 0 | 0")
  return(as.formula(f))
}

# Function to generate both prox plus prox+categorisation model formulas
generate_both_formulas <- function(controls=NULL) {
  p_formula <- controls_to_fomula(iv=c("redist", "eu"), controls)
  c_formula <- controls_to_fomula(iv=c("redist", "eu", "redistSameSide", "euSameSide"), controls)
  return(list(p_formula=p_formula, c_formula=c_formula))
}

# Function to run a single model
run_model <- function(formula, train_data) {
  train_mlogit <- mlogit.data(train_data, choice="chosen", shape="long", chid.var="id")
  m <- mlogit(formula, data=train_mlogit)
  return(m)
}

# Function that estimates the models in full
estimate_models <- function(data, controls){
  # Prepare data
  train_test = train_test_split(data)
  train <- train_test$train
  test <- train_test$test
  
  # Prepare functions
  formulas <- generate_both_formulas(controls)
  p_formula <- formulas$p_formula
  c_formula <- formulas$c_formula
  
  # Run both models
  p_model <- run_model(p_formula, train)
  c_model <- run_model(c_formula, train)
  
  # Return
  return(list(p_model=p_model, c_model=c_model, test=test))
}



# Simulation functions ----

simulate_votes <- function(nobs, dist, models, labsim, shift=0) {
  
  # Initialise arrays of simulated probabilities
  simProbs_p <- simProbs_c <- array(numeric(n_obs*length(unique(bes$vote19))*length(labsim)),
                                    dim=c(n_obs,
                                          length(unique(bes$vote19)),
                                          length(labsim)))
  
  # Extract models
  model_p <- models$p_model
  model_c <- models$c_model
  
  # Simulation loop
  for (i in 1:length(labsim)) {
    
    tempDist <- dist
    tempDist$eu[tempDist$Party == "Lab"] <- abs(tempDist$euSelf[tempDist$Party == "Lab"] - labsim[i])
    tempDist$euSameSide[tempDist$Party == "Lab"] <- same_side(tempDist$euSelf[tempDist$Party == "Lab"], labsim[i], m=shift)
    simProbs_p[,,i] <- predict(model_p, newdata=tempDist)
    simProbs_c[,,i] <- predict(model_c, newdata=tempDist)
    
  }
  
  # Rename rows and columns
  colnames(simProbs_p) <- colnames(simProbs_c) <- unique(dist$Party)
  rownames(simProbs_p) <- rownames(simProbs_c) <- unique(dist$id)
  
  # Return
  return(list(simProbs_p=simProbs_p, simProbs_c=simProbs_c))
}






