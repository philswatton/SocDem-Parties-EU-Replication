# Lab GE 2019 ----
# Phil Swatton
# University of Essex
# File 04: Clogit


## Packages
library(tidyverse)
library(mclogit)


## Data
bes <- readRDS("data/scaled.rds")%>%
  rename(redistSelf=redist,
         euSelf=eu) #move these 2 lines into file 02 at some point
# bes <- readRDS("data/scaledAlt.rds")
parties <- readRDS("data/parties.rds")




# Create Distance DF ----

# if lower distance better, then need to caputre higher prob of non-voting as lowest distance gets higher
# inverse distance: highest of all distances on a given dim, subtract lowest distance from it
# - if highest is 4, then having a lowest distance of 4 will result in inverse dist = 0 and thus likely not to vote
# - if highest is 4, then having a lowest distance of 1 will result in inverse dist = 3 and thus likely to vote

# Possibly set all of 'nother' attributes to 0 - since there is no utility gain from not voting


## Prepare variables for non-voters, remove LR data
data <- bes %>%
  filter(complete.cases(.)) %>%
  mutate(winConstituencyNon = 0,
         likeNon = 0,
         leaderLikeNon = 0,
         
         # lrSameSideNon = 0,
         redistSameSideNon = 0,
         euSameSideNon = 0,
         # immigSameSideNon = 0,
         # enviroSameSideNon = 0,
         
         # lrDistNon = pmin(lrDistCon, lrDistLab, lrDistLD, lrDistGreen, lrDistBrexit),
         redistDistNon = 0,#max(as.matrix(redistDistCon, redistDistLab, redistDistLD, redistDistGreen, redistDistBrexit)) - pmin(redistDistCon, redistDistLab, redistDistLD, redistDistGreen, redistDistBrexit),
         euDistNon = 0,#max(as.matrix(euDistCon, euDistLab, euDistLD, euDistGreen, euDistBrexit)) - pmin(euDistCon, euDistLab, euDistLD, euDistGreen, euDistBrexit),
         # immigDistNon = 0,
         # enviroDistNon = 0,
         
         rownum = 1:n())


## Preapre variable names for reshape
names(data) <- names(data) %>% 
  gsub("winConstituency", "winProbDist", .) %>%
  gsub("Like", "LikeDist", .) %>%
  gsub("like", "likeDist", .) %>% 
  gsub("Side", "SideDist", .) #makes things easier in the reshape


## Reshaping
dist <- data %>%
  pivot_longer(c(5:9,12:(ncol(data)-1)), names_to=c("dimension","Party"), names_sep="Dist", values_to="values") %>%
  pivot_wider(names_from=dimension, values_from="values") %>%
  mutate(chosen = Party == vote19,
         lagChosen = Party == vote17,
         chosenID = Party == partyID)


## Sanity check
nrow(data) * length(unique(data$vote19)) == nrow(dist) # returns TRUE


## OPTIONAL: Squared distances
# dist2 <- dist %>% mutate(across(all_of(c("redist","eu","immig","enviro")), ~.x^2))




# Run Model ----


## Cross-validation ----

# Splitting the dataset into 60% training and 40% test chunks
set.seed(42)
s <- sample(data$rownum, size=round(0.6*length(data$rownum)), replace=F)
train <- dist[dist$rownum %in% s,]
test <- dist[!dist$rownum %in% s,]


## Sanity check
nrow(train) + nrow(test) == nrow(dist)


## Run models ----

# Proximity-only model
result_p <- mclogit(cbind(chosen,id) ~ redist + eu + winProb + like + leaderLike + lagChosen + chosenID + Party,
                       train)
summary(result_p)
saveRDS(result_p, "results/clogit_p.rds")

# Proximity + Categorisation model
result_c <- mclogit(cbind(chosen,id) ~ redist + redistSameSide + eu + euSameSide + winProb + like + leaderLike + lagChosen + chosenID + Party,
                       train)
summary(result_c)
saveRDS(result_c, "results/clogit_c.rds")



## Get predictions
# predProbs <- predict(result, test, type="link")
predProbs_p <- predict(result_p, test, type="response")
predProbs_c <- predict(result_c, test, type="response")




## Cross-Validation by Total Vote %s ----

# Calculate predicted vote shares for each party in the sample
temp_p <- test %>%
  mutate(prob = predProbs_p) %>%
  select(id, vote19, Party, prob) %>%
  pivot_wider(names_from = Party,
              values_from = prob)
temp_c <- test %>%
  mutate(prob = predProbs_c) %>%
  select(id, vote19, Party, prob) %>%
  pivot_wider(names_from = Party,
              values_from = prob)

# Calculate real vote shares for each party in the sample
preds_all <- temp_p %>%
  select(3:8) %>%
  colSums()

# Compare
prop_p <- prop.table(table(temp_p$vote19)) %>% as.data.frame() %>% rename(Party = Var1, `Prop P`=Freq)
prop_c <- prop.table(table(temp_c$vote19)) %>% as.data.frame() %>% rename(Party = Var1, `Prop C`=Freq)
true <- (preds_all/sum(preds_all)) %>% as.data.frame()
names(true) <- c("Sample")
true$Party <- rownames(true)
rownames(true) <- NULL

table <- left_join(left_join(prop_p, prop_c), true) %>% 
  mutate(across(where(is.numeric), ~round(.x, 2)))
table
saveRDS(table, "results/clogit_cv.rds")





# Simulation ----

plot(density(bes$redistSelf[!is.na(bes$redistSelf)]), xlim=c(-5,5))
plot(density(bes$euSelf[!is.na(bes$euSelf)]), xlim=c(-5,5)) #mostly -4 to 4, but really from -2 to 2

labsim <- seq(-2,2,0.01)
labsim <- labsim[labsim != 0]

same_side = function(x, y) {
  
  out <- case_when(x > 0 & y > 0 ~ 1,
                   x < 0 & y < 0 ~ 1,
                   x > 0 & y < 0 ~ 0,
                   x < 0 & y > 0 ~ 0,
                   T ~ NA_real_)
  
  return(out)
  
}


# 1. Replace labour EU distances and same sides based on new distance
# 2. Generate and store vote predictions based on the conditonal logit model results

simProbs_p <- simProbs_c <- array(numeric(nrow(data)*length(unique(data$vote19))*length(labsim)),
                                  dim=c(nrow(data),
                                        length(unique(data$vote19)),
                                        length(labsim)))
for (i in 1:length(labsim)) {
  
  tempDist <- dist
  tempDist$eu[tempDist$Party == "Lab"] <- abs(tempDist$euSelf[tempDist$Party == "Lab"] - labsim[i])
  tempDist$euSameSide[tempDist$Party == "Lab"] <- same_side(tempDist$euSelf[tempDist$Party == "Lab"], labsim[i])
  tempDist$prob_p <- predict(result_p, tempDist, type="response")
  tempDist$prob_c <- predict(result_c, tempDist, type="response")
  
  tempdf_p <- tempDist %>%
    select(id, Party, prob_p) %>%
    pivot_wider(names_from = Party,
                values_from = prob_p)
  tempdf_c <- tempDist %>%
    select(id, Party, prob_c) %>%
    pivot_wider(names_from = Party,
                values_from = prob_c)
  
  simProbs_p[,,i] <- as.matrix(tempdf_p[,2:7])
  simProbs_c[,,i] <- as.matrix(tempdf_c[,2:7])
  
}

colnames(simProbs_p) <- names(tempdf_p[2:7])
rownames(simProbs_p) <- tempdf_p$id
colnames(simProbs_c) <- names(tempdf_c[2:7])
rownames(simProbs_c) <- tempdf_c$id

# saveRDS(data %>% select(id, vote19, vote17), "data/id.rds")
# saveRDS(simProbs_p, "data/predProbs_p.rds")
# saveRDS(simProbs_c, "data/predProbs_c.rds")



## Plot ----

# Proximity
sim_sample_p <- map_dfr(1:length(labsim), function (x) {
  means <- colMeans(simProbs_p[,,x])
  names(means) <- names(tempdf_p[,2:7])
  return(means)
})

ggplot() +
  geom_line(aes(y=Lab, x=labsim), sim_sample_p, color="red", size=1) +
  geom_line(aes(y=Con, x=labsim), sim_sample_p, color="blue", size=1) +
  geom_line(aes(y=LD, x=labsim), sim_sample_p, color="orange", size=1) +
  geom_line(aes(y=Brexit, x=labsim), sim_sample_p, color="cyan", size=1) +
  geom_line(aes(y=Green, x=labsim), sim_sample_p, color="chartreuse4", size=1) +
  geom_line(aes(y=Non, x=labsim), sim_sample_p, color="black", size=1) +
  geom_vline(xintercept=parties$eu[2], size=1) +
  geom_vline(xintercept=labsim[which.max(sim_sample_p$Lab)], size=1, linetype="dashed", color="#E4003B") +
  geom_vline(xintercept=labsim[which.max(sim_sample_p$Lab - sim_sample_p$Con)], size=1, linetype="twodash", color="#00AEEF") +
  scale_y_continuous(limits=c(0,1))


# Proximity + Categorisation
sim_sample_c <- map_dfr(1:length(labsim), function (x) {
  means <- colMeans(simProbs_c[,,x])
  names(means) <- names(tempdf_c[,2:7])
  return(means)
})

ggplot() +
  geom_line(aes(y=Lab, x=labsim), sim_sample_c, color="red", size=1) +
  geom_line(aes(y=Con, x=labsim), sim_sample_c, color="blue", size=1) +
  geom_line(aes(y=LD, x=labsim), sim_sample_c, color="orange", size=1) +
  geom_line(aes(y=Brexit, x=labsim), sim_sample_c, color="cyan", size=1) +
  geom_line(aes(y=Green, x=labsim), sim_sample_c, color="chartreuse4", size=1) +
  geom_line(aes(y=Non, x=labsim), sim_sample_c, color="black", size=1) +
  scale_y_continuous(limits=c(0,1))







# Redist Simulation ----


labsim <- seq(-2,2,0.01)
labsim <- labsim[labsim != 0]

same_side = function(x, y) {
  
  out <- case_when(x > 0 & y > 0 ~ 1,
                   x < 0 & y < 0 ~ 1,
                   x > 0 & y < 0 ~ 0,
                   x < 0 & y > 0 ~ 0,
                   T ~ NA_real_)
  
  return(out)
  
}


# 1. Replace labour EU distances and same sides based on new distance
# 2. Generate and store vote predictions based on the conditonal logit model results

simProbs_p <- simProbs_c <- array(numeric(nrow(data)*length(unique(data$vote19))*length(labsim)),
                                  dim=c(nrow(data),
                                        length(unique(data$vote19)),
                                        length(labsim)))
for (i in 1:length(labsim)) {
  
  tempDist <- dist
  tempDist$redist[tempDist$Party == "Lab"] <- abs(tempDist$redistSelf[tempDist$Party == "Lab"] - labsim[i])
  tempDist$redistSameSide[tempDist$Party == "Lab"] <- same_side(tempDist$redistSelf[tempDist$Party == "Lab"], labsim[i])
  tempDist$prob_p <- predict(result_p, tempDist, type="response")
  tempDist$prob_c <- predict(result_c, tempDist, type="response")
  
  tempdf_p <- tempDist %>%
    select(id, Party, prob_p) %>%
    pivot_wider(names_from = Party,
                values_from = prob_p)
  tempdf_c <- tempDist %>%
    select(id, Party, prob_c) %>%
    pivot_wider(names_from = Party,
                values_from = prob_c)
  
  simProbs_p[,,i] <- as.matrix(tempdf_p[,2:7])
  simProbs_c[,,i] <- as.matrix(tempdf_c[,2:7])
  
}

colnames(simProbs_p) <- names(tempdf_p[2:7])
rownames(simProbs_p) <- tempdf_p$id
colnames(simProbs_c) <- names(tempdf_c[2:7])
rownames(simProbs_c) <- tempdf_c$id


# Proximity
sim_sample_p <- map_dfr(1:length(labsim), function (x) {
  means <- colMeans(simProbs_p[,,x])
  # names(means) <- names(tempdf_p[,2:7])
  return(means)
})

phue <- c("#00AEEF", "#E4003B", "#FBB347", "#6AB023", "purple")

sim_p <- ggplot() +
  geom_line(aes(y=Lab, x=labsim), sim_sample_p, color=phue[2], size=1) +
  geom_line(aes(y=Con, x=labsim), sim_sample_p, color=phue[1], size=1) +
  geom_line(aes(y=LD, x=labsim), sim_sample_p, color=phue[3], size=1) +
  geom_line(aes(y=Brexit, x=labsim), sim_sample_p, color=phue[4], size=1) +
  geom_line(aes(y=Green, x=labsim), sim_sample_p, color=phue[5], size=1) +
  geom_line(aes(y=Non, x=labsim), sim_sample_p, color="black", size=1) +
  geom_vline(xintercept=parties$redist[2], size=1) +
  geom_vline(xintercept=labsim[which.max(sim_sample_p$Lab)], size=1, linetype="dashed", color="#E4003B") +
  geom_vline(xintercept=labsim[which.max(sim_sample_p$Lab - sim_sample_p$Con)], size=1, linetype="twodash", color="#00AEEF") +
  scale_y_continuous(limits=c(0,1)) +
  labs(title="Proximity Simulation", y="Proportion", x="Simulated Labour Redistribution Position") +
  theme_classic() +
  theme(aspect.ratio=1,
        plot.title=element_text(hjust=0.5))

# Proximity + Categorisation
sim_sample_c <- map_dfr(1:length(labsim), function (x) {
  means <- colMeans(simProbs_c[,,x])
  # names(means) <- names(tempdf_c[,2:7])
  return(means)
})

sim_c <- ggplot() +
  geom_line(aes(y=Lab, x=labsim), sim_sample_c, color=phue[2], size=1) +
  geom_line(aes(y=Con, x=labsim), sim_sample_c, color=phue[1], size=1) +
  geom_line(aes(y=LD, x=labsim), sim_sample_c, color=phue[3], size=1) +
  geom_line(aes(y=Brexit, x=labsim), sim_sample_c, color=phue[4], size=1) +
  geom_line(aes(y=Green, x=labsim), sim_sample_c, color=phue[5], size=1) +
  geom_line(aes(y=Non, x=labsim), sim_sample_c, color="black", size=1) +
  geom_vline(xintercept=parties$redist[2], size=1) +
  geom_vline(xintercept=labsim[which.max(sim_sample_c$Lab)], size=1, linetype="dashed", color="#E4003B") +
  geom_vline(xintercept=labsim[which.max(sim_sample_c$Lab - sim_sample_c$Con)], size=1, linetype="twodash", color="#00AEEF") +
  scale_y_continuous(limits=c(0,1)) +
  labs(title="Proximity + Categorisation Simulation", y="Proportion", x="Simulated Labour Redistribution Position") +
  theme_classic() +
  theme(aspect.ratio=1,
        plot.title=element_text(hjust=0.5))


ggpubr::ggarrange(sim_p, sim_c, ncol=2)
ggsave("temp/redistSim.pdf")




# Redist sim w/ new LD position ----

labsim <- seq(-2,2,0.01)
labsim <- labsim[labsim != 0]
ldsim <- parties[3,2]

same_side = function(x, y) {
  
  out <- case_when(x > 0 & y > 0 ~ 1,
                   x < 0 & y < 0 ~ 1,
                   x > 0 & y < 0 ~ 0,
                   x < 0 & y > 0 ~ 0,
                   T ~ NA_real_)
  
  return(out)
  
}


# 1. Replace labour EU distances and same sides based on new distance
# 2. Generate and store vote predictions based on the conditonal logit model results

simProbs_p <- simProbs_c <- array(numeric(nrow(data)*length(unique(data$vote19))*length(labsim)),
                                  dim=c(nrow(data),
                                        length(unique(data$vote19)),
                                        length(labsim)))
for (i in 1:length(labsim)) {
  
  tempDist <- dist
  tempDist$redist[tempDist$Party == "LD"] <- abs(tempDist$redistSelf[tempDist$Party == "LD"] - ldsim) #no need to recalculate same side while only one sim value
  tempDist$redist[tempDist$Party == "Lab"] <- abs(tempDist$redistSelf[tempDist$Party == "Lab"] - labsim[i])
  tempDist$redistSameSide[tempDist$Party == "Lab"] <- same_side(tempDist$redistSelf[tempDist$Party == "Lab"], labsim[i])
  tempDist$prob_p <- predict(result_p, tempDist, type="response")
  tempDist$prob_c <- predict(result_c, tempDist, type="response")
  
  tempdf_p <- tempDist %>%
    select(id, Party, prob_p) %>%
    pivot_wider(names_from = Party,
                values_from = prob_p)
  tempdf_c <- tempDist %>%
    select(id, Party, prob_c) %>%
    pivot_wider(names_from = Party,
                values_from = prob_c)
  
  simProbs_p[,,i] <- as.matrix(tempdf_p[,2:7])
  simProbs_c[,,i] <- as.matrix(tempdf_c[,2:7])
  
}

colnames(simProbs_p) <- names(tempdf_p[2:7])
rownames(simProbs_p) <- tempdf_p$id
colnames(simProbs_c) <- names(tempdf_c[2:7])
rownames(simProbs_c) <- tempdf_c$id


# Proximity
sim_sample_p <- map_dfr(1:length(labsim), function (x) {
  means <- colMeans(simProbs_p[,,x])
  # names(means) <- names(tempdf_p[,2:7])
  return(means)
})

phue <- c("#00AEEF", "#E4003B", "#FBB347", "#6AB023", "purple")

sim_p <- ggplot() +
  geom_line(aes(y=Lab, x=labsim), sim_sample_p, color=phue[2], size=1) +
  geom_line(aes(y=Con, x=labsim), sim_sample_p, color=phue[1], size=1) +
  geom_line(aes(y=LD, x=labsim), sim_sample_p, color=phue[3], size=1) +
  geom_line(aes(y=Brexit, x=labsim), sim_sample_p, color=phue[4], size=1) +
  geom_line(aes(y=Green, x=labsim), sim_sample_p, color=phue[5], size=1) +
  geom_line(aes(y=Non, x=labsim), sim_sample_p, color="black", size=1) +
  geom_vline(xintercept=parties$redist[2], size=1) +
  geom_vline(xintercept=labsim[which.max(sim_sample_p$Lab)], size=1, linetype="dashed", color="#E4003B") +
  geom_vline(xintercept=labsim[which.max(sim_sample_p$Lab - sim_sample_p$Con)], size=1, linetype="twodash", color="#00AEEF") +
  scale_y_continuous(limits=c(0,1)) +
  labs(title="Proximity Simulation", y="Proportion", x="Simulated Labour Redistribution Position") +
  theme_classic() +
  theme(aspect.ratio=1,
        plot.title=element_text(hjust=0.5))

# Proximity + Categorisation
sim_sample_c <- map_dfr(1:length(labsim), function (x) {
  means <- colMeans(simProbs_c[,,x])
  # names(means) <- names(tempdf_c[,2:7])
  return(means)
})

sim_c <- ggplot() +
  geom_line(aes(y=Lab, x=labsim), sim_sample_c, color=phue[2], size=1) +
  geom_line(aes(y=Con, x=labsim), sim_sample_c, color=phue[1], size=1) +
  geom_line(aes(y=LD, x=labsim), sim_sample_c, color=phue[3], size=1) +
  geom_line(aes(y=Brexit, x=labsim), sim_sample_c, color=phue[4], size=1) +
  geom_line(aes(y=Green, x=labsim), sim_sample_c, color=phue[5], size=1) +
  geom_line(aes(y=Non, x=labsim), sim_sample_c, color="black", size=1) +
  geom_vline(xintercept=parties$redist[2], size=1) +
  geom_vline(xintercept=labsim[which.max(sim_sample_c$Lab)], size=1, linetype="dashed", color="#E4003B") +
  geom_vline(xintercept=labsim[which.max(sim_sample_c$Lab - sim_sample_c$Con)], size=1, linetype="twodash", color="#00AEEF") +
  scale_y_continuous(limits=c(0,1)) +
  labs(title="Proximity + Categorisation Simulation", y="Proportion", x="Simulated Labour Redistribution Position") +
  theme_classic() +
  theme(aspect.ratio=1,
        plot.title=element_text(hjust=0.5))


ggpubr::ggarrange(sim_p, sim_c, ncol=2)
ggsave("temp/redistSimLD.pdf")



