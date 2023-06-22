# Lab GE 2019 ----
# Phil Swatton
# University of Essex
# File 02: Scaling BES W18 Extract

 
## Packages
library(tidyverse)
library(basicspace)
library(rjags)
library(bayestestR)




# Data ----

# Read in data
bes <- readRDS("data/unscaled.rds")


# Prepare Subsets - Views
redistSub <- bes %>% select(id, matches("redist"))
euSub <- bes %>% select(id, matches("EUIntegration"))


# Function to filter missing data
missmat <- function(mat, max=2) { #max is maximum number of missing stimuli placements
  counts <- apply(mat[3:7], 1, function(x) sum(is.na(x)))
  return(counts <= max)
}


# Filter missing data
# lrMat <- lrSub[missmat(lrSub),]
redistMat <- redistSub[missmat(redistSub),]
euMat <- euSub[missmat(euSub),]


# Prepare Subsets - Affect towards leader and party
leaderMat <- bes %>% select(matches("Like", F))
partyMat <- bes %>% select(matches("like", F))




# BAM Scaling ----

# function
bamscale <- function(x, chains=2, iter=10000) {
  
  # Prepare data
  X <- x[3:7]
  N <- nrow(x)
  J <- ncol(X)
  
  # Run regular AM first
  am <- aldmck(X, polarity=3)
  
  out <- jags.model("bam.jags",
                    data = list('z' = X, 'q' = J, 'N' = N),
                    inits = list(zhatstar = am$stimuli + rnorm(length(am$stimuli), 0, 1)),
                    n.chains = chains,
                    n.adapt = iter)
  
  return(out)
  
}


## Scaling ----

# lrAM <- bamscale(lrMat)
redistAM <- bamscale(redistMat)
euAM <- bamscale(euMat)


## Extract estimates from posterior dist ----

bamresults <- function(bam, s=5000, thin=1) {
  
  stim <- coda.samples(bam, 'zhat', s, thin=thin) #estimated location of the stimuli
  a <- coda.samples(bam, 'a', s, thin=thin) #intercept for individuals
  b <- coda.samples(bam, 'b', s, thin=thin) #weight term for individuals
  
  return(list(stim=stim, a=a, b=b))
  
}

redistOut <- bamresults(redistAM)
euOut <- bamresults(euAM)

saveRDS(redistOut, "results/redistAM.rds")
saveRDS(euOut, "results/euAM.rds")


## Party Locations ----

# Stimuli locations
parties <- data.frame(
  redist = summary(redistOut$stim)$statistics[,1],
  eu = summary(euOut$stim)$statistics[,1]
)
rownames(parties) <- c("Con", "Lab", "LD", "Green", "Brexit")


## Respondent Locations ----

# Y = a + bZ
# Z = (Y - a)/b

idealpt <- function(bamout, self) {
  
  asamp <- do.call(rbind, bamout$a) %>% t()
  bsamp <- do.call(rbind, bamout$b) %>% t()
  
  n <- length(self)
  s <- ncol(asamp)
  
  idealsamp <- matrix(numeric(n*s), nrow=n, ncol=s)
  
  for (i in 1:n) {
    for (j in 1:s) {
      idealsamp[i,j] <- (self[i] - asamp[i,j])/bsamp[i,j]
    }
  }
  
  return(idealsamp)
  
}


euIdeal <- apply(idealpt(euOut, euMat$EUIntegrationSelf), 1, median)
gc()
redistIdeal <- apply(idealpt(redistOut, redistMat$redistSelf), 1, median)
gc()



# Data frame ----
resp1 <- left_join(bes %>% select(id, vote19, vote17, partyID, starts_with("winConstituency")),
                   data.frame(
                     id = redistMat$id,
                     redist = redistIdeal
                   )) %>%
  left_join(data.frame(
    id = euMat$id,
    eu = euIdeal
  ))




# Affect ----

# Function to impute missing responses as 5 because missingness is intolerable
impute5 <- function(x) {
  
  out <- case_when(is.na(x) ~ 5,
                   TRUE~ x)
  return(out)
  
}


# Impute the 5s
partyMat2 <- partyMat %>% mutate(across(.fns=impute5))
leaderMat2 <- leaderMat %>% mutate(across(.fns=impute5))


# Run regressions
partyReg <- map(partyMat2, ~lm(.x ~ resp1$redist + resp1$eu, na.action=na.exclude))
leaderReg <- map(leaderMat2, ~lm(.x ~ resp1$redist + resp1$eu, na.action=na.exclude))

# look at results out of curiosity
texreg::screenreg(partyReg)
texreg::screenreg(leaderReg)

# Predict outputs
partyResids <- map_dfc(partyReg, residuals)
leaderResids <- map_dfc(leaderReg, residuals) %>%
  mutate(leaderLikeGreen = (leaderLikeGreen1 + leaderLikeGreen2)/2) %>%
  select(-leaderLikeGreen1, -leaderLikeGreen2)




# Final dataframe ----

same_side = function(x, y) {
  
  out <- case_when(x > 0 & y > 0 ~ 1,
                   x < 0 & y < 0 ~ 1,
                   x > 0 & y < 0 ~ 0,
                   x < 0 & y > 0 ~ 0,
                   T ~ NA_real_)
  
  return(out)
  
}

## Create frame
resp2 <- cbind(resp1, partyResids, leaderResids) %>%
  mutate(redistDistCon = abs(redist - parties$redist[rownames(parties) == "Con"]),
         redistDistLab = abs(redist - parties$redist[rownames(parties) == "Lab"]),
         redistDistLD = abs(redist - parties$redist[rownames(parties) == "LD"]),
         redistDistGreen = abs(redist - parties$redist[rownames(parties) == "Green"]),
         redistDistBrexit = abs(redist - parties$redist[rownames(parties) == "Brexit"]),
         
         euDistCon = abs(eu - parties$eu[rownames(parties) == "Con"]),
         euDistLab = abs(eu - parties$eu[rownames(parties) == "Lab"]),
         euDistLD = abs(eu - parties$eu[rownames(parties) == "LD"]),
         euDistGreen = abs(eu - parties$eu[rownames(parties) == "Green"]),
         euDistBrexit = abs(eu - parties$eu[rownames(parties) == "Brexit"]),
         
         redistSameSideCon = same_side(redist, parties$redist[rownames(parties) == "Con"]),
         redistSameSideLab = same_side(redist, parties$redist[rownames(parties) == "Lab"]),
         redistSameSideLD = same_side(redist, parties$redist[rownames(parties) == "LD"]),
         redistSameSideGreen = same_side(redist, parties$redist[rownames(parties) == "Green"]),
         redistSameSideBrexit = same_side(redist, parties$redist[rownames(parties) == "Brexit"]),
         
         euSameSideCon = same_side(eu, parties$eu[rownames(parties) == "Con"]),
         euSameSideLab = same_side(eu, parties$eu[rownames(parties) == "Lab"]),
         euSameSideLD = same_side(eu, parties$eu[rownames(parties) == "LD"]),
         euSameSideGreen = same_side(eu, parties$eu[rownames(parties) == "Green"]),
         euSameSideBrexit = same_side(eu, parties$eu[rownames(parties) == "Brexit"]))


## Save
saveRDS(resp2, "data/scaled.rds")
saveRDS(parties, "data/parties.rds")




