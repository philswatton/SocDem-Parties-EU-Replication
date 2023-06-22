# Lab GE 2019 ----
# Phil Swatton
# University of Essex
# File 03: Clogit models


# Packages
library(tidyverse)
library(mlogit)


## Data
bes <- readRDS("data/scaled.rds")%>%
  rename(redistSelf=redist,
         euSelf=eu) #move these 2 lines into file 02 at some point
parties <- readRDS("data/parties.rds")




# Prepare Data ----
source("03b_clogit_functions.R")
dist <- bes_to_long(bes, square=F)
n_obs <- length(unique(dist$id))


# Run Models ----

# Controls
controls <- c("winProb", "like", "leaderLike", "lagChosen", "chosenID")

# Simulated positions
source("labsim.R")

# Folder
if (!dir.exists("results")) dir.create("results")

# Main models
set.seed(42)
models <- estimate_models(dist, controls)
sims <- simulate_votes(n_obs, dist, models, labsim)
results <- list(models=models, sims=sims)
save(results, file="results/results.Rdata")

# Squared models
set.seed(42)
models_sq <- estimate_models(bes_to_long(bes, square=T), controls)
sims_sq <- simulate_votes(n_obs, dist, models_sq, labsim)
results_sq <- list(models=models_sq, sims=sims_sq)
save(results_sq, file="results/results_sq.Rdata")

# Model w/out controls
set.seed(42)
models_no_ctrl <- estimate_models(dist, NULL)
sims_no_ctrl <- simulate_votes(n_obs, dist, models_no_ctrl, labsim)
results_no_ctrl <- list(models=models_no_ctrl, sims=sims_no_ctrl)
save(results_no_ctrl, file="results/results_no_ctrl.Rdata")

# Models shifting center to left
set.seed(42)
left_shift <- -0.1
models_left_shift <- estimate_models(bes_to_long(bes, square=F, left_shift), controls)
sims_left_shift <- simulate_votes(n_obs, dist, models_left_shift, labsim, left_shift)
results_left_shift <- list(models=models_left_shift, sims=sims_left_shift)
save(results_left_shift, file="results/results_left_shift.Rdata")

# Models shifting center to right
set.seed(42)
right_shift <- 0.1
models_right_shift <- estimate_models(bes_to_long(bes, square=F, right_shift), controls)
sims_right_shift <- simulate_votes(n_obs, dist, models_right_shift, labsim, right_shift)
results_right_shift <- list(models=models_right_shift, sims=sims_right_shift)
save(results_right_shift, file="results/results_right_shift.Rdata")




















