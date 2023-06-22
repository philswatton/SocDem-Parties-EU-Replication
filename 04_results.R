# Lab GE 2019 ----
# Phil Swatton
# University of Essex
# File 03: Clogit models


# Packages
library(tidyverse)
library(mlogit)
library(texreg)
library(xtable)
library(ggpubr)
library(ggrepel)
library(haven)
library(labelled)
library(survey)
library(srvyr)

# Simulated positions
source("labsim.R")

# Party Positions
parties <- readRDS("data/parties.rds")

# Constituency Data
const <- read_dta("data/BES-2019-General-Election-results-file-v1.1.dta")

# BES
bes <- read_dta("data/BES2019_W17_v1.0-2.dta")

# Results
load("results/results.Rdata")
load("results/results_sq.Rdata")
load("results/results_no_ctrl.Rdata")
load("results/results_left_shift.Rdata")
load("results/results_right_shift.Rdata")

# Functions
source("04b_output_functions.R")

# Folders
if (!dir.exists("tables")) dir.create("tables")
if (!dir.exists("plots")) dir.create("plots")



# BAM Results ----

# Data
am <- readRDS("data/scaled.rds")%>%
  rename(redistSelf=redist,
         euSelf=eu)
parties <- readRDS("data/parties.rds")

# NOTE - deliberately cutting out outliers with these plots
re_bam <- ggplot(am, aes(x=redistSelf)) +
  geom_vline(xintercept=parties$redist, color=phue, size=1.5, linetype=c(rep("solid",4), "dashed")) +
  geom_density(size=1) +
  scale_x_continuous(limits=c(-5,5), breaks=seq(-5,5,1)) +
  scale_y_continuous(limits=c(0,0.6), breaks=seq(0,0.6,0.1)) +
  labs(title="Redistribution", y="Density", x="Redistribution Ideal Point") +
  theme_classic() +
  theme(aspect.ratio=1,
        plot.title=element_text(hjust=0.5))

eu_bam <- ggplot(am, aes(x=euSelf)) +
  geom_vline(xintercept=parties$eu, color=phue, size=1.5) +
  geom_density(size=1) +
  scale_x_continuous(limits=c(-5,5), breaks=seq(-5,5,1)) +
  scale_y_continuous(limits=c(0,0.6), breaks=seq(0,0.6,0.1)) +
  labs(title="EU Integration", y="Density", x="EU Integration Ideal Point") +
  theme_classic() +
  theme(aspect.ratio=1,
        plot.title=element_text(hjust=0.5))

ggarrange(re_bam, eu_bam, ncol=2)
ggsave("plots/bam.pdf", width=6, height=3)



# Regression Tables ----

# Main results
write_model_tex(results$models[1:2])



# Coef Plots ----
make_coef_plot(results$models)



# Cross-Validation ----
write_cv_table(results, "cv")



# Vote Simulations ----
make_sim_plots(results$sims, file_name="sims.pdf", parties=parties)
make_sim_plots(results_sq$sims, file_name="sims_sq.pdf", parties=parties)
make_sim_plots(results_no_ctrl$sims, file_name="sims_no_ctrl.pdf", parties=parties)
make_sim_plots(results_left_shift$sims, file_name="sims_left_shift.pdf", parties=parties)
make_sim_plots(results_right_shift$sims, file_name="sims_right_shift.pdf", parties=parties)



# Seat Predictions (UNS/URS) ----

## Set up national and regional data ----

# Constituencies to calculate swing from
target <- const %>% 
  select(ConstituencyName, Region, ConVote17, LabVote17, LDVote17, GreenVote17, UKIPVote17, TotalVote17, Electorate17) %>%
  rename(constituency = ConstituencyName,
         region = Region,
         BrexitVote17 = UKIPVote17) %>%
  mutate(region = to_character(region),
         across(matches("Vote17"), ~case_when(is.na(.x) ~ 0,
                                              T~.x))) %>%
  filter(!region %in% c("Wales", "Scotland"))
names(target) <- names(target) %>% gsub("Vote", "", .) %>% gsub("17", "", .)

# Temp Df
temp <- target %>%
  mutate(Other = Total - Con - Lab - LD - Green - Brexit) %>%
  select(region, Con, Lab, LD, Green, Brexit, Other, Total, Electorate)

# National Vote
national <- temp %>%
  select(-region) %>%
  mutate(Non = Electorate - Total) %>%
  summarise(across(.fns=sum)) %>%
  select(-Total, -Electorate, -Non)
rsum_nat <- sum(national)
national <- national/rsum_nat

# Regional Vote
region <- temp %>%
  group_by(region) %>%
  mutate(Non = Electorate - Total) %>%
  summarise(across(.fns=sum)) %>%
  select(-Total, -Electorate, -Non)
rsum_region <- region %>% select(where(is.numeric)) %>% rowSums()
region <- region %>%
  mutate(across(where(is.numeric), .fns=~.x/rsum_region))

# Constituency Vote
const17 <- target %>%
  mutate(Other = Total - Con - Lab - LD - Green - Brexit,
         Con = Con/Total,
         Lab = Lab/Total,
         LD = LD/Total,
         Green = Green/Total,
         Brexit = Brexit/Total,
         Other = Other/Total)

# BES
id <- bes %>%
  select(id, wt, pcon) %>%
  rename(constituency = pcon) %>%
  mutate(constituency = to_character(constituency)) %>%
  filter(constituency != "NOT in a 2010 Parliamentary Constituency" & constituency != "Lagan Valley") %>%
  left_join(const17 %>% select(constituency, region)) %>%
  filter(complete.cases(.)) %>%
  select(-constituency)



## Compute UNS and URS for each model ----

# Main Results
make_uns_plots(results$sims, extra=NULL)
make_urs_plots(results$sims, extra=NULL)

# Squared Distance
make_uns_plots(results_sq$sims, extra="_sq")
make_urs_plots(results_sq$sims, extra="_sq")

# No Controls
make_uns_plots(results_no_ctrl$sims, extra="_no_ctrl")
make_urs_plots(results_no_ctrl$sims, extra="_no_ctrl")

# Left Shift
make_uns_plots(results_left_shift$sims, extra="_left_shift")
make_urs_plots(results_left_shift$sims, extra="_left_shift")

# Right Shift
make_uns_plots(results_right_shift$sims, extra="_right_shift")
make_urs_plots(results_right_shift$sims, extra="_right_shift")







