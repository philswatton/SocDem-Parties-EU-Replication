# Lab GE 2019 ----
# Phil Swatton
# University of Essex
# File 01: Extracting and cleaning BES for scaling and clogit


## Packages
library(tidyverse)
library(haven)



# Read in ----
bes17 <- read_dta("data/BES2019_W17_v1.0-2.dta")



# Selecting variables ----
bes <- bes17 %>% 
  filter(country == 1) %>%
  select(p_past_vote_2017, p_turnout_2017,
         p_past_vote_2019, p_turnout_2019,
         
         # Party ID
         partyId, partyIdSqueeze,
         
         ## Ideology
         leftRight,
         matches("lr[[:alpha:]]+", F),
         matches("redist[[:alpha:]]+"),
         matches("EUIntegration[[:alpha:]]+", F),
         
         ## Like scores
         matches("like[[:alpha:]]+"),
         
         ## Win probability
         matches("winConstituency[[:alpha:]]+"),
         
         ## Admin
         id, wt) %>% #left_join(besip) %>%
  select(-EUIntegrationCorbyn, -EUIntegrationJohnson,
         -winConstituencyUKIP, -likeUKIP) %>%
  select(-matches("SNP"), -matches("PC"),
         -likeSturgeon, -likePrice, -winConstituencyInd)


# Recoding and Renaming ----

## Functions ----

### Function 
removeDK <- function(input, remove=9999) {
  
  input <- as.numeric(input)
  
  output <- case_when(input %in% remove ~ NA_real_,
                      TRUE ~ input)
  
  return(output)
  
}


## Recodes ----

unscaled <- bes %>%
  rename(lrSelf = leftRight,
         lrGreen = lrgreens,
         likeGreen = likeGrn,
         likeBrexit = likeBrexitParty,
         leaderLikeCon = likeJohnson,
         leaderLikeLab = likeCorbyn,
         leaderLikeLD = likeSwinson,
         leaderLikeBrexit = likeFarage,
         leaderLikeGreen1 = likeBartley,
         leaderLikeGreen2 = likeBerry) %>%
  mutate(across(.fns=removeDK),
         vote19 = case_when(p_turnout_2019 == 0 | p_past_vote_2019 == 0  ~ "Non",
                            p_past_vote_2019 == 1 ~"Con",
                            p_past_vote_2019 == 2 ~"Lab",
                            p_past_vote_2019 == 3 ~"LD",
                            p_past_vote_2019 == 7 ~"Green",
                            p_past_vote_2019 == 12 ~"Brexit",
                            !is.na(p_past_vote_2019) ~"Other",
                            TRUE ~ NA_character_),
         vote17 = case_when(p_turnout_2017 == 0 | p_past_vote_2017 == 0  ~ "Non",
                            p_past_vote_2017 == 1 ~"Con",
                            p_past_vote_2017 == 2 ~"Lab",
                            p_past_vote_2017 == 3 ~"LD",
                            p_past_vote_2017 == 7 ~"Green",
                            p_past_vote_2017 == 6 ~"Brexit", #Treating UKIP 2017 as BXP
                            !is.na(p_past_vote_2017) ~"Other",
                            TRUE ~ NA_character_),
         partyID = case_when(partyId == 1 ~ "Con",
                             partyId == 2 ~ "Lab",
                             partyId == 3 ~ "LD",
                             partyId == 7 ~ "Green",
                             partyId == 12 ~ "Brexit",
                             partyId %in% c(6, 8, 9, 11) ~ "Other", #UKIP, BNP, Other, Change UK - nones and DKs get asked the squeeze q
                             partyIdSqueeze == 1 ~ "Con",
                             partyIdSqueeze == 2 ~ "Lab",
                             partyIdSqueeze == 3 ~ "LD",
                             partyIdSqueeze == 7 ~ "Green",
                             partyIdSqueeze == 12 ~ "Brexit",
                             partyIdSqueeze %in% c(6, 8, 9, 11) ~ "Other",
                             partyIdSqueeze == 10 ~ "Non",
                             TRUE ~ NA_character_)) %>%
  filter(!is.na(vote19) & vote19 != "Other") %>%
  select(-p_past_vote_2017, -p_turnout_2017,
         -p_past_vote_2019, -p_turnout_2019,
         -partyId, -partyIdSqueeze)




# Saving output ----

saveRDS(unscaled, "data/unscaled.rds")





