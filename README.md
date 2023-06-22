# Replication mateirals for "Social democratic party positions on the EU: The case of Brexit"

This repository contains replication materials and R code for my article published in Party Politics, *Social democratic party positions on the EU: The case of Brexit*. The DOI for the article is: <https://doi.org/10.1177/13540688231177557>
  
This README contains an explanation of the contents of this repository, and notes some files which are not included.

To the best of my ability, I have checked over the code and made sure it remains correct for current usage. This paper was done largely in three different time periods during my PhD, with the analysis being almost wholly re-written at one point. The one part that has not been rewritten was the script implementing Bayesian-Aldrich-McKelvey scaling, which is included here but is more likely to be outdated for any user of these files using more recent versions of JAGS/rjags.

All of the scripts - particularly the more recent ones - contain many comments which hopefully along with this README should be sufficient to guide a reader through the steps taken. If not, or if you have any issues in using these replication materials where I can be of help, please visit my website to find my latest contact details: <https://philswatton.github.io/>. Please note I may take some time to respond!
  
## Main Scripts

The main point of this repository is to offer the scripts used to prepare and analyse the data used in the paper. They are as follows:
  
1. **01_bes.R**: Selects the relevant variables of choice, filters, and cleans the BES data used in the paper. Outputs from this script are saved in the 'data' folder.
2. **02_scaling.R**: Applies Bayesian Aldrich-McKelvey scaling to the data, imputes values of '5' in likeability data, scales likeability data, and computes distances/same side variables. Outputs from this script are saved in the 'data' folder.
3. **03b_clogit.R**: Runs conditional logit models and simulations for the main analysis and the additional analyses in the appendix. Outputs from this script are saved in the 'results' and 'data' folders.
4. **04_results.R**: Runs functions that generate the plots and tables presented in the main analysis and the additional analyses in the appendix. Outputs from this script are saved in the 'plots' and 'tables' folders.

## Additional Scripts

1. **03b_clogit_functions.R**: Functions for the steps performed in **03b_clogit.R**
2. **04b_output_functions.R**: Functions for the steps performed in **04b_results.R**
3. **bam.jags**: JAGS code for the Bayesian-Aldrich-McKelvey scaling. Taken from the University of Essex version of the `asmcjr` R package. Link to the package: <https://github.com/uniofessex/asmcjr>
4. **labsim.R**: Variables used during the simulation and useful to refer back to later on.

## Folders

1. **data**: Contains data files generates in scripts 01 and 02. **unscaled.rds** contains BES data prior to scaling, while **scaled.rds** contains the BES data post-scaling. Finally, **parties.rds** contains the esimtated party positions from Aldrich-McKelvey scaling.
2. **plots**: All output plots. Plots beginning with **sims** are plots of simulations, with **uns** are UNS results, and with **urs** are URS results. Postfixes if present refer to which appendix result they belong to: **left_shift** and **right_shift** for shifts in the centre position, **_sq** for squared distance function results, and **_no_ctrl** for the results without controls. Finally, 
3. **results**: Due to the size of the objects created, this folder does not exist in the repository, but will be crated if you run script 3. The folder contains the results from script 03. Each file is a .Rdata file with postfixes in the manner above. Each result is a list with two elements. Replacing `results` with the appropriate R object name, the first is `results$models`, which contains both models `results$model$p_model` for the proximity-only model and the second is `results$model$c_model` for the proximity plus categorisation model. The third element is `results$model$test` which contains the test dataset. The second element of `results` is `results$sims`, with `results$sims$simProbs_p` and `results$sims$simProbs_c` containing the simulated vote choices from the proximity-only and proximity plus categorisation models respectively.
4. **tables**: This contains the latex tables generated in script 04. **reg.tex** contains the main regression results while **cv.tex** contains the cross-validation table.

## Not In This Repository

Users of this repository should be able to rune scripts 2 and 3 without making any further changes. Script 3 must be run before script 4 is run. The original BES data files used are not however included, and thus script 1 cannot be run without making further downloads. They can be easily downloaded however. The files used are:
  
1. BES Internet Panel Wave 17, version 1.0-2, STATA file
2. BES 2019 General Election Results File, version 1.1, STATA file

They can be downloaded for free from the BES website: <https://www.britishelectionstudy.com/>
  
These are used in script 1. Note that as versions etc change over time, you may need to change the lines in script 1 to reflect new file names.


