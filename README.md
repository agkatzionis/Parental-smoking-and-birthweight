# Parental-smoking-and-birthweight

This repository contains R code and results for the simulation study implemented in the paper "Challenges in using data on fathers/partners to study prenatal smoking and offspring health" (K. Easey et al. 2023, link to be added when the paper is publicly available).We ran a simulation study informed by the ALSPAC dataset to estimate the association between parental smoking and offspring birthweight, and to assess whether selection bias would affect association estimates due to missing data for some fathers/partners.

The file "Smoking and Birthweight Simulations.R" contains the code used to run the simulation and provides details on how the results files were created. The results files are organized in three sub-folders:
- "Main": results from our baseline simulation experiment, informed as closely as possible from the ALSPAC data.
- "Confounding": results from a simulation experiment where we have induced additional confounding of partner participation and offspring birthweight.
- "Interaction": results from a simulation experiment where we have added an interaction between partner smoking and partner BMI in their effects on participation.

The file "Smoking and Birthweight Simulations.R" also contains the code used to create Figure 5 and the Supplementary Tables included in the paper, where the simulation results are summarized.


