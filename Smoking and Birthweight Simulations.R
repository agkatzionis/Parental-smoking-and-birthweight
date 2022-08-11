
##########   SELECTION BIAS IN BIRTHWEIGHT STUDIES   ##########

## This file contains simulations to study how missing paternal
## data in ALSPAC may impact analyses of the effect of parental 
## smoking on offspring birthweight, in support of the paper

## "Challenges in using data on fathers/partners to study prenatal
## exposures and offspring health" (K. Easey et al. 2022).



## Set working directory.
setwd("My Working Directory")

## Auxiliary functions.
logit <- function (x) log(x / (1 - x))
expit <- function (x) exp(x) / (1 + exp(x))

## For illustration.
options(digits = 4)

## We run two variants of the simulation: The "Main Simulation"
## follows the causal diagram of Figure 2 in our paper, while a 
## "Confounded simulation" adds a confounder of paternal 
## participation and birthweight to induce stronger bias.



## For each of the two simulations we consider a range of selection 
## effects, described in our manuscript. We also fit a variety of
## different models for birthweight, modelling it either separately
## for fathers and mothers or jointly. Each model is fitted both 
## to all families and to families with participating partners only, 
## for comparison. The full list of fitted models is as follows:

## p.model.1: Birthweight ~ paternal smoking
## p.model.2: Birthweight ~ paternal smoking + paternal BMI
## p.model.3: Birthweight ~ paternal smoking + paternal SEP + maternal SEP
## p.model.4: Birthweight ~ paternal smoking + paternal BMI + paternal SEP + maternal SEP
## p.model.5-8: Same as p.model.1-4 but only for families with participating partners.
## m.model.1-8 same as p.model.1-8 but for maternal smoking instead of paternal.
## j.model.1-8 same as p.model.1-8 but using both paternal and maternal smoking (and BMI) as covariates.

## Not all of these models ended up being included in our manuscript.
## The most interesting ones are j.model.5-8 as they represent the
## analysis an applied researcher would likely perform.

##################################################

##########   MAIN SIMULATION   ##########

## This simulation follows ALSPAC as closely as possible.

## Seed for the simulations.
seed <- 5482599

## Number of iterations.
iter <- 1000

## Sample size.
N <- 15000
## ALSPAC has about 15000 participating families.

## Generate selection effects (Odds Ratios) for fathers' participation.
pat_bmi_list <- c(exp(0.00), 1.5, 2, 5, 10, 20, 1.5, 2, 5, 10, 20)
pat_sep_list <- c(exp(0.46), 1.5, 2, 5, 10, 20, 1.5, 2, 5, 10, 20)
pat_smk_list <- c(exp(-0.81), 1, 1, 1, 1, 1, 2/3, 1/2, 1/5, 1/10, 1/20)
mat_sep_or <- exp(0.324)   ## Fixed to observed value.

## Generate selection intercepts so that 76.7% are selected.
sel_int_list <- c(0.75, 0.38, 0.16, -0.38, -0.70, -0.93, 0.60, 0.53, 0.56, 0.67, 0.83)

## Start looping over selection effects.
for (J in 1:11) {
  
  ## Track progress.
  print(paste("Parameter Setting ", J, " Initiated", sep = ""))
  
  ## Specify the selection effects.
  pat_bmi_or <- pat_bmi_list[J]
  pat_sep_or <- pat_sep_list[J]
  pat_smk_or <- pat_smk_list[J]
  selection.intercept <- sel_int_list[J]
  
  ## Store simulation results for paternal effects here.
  p.model1 <- matrix(0, iter, 2)
  p.model2 <- matrix(0, iter, 4)
  p.model3 <- matrix(0, iter, 6)
  p.model4 <- matrix(0, iter, 8)
  colnames(p.model1) <- c("Estimate", "Std Err")
  colnames(p.model2) <- c("Estimate", "Std Err", "Bmi Est", "Bmi Se")
  colnames(p.model3) <- c("Estimate", "Std Err", "Sep-M Est", "Sep-M Se", "Sep-P Est", "Sep-P Se")
  colnames(p.model4) <- c("Estimate", "Std Err", "Bmi Est", "Bmi Se", "Sep-M Est", "Sep-M Se", "Sep-P Est", "Sep-P Se")
  p.model5 <- matrix(0, iter, 2)
  p.model6 <- matrix(0, iter, 4)
  p.model7 <- matrix(0, iter, 6)
  p.model8 <- matrix(0, iter, 8)
  colnames(p.model5) <- c("Estimate", "Std Err")
  colnames(p.model6) <- c("Estimate", "Std Err", "Bmi Est", "Bmi Se")
  colnames(p.model7) <- c("Estimate", "Std Err", "Sep-M Est", "Sep-M Se", "Sep-P Est", "Sep-P Se")
  colnames(p.model8) <- c("Estimate", "Std Err", "Bmi Est", "Bmi Se", "Sep-M Est", "Sep-M Se", "Sep-P Est", "Sep-P Se")
  
  ## Store simulation results for maternal effects here.
  m.model1 <- matrix(0, iter, 2)
  m.model2 <- matrix(0, iter, 4)
  m.model3 <- matrix(0, iter, 6)
  m.model4 <- matrix(0, iter, 8)
  colnames(m.model1) <- c("Estimate", "Std Err")
  colnames(m.model2) <- c("Estimate", "Std Err", "Bmi Est", "Bmi Se")
  colnames(m.model3) <- c("Estimate", "Std Err", "Sep-M Est", "Sep-M Se", "Sep-P Est", "Sep-P Se")
  colnames(m.model4) <- c("Estimate", "Std Err", "Bmi Est", "Bmi Se", "Sep-M Est", "Sep-M Se", "Sep-P Est", "Sep-P Se")
  m.model5 <- matrix(0, iter, 2)
  m.model6 <- matrix(0, iter, 4)
  m.model7 <- matrix(0, iter, 6)
  m.model8 <- matrix(0, iter, 8)
  colnames(m.model5) <- c("Estimate", "Std Err")
  colnames(m.model6) <- c("Estimate", "Std Err", "Bmi Est", "Bmi Se")
  colnames(m.model7) <- c("Estimate", "Std Err", "Sep-M Est", "Sep-M Se", "Sep-P Est", "Sep-P Se")
  colnames(m.model8) <- c("Estimate", "Std Err", "Bmi Est", "Bmi Se", "Sep-M Est", "Sep-M Se", "Sep-P Est", "Sep-P Se")
  
  ## Store simulation results for joint effects here.
  j.model1 <- matrix(0, iter, 4)
  j.model2 <- matrix(0, iter, 8)
  j.model3 <- matrix(0, iter, 8)
  j.model4 <- matrix(0, iter, 12)
  colnames(j.model1) <- c("M Estimate", "M Std Err", "P Estimate", "P Std Err")
  colnames(j.model2) <- c("M Estimate", "M Std Err", "P Estimate", "P Std Err", "Bmi-M Est", "Bmi-M Se", "Bmi-P Est", "Bmi-P Se")
  colnames(j.model3) <- c("M Estimate", "M Std Err", "P Estimate", "P Std Err", "Sep-M Est", "Sep-M Se", "Sep-P Est", "Sep-P Se")
  colnames(j.model4) <- c("M Estimate", "M Std Err", "P Estimate", "P Std Err", "Bmi-M Est", "Bmi-M Se", "Bmi-P Est", "Bmi-P Se", "Sep-M Est", "Sep-M Se", "Sep-P Est", "Sep-P Se")
  j.model5 <- matrix(0, iter, 4)
  j.model6 <- matrix(0, iter, 8)
  j.model7 <- matrix(0, iter, 8)
  j.model8 <- matrix(0, iter, 12)
  colnames(j.model5) <- c("M Estimate", "M Std Err", "P Estimate", "P Std Err")
  colnames(j.model6) <- c("M Estimate", "M Std Err", "P Estimate", "P Std Err", "Bmi-M Est", "Bmi-M Se", "Bmi-P Est", "Bmi-P Se")
  colnames(j.model7) <- c("M Estimate", "M Std Err", "P Estimate", "P Std Err", "Sep-M Est", "Sep-M Se", "Sep-P Est", "Sep-P Se")
  colnames(j.model8) <- c("M Estimate", "M Std Err", "P Estimate", "P Std Err", "Bmi-M Est", "Bmi-M Se", "Bmi-P Est", "Bmi-P Se", "Sep-M Est", "Sep-M Se", "Sep-P Est", "Sep-P Se")
  
  ## Store prevalence of smoking and proportion of missingness.
  missing.prop <- rep(0, iter)
  smoking.prop <- matrix(0, iter, 2)
  colnames(smoking.prop) <- c("Maternal", "Paternal")
  
  ## Store pairwise trait correlations.
  corrs <- matrix(0, 7, 7)
  colnames(corrs) <- c("Smoking P", "Smoking M", "SEP P", "SEP M", "BMI P", "BMI M", "Birthweight")
  rownames(corrs) <- c("Smoking P", "Smoking M", "SEP P", "SEP M", "BMI P", "BMI M", "Birthweight")
  
  ## Store Z-statistic moments.
  z.stats <- matrix(0, iter, 6)
  colnames(z.stats) <- c("pBMI Mean", "pBMI Std", "mBMI Mean", "mBMI Std", "BW Mean", "BW Std")
  
  ## Start the iteration loop.
  for (I in 1:iter) {
    
    ## Seed it.
    set.seed (seed + 10000 * J + I)
    
    ## Generate a confounding effect for maternal and paternal SEP.
    U.sep <- rnorm(N, 0, 1)
    
    ## Generate paternal and maternal SEP.
    ## These are correlated with a correlation larger than 0.575.
    ## The correlation is tuned to get 0.575 for the 0/1/2/3 counts.
    sep.cont <- matrix(0, N, 2)
    sep.cont[, 1] <- sqrt(0.654) * U.sep + sqrt(1 - 0.654) * rnorm(N, 0, 1)
    sep.cont[, 2] <- sqrt(0.654) * U.sep + sqrt(1 - 0.654) * rnorm(N, 0, 1)
    
    ## Compute thresholds in order to convert continuous values to 0/1/2/3.
    ## Proportions in ALSPAC were 0 = 20.2%, 1 = 44.5%, 2 = 22.4%, 3 = 12.9%.
    sep.thresholds <- qnorm(c(0.202, 0.647, 0.871))
    
    ## Convert.
    sep <- matrix(0, N, 2)
    sep[which(sep.cont[, 1] > sep.thresholds[1]), 1] <- 1
    sep[which(sep.cont[, 1] > sep.thresholds[2]), 1] <- 2
    sep[which(sep.cont[, 1] > sep.thresholds[3]), 1] <- 3
    sep[which(sep.cont[, 2] > sep.thresholds[1]), 2] <- 1
    sep[which(sep.cont[, 2] > sep.thresholds[2]), 2] <- 2
    sep[which(sep.cont[, 2] > sep.thresholds[3]), 2] <- 3
    colnames(sep) <- c("Maternal", "Paternal")
    
    ## Generate a confounding effect for maternal and paternal smoking.
    U.smk <- rnorm(N, 0, 1)
    
    ## Generate paternal smoking status (binary - logistic).
    pat_smoking_probs <- expit( - 0.608 * sep[, 2] + 2.65 * U.smk + log(1.92))
    pat_smoking_status <- rbinom(N, 1, pat_smoking_probs)
    smoking.prop[I, 2] <- sum(pat_smoking_status) / N
    
    ## Generate maternal smoking status, likewise.
    mat_smoking_probs <- expit( - 0.692 * sep[, 1] + 2.65 * U.smk + log(0.46))
    mat_smoking_status <- rbinom(N, 1, mat_smoking_probs)
    smoking.prop[I, 1] <- sum(mat_smoking_status) / N
    
    ## Generate a confounding effect for maternal and paternal BMI.
    U.bmi <- rnorm(N, 0, 1)
    
    ## Generate paternal BMI (Z-scores).
    pat_bmi <- rnorm(N, 0.165, 0.918) - 0.105 * pat_smoking_status - 0.088 * sep[, 2] + 0.4 * U.bmi
    ## Confounding coefficients selected to adjust parental 
    ## BMI correlations, error coefficients to standardize.
    
    ## Generate maternal BMI (Z-scores).
    mat_bmi <- rnorm(N, 0.201, 0.911) - 0.116 * mat_smoking_status - 0.129 * sep[, 1] + 0.4 * U.bmi
    
    ## Generate offspring birthweight.
    birthweight <- rnorm(N, 0.168, 0.972) + 0.055 * pat_bmi + 0.141 * mat_bmi - 0.155 * pat_smoking_status - 0.309 * mat_smoking_status
    
    ## Generate paternal selection indicator.
    pat_selection_probs <- expit(log(mat_sep_or) * sep[, 1] + log(pat_sep_or) * sep[, 2] + log(pat_bmi_or) * pat_bmi + log(pat_smk_or) * pat_smoking_status + selection.intercept)
    pat_r <- rbinom(N, 1, pat_selection_probs)
    missing.prop[I] <- 1 - sum(pat_r) / N
    
    ## Store Z-statistic means and variances as a diagnostic.
    z.stats[I, ] <- c(mean(pat_bmi), sd(pat_bmi), mean(mat_bmi), sd(mat_bmi), mean(birthweight), sd(birthweight))
    
    
    ##   PATERNAL EFFECTS   ##
    
    ## Observational effect in the whole sample.
    fit1 <- summary(lm(birthweight ~ pat_smoking_status))
    p.model1[I, ] <- c(fit1$coef[2, 1], fit1$coef[2, 2]) 
    ## Effect adjusting for paternal BMI.
    fit2 <- summary(lm(birthweight ~ pat_smoking_status + pat_bmi))
    p.model2[I, ] <- c(fit2$coef[2, 1], fit2$coef[2, 2], fit2$coef[3, 1], fit2$coef[3, 2]) 
    ## Effect adjusting for socioeconomic status.
    fit3 <- summary(lm(birthweight ~ pat_smoking_status + sep))
    p.model3[I, ] <- c(fit3$coef[2, 1], fit3$coef[2, 2], fit3$coef[3, 1], fit3$coef[3, 2], fit3$coef[4, 1], fit3$coef[4, 2]) 
    ## Effect adjusting for paternal BMI and socioeconomic status.
    fit4 <- summary(lm(birthweight ~ pat_smoking_status + pat_bmi + sep))
    p.model4[I, ] <- c(fit4$coef[2, 1], fit4$coef[2, 2], fit4$coef[3, 1], fit4$coef[3, 2], fit4$coef[4, 1], fit4$coef[4, 2], fit4$coef[5, 1], fit4$coef[5, 2]) 
    
    ## Observational effect in families with participating fathers.
    fit5 <- summary(lm(birthweight[pat_r == 1] ~ pat_smoking_status[pat_r == 1]))
    p.model5[I, ] <- c(fit5$coef[2, 1], fit5$coef[2, 2]) 
    ## Effect adjusting for paternal BMI.
    fit6 <- summary(lm(birthweight[pat_r == 1] ~ pat_smoking_status[pat_r == 1] + pat_bmi[pat_r == 1]))
    p.model6[I, ] <- c(fit6$coef[2, 1], fit6$coef[2, 2], fit6$coef[3, 1], fit6$coef[3, 2]) 
    ## Effect adjusting for socioeconomic status.
    fit7 <- summary(lm(birthweight[pat_r == 1] ~ pat_smoking_status[pat_r == 1] + sep[pat_r == 1, ]))
    p.model7[I, ] <- c(fit7$coef[2, 1], fit7$coef[2, 2], fit7$coef[3, 1], fit7$coef[3, 2], fit7$coef[4, 1], fit7$coef[4, 2]) 
    ## Effect adjusting for paternal BMI and socioeconomic status.
    fit8 <- summary(lm(birthweight[pat_r == 1] ~ pat_smoking_status[pat_r == 1] + pat_bmi[pat_r == 1] + sep[pat_r == 1, ]))
    p.model8[I, ] <- c(fit8$coef[2, 1], fit8$coef[2, 2], fit8$coef[3, 1], fit8$coef[3, 2], fit8$coef[4, 1], fit8$coef[4, 2], fit8$coef[5, 1], fit8$coef[5, 2]) 
    
    
    ##   MATERNAL EFFECTS   ##
    
    ## Observational effect in the whole sample.
    fit1 <- summary(lm(birthweight ~ mat_smoking_status))
    m.model1[I, ] <- c(fit1$coef[2, 1], fit1$coef[2, 2]) 
    ## Effect adjusting for maternal BMI.
    fit2 <- summary(lm(birthweight ~ mat_smoking_status + mat_bmi))
    m.model2[I, ] <- c(fit2$coef[2, 1], fit2$coef[2, 2], fit2$coef[3, 1], fit2$coef[3, 2]) 
    ## Effect adjusting for socioeconomic status.
    fit3 <- summary(lm(birthweight ~ mat_smoking_status + sep))
    m.model3[I, ] <- c(fit3$coef[2, 1], fit3$coef[2, 2], fit3$coef[3, 1], fit3$coef[3, 2], fit3$coef[4, 1], fit3$coef[4, 2]) 
    ## Effect adjusting for maternal BMI and socioeconomic status.
    fit4 <- summary(lm(birthweight ~ mat_smoking_status + mat_bmi + sep))
    m.model4[I, ] <- c(fit4$coef[2, 1], fit4$coef[2, 2], fit4$coef[3, 1], fit4$coef[3, 2], fit4$coef[4, 1], fit4$coef[4, 2], fit4$coef[5, 1], fit4$coef[5, 2]) 
    
    ## Observational effect in families with participating fathers.
    fit5 <- summary(lm(birthweight[pat_r == 1] ~ mat_smoking_status[pat_r == 1]))
    m.model5[I, ] <- c(fit5$coef[2, 1], fit5$coef[2, 2]) 
    ## Effect adjusting for maternal BMI.
    fit6 <- summary(lm(birthweight[pat_r == 1] ~ mat_smoking_status[pat_r == 1] + mat_bmi[pat_r == 1]))
    m.model6[I, ] <- c(fit6$coef[2, 1], fit6$coef[2, 2], fit6$coef[3, 1], fit6$coef[3, 2]) 
    ## Effect adjusting for socioeconomic status.
    fit7 <- summary(lm(birthweight[pat_r == 1] ~ mat_smoking_status[pat_r == 1] + sep[pat_r == 1, ]))
    m.model7[I, ] <- c(fit7$coef[2, 1], fit7$coef[2, 2], fit7$coef[3, 1], fit7$coef[3, 2], fit7$coef[4, 1], fit7$coef[4, 2]) 
    ## Effect adjusting for maternal BMI and socioeconomic status.
    fit8 <- summary(lm(birthweight[pat_r == 1] ~ mat_smoking_status[pat_r == 1] + mat_bmi[pat_r == 1] + sep[pat_r == 1, ]))
    m.model8[I, ] <- c(fit8$coef[2, 1], fit8$coef[2, 2], fit8$coef[3, 1], fit8$coef[3, 2], fit8$coef[4, 1], fit8$coef[4, 2], fit8$coef[5, 1], fit8$coef[5, 2]) 
    
    
    ##   JOINT EFFECTS   ##
    
    ## Observational effect in the whole sample.
    fit1 <- summary(lm(birthweight ~ mat_smoking_status + pat_smoking_status))
    j.model1[I, ] <- c(fit1$coef[2, 1], fit1$coef[2, 2], fit1$coef[3, 1], fit1$coef[3, 2]) 
    ## Effect adjusting for parental BMI.
    fit2 <- summary(lm(birthweight ~ mat_smoking_status + pat_smoking_status + mat_bmi + pat_bmi))
    j.model2[I, ] <- c(fit2$coef[2, 1], fit2$coef[2, 2], fit2$coef[3, 1], fit2$coef[3, 2], fit2$coef[4, 1], fit2$coef[4, 2], fit2$coef[5, 1], fit2$coef[5, 2]) 
    ## Effect adjusting for socioeconomic status.
    fit3 <- summary(lm(birthweight ~ mat_smoking_status + pat_smoking_status + sep))
    j.model3[I, ] <- c(fit3$coef[2, 1], fit3$coef[2, 2], fit3$coef[3, 1], fit3$coef[3, 2], fit3$coef[4, 1], fit3$coef[4, 2], fit3$coef[5, 1], fit3$coef[5, 2]) 
    ## Effect adjusting for parental BMI and socioeconomic status.
    fit4 <- summary(lm(birthweight ~ mat_smoking_status + pat_smoking_status + mat_bmi + pat_bmi + sep))
    j.model4[I, ] <- c(fit4$coef[2, 1], fit4$coef[2, 2], fit4$coef[3, 1], fit4$coef[3, 2], fit4$coef[4, 1], fit4$coef[4, 2], fit4$coef[5, 1], fit4$coef[5, 2], fit4$coef[6, 1], fit4$coef[6, 2], fit4$coef[7, 1], fit4$coef[7, 2]) 
    
    ## Observational effect in families with participating fathers.
    fit5 <- summary(lm(birthweight[pat_r == 1] ~ mat_smoking_status[pat_r == 1] + pat_smoking_status[pat_r == 1]))
    j.model5[I, ] <- c(fit5$coef[2, 1], fit5$coef[2, 2], fit5$coef[3, 1], fit5$coef[3, 2]) 
    ## Effect adjusting for parental BMI.
    fit6 <- summary(lm(birthweight[pat_r == 1] ~ mat_smoking_status[pat_r == 1] + pat_smoking_status[pat_r == 1] + mat_bmi[pat_r == 1] + pat_bmi[pat_r == 1]))
    j.model6[I, ] <- c(fit6$coef[2, 1], fit6$coef[2, 2], fit6$coef[3, 1], fit6$coef[3, 2], fit6$coef[4, 1], fit6$coef[4, 2], fit6$coef[5, 1], fit6$coef[5, 2]) 
    ## Effect adjusting for socioeconomic status.
    fit7 <- summary(lm(birthweight[pat_r == 1] ~ mat_smoking_status[pat_r == 1] + pat_smoking_status[pat_r == 1] + sep[pat_r == 1, ]))
    j.model7[I, ] <- c(fit7$coef[2, 1], fit7$coef[2, 2], fit7$coef[3, 1], fit7$coef[3, 2], fit7$coef[4, 1], fit7$coef[4, 2], fit7$coef[5, 1], fit7$coef[5, 2]) 
    ## Effect adjusting for parental BMI and socioeconomic status.
    fit8 <- summary(lm(birthweight[pat_r == 1] ~ mat_smoking_status[pat_r == 1] + pat_smoking_status[pat_r == 1] + mat_bmi[pat_r == 1] + pat_bmi[pat_r == 1] + sep[pat_r == 1, ]))
    j.model8[I, ] <- c(fit8$coef[2, 1], fit8$coef[2, 2], fit8$coef[3, 1], fit8$coef[3, 2], fit8$coef[4, 1], fit8$coef[4, 2], fit8$coef[5, 1], fit8$coef[5, 2], fit8$coef[6, 1], fit8$coef[6, 2], fit8$coef[7, 1], fit8$coef[7, 2]) 
    
    
    ## Store pairwise trait correlations.
    corrs <- corrs + cor(cbind(pat_smoking_status, mat_smoking_status, sep[, 2], sep[, 1], pat_bmi, mat_bmi, birthweight)) / iter
    
    ## Print progress.
    if (I %% 100 == 0) print(paste(I, " done."))
    
  }
  
  ## Save results for the specified selection effects.
  filename <- paste("Main_Simulation_", J, ".RData", sep = "")
  save(p.model1, p.model2, p.model3, p.model4, p.model5, p.model6, p.model7, p.model8, 
       m.model1, m.model2, m.model3, m.model4, m.model5, m.model6, m.model7, m.model8, 
       j.model1, j.model2, j.model3, j.model4, j.model5, j.model6, j.model7, j.model8, 
       missing.prop, smoking.prop, corrs, z.stats, file = filename)
  
}

#################################################

##########   CONFOUNDED SIMULATION   ##########

## This simulation modifies the main simulation by adding
## confounding between the outcome and selection. The
## variance of the confounder between participation and 
## birthweight is set so that their correlation is the
## same as that observed in ALSPAC.

## Seed for the simulations.
seed <- 8482599

## Number of iterations.
iter <- 1000

## Sample size.
N <- 15000
## ALSPAC has about 15000 participating families.

## Generate selection effects (Odds Ratios) for fathers' participation.
pat_bmi_list <- c(exp(0.00), 1.5, 2, 5, 10, 20, 1.5, 2, 5, 10, 20)
pat_sep_list <- c(exp(0.46), 1.5, 2, 5, 10, 20, 1.5, 2, 5, 10, 20)
pat_smk_list <- c(exp(-0.81), 1, 1, 1, 1, 1, 2/3, 1/2, 1/5, 1/10, 1/20)
mat_sep_or <- exp(0.324)   ## Fixed to observed value.

## Generate selection intercepts so that 76.7% are selected.
sel_int_list <- c(0.75, 0.39, 0.16, -0.38, -0.67, -0.94, 0.60, 0.54, 0.56, 0.68, 0.83)

## Set the variance of the new confounder term to match the
## birthweight-participation correlation in ALSPAC.
ubw_var <- c(0.0151, 0.0144, 0.018, 0.0334, 0.0528, 0.082, 0.0149, 0.0203, 0.0376, 0.0631, 0.0957)


## Start looping over selection effects.
for (J in 1:11) {
  
  ## Track progress.
  print(paste("Parameter Setting ", J, " Initiated", sep = ""))
  
  ## Specify the selection effects.
  pat_bmi_or <- pat_bmi_list[J]
  pat_sep_or <- pat_sep_list[J]
  pat_smk_or <- pat_smk_list[J]
  selection.intercept <- sel_int_list[J]
  ubw_variance <- ubw_var[J]
  
  ## Store simulation results for paternal effects here.
  p.model1 <- matrix(0, iter, 2)
  p.model2 <- matrix(0, iter, 4)
  p.model3 <- matrix(0, iter, 6)
  p.model4 <- matrix(0, iter, 8)
  colnames(p.model1) <- c("Estimate", "Std Err")
  colnames(p.model2) <- c("Estimate", "Std Err", "Bmi Est", "Bmi Se")
  colnames(p.model3) <- c("Estimate", "Std Err", "Sep-M Est", "Sep-M Se", "Sep-P Est", "Sep-P Se")
  colnames(p.model4) <- c("Estimate", "Std Err", "Bmi Est", "Bmi Se", "Sep-M Est", "Sep-M Se", "Sep-P Est", "Sep-P Se")
  p.model5 <- matrix(0, iter, 2)
  p.model6 <- matrix(0, iter, 4)
  p.model7 <- matrix(0, iter, 6)
  p.model8 <- matrix(0, iter, 8)
  colnames(p.model5) <- c("Estimate", "Std Err")
  colnames(p.model6) <- c("Estimate", "Std Err", "Bmi Est", "Bmi Se")
  colnames(p.model7) <- c("Estimate", "Std Err", "Sep-M Est", "Sep-M Se", "Sep-P Est", "Sep-P Se")
  colnames(p.model8) <- c("Estimate", "Std Err", "Bmi Est", "Bmi Se", "Sep-M Est", "Sep-M Se", "Sep-P Est", "Sep-P Se")
  
  ## Store simulation results for maternal effects here.
  m.model1 <- matrix(0, iter, 2)
  m.model2 <- matrix(0, iter, 4)
  m.model3 <- matrix(0, iter, 6)
  m.model4 <- matrix(0, iter, 8)
  colnames(m.model1) <- c("Estimate", "Std Err")
  colnames(m.model2) <- c("Estimate", "Std Err", "Bmi Est", "Bmi Se")
  colnames(m.model3) <- c("Estimate", "Std Err", "Sep-M Est", "Sep-M Se", "Sep-P Est", "Sep-P Se")
  colnames(m.model4) <- c("Estimate", "Std Err", "Bmi Est", "Bmi Se", "Sep-M Est", "Sep-M Se", "Sep-P Est", "Sep-P Se")
  m.model5 <- matrix(0, iter, 2)
  m.model6 <- matrix(0, iter, 4)
  m.model7 <- matrix(0, iter, 6)
  m.model8 <- matrix(0, iter, 8)
  colnames(m.model5) <- c("Estimate", "Std Err")
  colnames(m.model6) <- c("Estimate", "Std Err", "Bmi Est", "Bmi Se")
  colnames(m.model7) <- c("Estimate", "Std Err", "Sep-M Est", "Sep-M Se", "Sep-P Est", "Sep-P Se")
  colnames(m.model8) <- c("Estimate", "Std Err", "Bmi Est", "Bmi Se", "Sep-M Est", "Sep-M Se", "Sep-P Est", "Sep-P Se")
  
  ## Store simulation results for joint effects here.
  j.model1 <- matrix(0, iter, 4)
  j.model2 <- matrix(0, iter, 8)
  j.model3 <- matrix(0, iter, 8)
  j.model4 <- matrix(0, iter, 12)
  colnames(j.model1) <- c("M Estimate", "M Std Err", "P Estimate", "P Std Err")
  colnames(j.model2) <- c("M Estimate", "M Std Err", "P Estimate", "P Std Err", "Bmi-M Est", "Bmi-M Se", "Bmi-P Est", "Bmi-P Se")
  colnames(j.model3) <- c("M Estimate", "M Std Err", "P Estimate", "P Std Err", "Sep-M Est", "Sep-M Se", "Sep-P Est", "Sep-P Se")
  colnames(j.model4) <- c("M Estimate", "M Std Err", "P Estimate", "P Std Err", "Bmi-M Est", "Bmi-M Se", "Bmi-P Est", "Bmi-P Se", "Sep-M Est", "Sep-M Se", "Sep-P Est", "Sep-P Se")
  j.model5 <- matrix(0, iter, 4)
  j.model6 <- matrix(0, iter, 8)
  j.model7 <- matrix(0, iter, 8)
  j.model8 <- matrix(0, iter, 12)
  colnames(j.model5) <- c("M Estimate", "M Std Err", "P Estimate", "P Std Err")
  colnames(j.model6) <- c("M Estimate", "M Std Err", "P Estimate", "P Std Err", "Bmi-M Est", "Bmi-M Se", "Bmi-P Est", "Bmi-P Se")
  colnames(j.model7) <- c("M Estimate", "M Std Err", "P Estimate", "P Std Err", "Sep-M Est", "Sep-M Se", "Sep-P Est", "Sep-P Se")
  colnames(j.model8) <- c("M Estimate", "M Std Err", "P Estimate", "P Std Err", "Bmi-M Est", "Bmi-M Se", "Bmi-P Est", "Bmi-P Se", "Sep-M Est", "Sep-M Se", "Sep-P Est", "Sep-P Se")
  
  ## Store prevalence of smoking and proportion of missingness.
  missing.prop <- rep(0, iter)
  smoking.prop <- matrix(0, iter, 2)
  colnames(smoking.prop) <- c("Maternal", "Paternal")
  
  ## Store pairwise trait correlations.
  corrs <- matrix(0, 7, 7)
  colnames(corrs) <- c("Smoking P", "Smoking M", "SEP P", "SEP M", "BMI P", "BMI M", "Birthweight")
  rownames(corrs) <- c("Smoking P", "Smoking M", "SEP P", "SEP M", "BMI P", "BMI M", "Birthweight")
  
  ## Store Z-statistic moments.
  z.stats <- matrix(0, iter, 6)
  colnames(z.stats) <- c("pBMI Mean", "pBMI Std", "mBMI Mean", "mBMI Std", "BW Mean", "BW Std")
  
  
  ## Start the iteration loop.
  for (I in 1:iter) {
    
    ## Seed it.
    set.seed (seed + 10000 * J + I)
    
    ## Generate a confounding effect for maternal and paternal SEP.
    U.sep <- rnorm(N, 0, 1)
    
    ## Generate paternal and maternal SEP.
    ## These are correlated with a correlation larger than 0.575.
    ## The correlation is tuned to get 0.575 for the 0/1/2/3 counts.
    sep.cont <- matrix(0, N, 2)
    sep.cont[, 1] <- sqrt(0.654) * U.sep + sqrt(1 - 0.654) * rnorm(N, 0, 1)
    sep.cont[, 2] <- sqrt(0.654) * U.sep + sqrt(1 - 0.654) * rnorm(N, 0, 1)
    
    ## Compute thresholds in order to convert continuous values to 0/1/2/3.
    ## Proportions in ALSPAC were 0 = 20.2%, 1 = 44.5%, 2 = 22.4%, 3 = 12.9%.
    sep.thresholds <- qnorm(c(0.202, 0.647, 0.871))
    
    ## Convert.
    sep <- matrix(0, N, 2)
    sep[which(sep.cont[, 1] > sep.thresholds[1]), 1] <- 1
    sep[which(sep.cont[, 1] > sep.thresholds[2]), 1] <- 2
    sep[which(sep.cont[, 1] > sep.thresholds[3]), 1] <- 3
    sep[which(sep.cont[, 2] > sep.thresholds[1]), 2] <- 1
    sep[which(sep.cont[, 2] > sep.thresholds[2]), 2] <- 2
    sep[which(sep.cont[, 2] > sep.thresholds[3]), 2] <- 3
    colnames(sep) <- c("Maternal", "Paternal")
    
    ## Generate a confounding effect for maternal and paternal smoking.
    U.smk <- rnorm(N, 0, 1)
    
    ## Generate paternal smoking status (binary - logistic).
    pat_smoking_probs <- expit( - 0.608 * sep[, 2] + 2.65 * U.smk + log(1.92))
    pat_smoking_status <- rbinom(N, 1, pat_smoking_probs)
    smoking.prop[I, 2] <- sum(pat_smoking_status) / N
    
    ## Generate maternal smoking status, likewise.
    mat_smoking_probs <- expit( - 0.692 * sep[, 1] + 2.65 * U.smk + log(0.46))
    mat_smoking_status <- rbinom(N, 1, mat_smoking_probs)
    smoking.prop[I, 1] <- sum(mat_smoking_status) / N
    
    ## Generate a confounding effect for maternal and paternal BMI.
    U.bmi <- rnorm(N, 0, 1)
    
    ## Generate paternal BMI (Z-scores).
    pat_bmi <- rnorm(N, 0.165, 0.918) - 0.105 * pat_smoking_status - 0.088 * sep[, 2] + 0.4 * U.bmi
    ## Confounding coefficients selected to adjust parental 
    ## BMI correlations, error coefficients to standardize.
    
    ## Generate maternal BMI (Z-scores).
    mat_bmi <- rnorm(N, 0.201, 0.911) - 0.116 * mat_smoking_status - 0.129 * sep[, 1] + 0.4 * U.bmi
    
    ## Generate a confounder between birthweight and selection.
    U.bw <- rnorm(N, 0, sqrt(0.1))   ## 10% of variation in birthweight.
    ## 10% is probably a lot, given that BMI and Smoking only 
    ## account for 5%, but it will help illustrate the bias.
    
    ## Generate offspring birthweight.
    birthweight <- rnorm(N, 0.168, 0.919) + U.bw + 0.055 * pat_bmi + 0.141 * mat_bmi - 0.155 * pat_smoking_status - 0.309 * mat_smoking_status
    
    ## Generate paternal selection indicator.
    pat_selection_probs <- expit(U.bw + log(mat_sep_or) * sep[, 1] + log(pat_sep_or) * sep[, 2] + log(pat_bmi_or) * pat_bmi + log(pat_smk_or) * pat_smoking_status + selection.intercept)
    pat_r <- rbinom(N, 1, pat_selection_probs)
    missing.prop[I] <- 1 - sum(pat_r) / N
    ## Variance is 0.2 for U.bw and 0.65 for the other terms within expit.
    ## Fairly strong, but again we want it to be strong.
    
    ## Store Z-statistic means and variances as a diagnostic.
    z.stats[I, ] <- c(mean(pat_bmi), sd(pat_bmi), mean(mat_bmi), sd(mat_bmi), mean(birthweight), sd(birthweight))
    
    
    ##   PATERNAL EFFECTS   ##
    
    ## Observational effect in the whole sample.
    fit1 <- summary(lm(birthweight ~ pat_smoking_status))
    p.model1[I, ] <- c(fit1$coef[2, 1], fit1$coef[2, 2]) 
    ## Effect adjusting for paternal BMI.
    fit2 <- summary(lm(birthweight ~ pat_smoking_status + pat_bmi))
    p.model2[I, ] <- c(fit2$coef[2, 1], fit2$coef[2, 2], fit2$coef[3, 1], fit2$coef[3, 2]) 
    ## Effect adjusting for socioeconomic status.
    fit3 <- summary(lm(birthweight ~ pat_smoking_status + sep))
    p.model3[I, ] <- c(fit3$coef[2, 1], fit3$coef[2, 2], fit3$coef[3, 1], fit3$coef[3, 2], fit3$coef[4, 1], fit3$coef[4, 2]) 
    ## Effect adjusting for paternal BMI and socioeconomic status.
    fit4 <- summary(lm(birthweight ~ pat_smoking_status + pat_bmi + sep))
    p.model4[I, ] <- c(fit4$coef[2, 1], fit4$coef[2, 2], fit4$coef[3, 1], fit4$coef[3, 2], fit4$coef[4, 1], fit4$coef[4, 2], fit4$coef[5, 1], fit4$coef[5, 2]) 
    
    ## Observational effect in families with participating fathers.
    fit5 <- summary(lm(birthweight[pat_r == 1] ~ pat_smoking_status[pat_r == 1]))
    p.model5[I, ] <- c(fit5$coef[2, 1], fit5$coef[2, 2]) 
    ## Effect adjusting for paternal BMI.
    fit6 <- summary(lm(birthweight[pat_r == 1] ~ pat_smoking_status[pat_r == 1] + pat_bmi[pat_r == 1]))
    p.model6[I, ] <- c(fit6$coef[2, 1], fit6$coef[2, 2], fit6$coef[3, 1], fit6$coef[3, 2]) 
    ## Effect adjusting for socioeconomic status.
    fit7 <- summary(lm(birthweight[pat_r == 1] ~ pat_smoking_status[pat_r == 1] + sep[pat_r == 1, ]))
    p.model7[I, ] <- c(fit7$coef[2, 1], fit7$coef[2, 2], fit7$coef[3, 1], fit7$coef[3, 2], fit7$coef[4, 1], fit7$coef[4, 2]) 
    ## Effect adjusting for paternal BMI and socioeconomic status.
    fit8 <- summary(lm(birthweight[pat_r == 1] ~ pat_smoking_status[pat_r == 1] + pat_bmi[pat_r == 1] + sep[pat_r == 1, ]))
    p.model8[I, ] <- c(fit8$coef[2, 1], fit8$coef[2, 2], fit8$coef[3, 1], fit8$coef[3, 2], fit8$coef[4, 1], fit8$coef[4, 2], fit8$coef[5, 1], fit8$coef[5, 2]) 
    
    
    ##   MATERNAL EFFECTS   ##
    
    ## Observational effect in the whole sample.
    fit1 <- summary(lm(birthweight ~ mat_smoking_status))
    m.model1[I, ] <- c(fit1$coef[2, 1], fit1$coef[2, 2]) 
    ## Effect adjusting for maternal BMI.
    fit2 <- summary(lm(birthweight ~ mat_smoking_status + mat_bmi))
    m.model2[I, ] <- c(fit2$coef[2, 1], fit2$coef[2, 2], fit2$coef[3, 1], fit2$coef[3, 2]) 
    ## Effect adjusting for socioeconomic status.
    fit3 <- summary(lm(birthweight ~ mat_smoking_status + sep))
    m.model3[I, ] <- c(fit3$coef[2, 1], fit3$coef[2, 2], fit3$coef[3, 1], fit3$coef[3, 2], fit3$coef[4, 1], fit3$coef[4, 2]) 
    ## Effect adjusting for maternal BMI and socioeconomic status.
    fit4 <- summary(lm(birthweight ~ mat_smoking_status + mat_bmi + sep))
    m.model4[I, ] <- c(fit4$coef[2, 1], fit4$coef[2, 2], fit4$coef[3, 1], fit4$coef[3, 2], fit4$coef[4, 1], fit4$coef[4, 2], fit4$coef[5, 1], fit4$coef[5, 2]) 
    
    ## Observational effect in families with participating fathers.
    fit5 <- summary(lm(birthweight[pat_r == 1] ~ mat_smoking_status[pat_r == 1]))
    m.model5[I, ] <- c(fit5$coef[2, 1], fit5$coef[2, 2]) 
    ## Effect adjusting for maternal BMI.
    fit6 <- summary(lm(birthweight[pat_r == 1] ~ mat_smoking_status[pat_r == 1] + mat_bmi[pat_r == 1]))
    m.model6[I, ] <- c(fit6$coef[2, 1], fit6$coef[2, 2], fit6$coef[3, 1], fit6$coef[3, 2]) 
    ## Effect adjusting for socioeconomic status.
    fit7 <- summary(lm(birthweight[pat_r == 1] ~ mat_smoking_status[pat_r == 1] + sep[pat_r == 1, ]))
    m.model7[I, ] <- c(fit7$coef[2, 1], fit7$coef[2, 2], fit7$coef[3, 1], fit7$coef[3, 2], fit7$coef[4, 1], fit7$coef[4, 2]) 
    ## Effect adjusting for maternal BMI and socioeconomic status.
    fit8 <- summary(lm(birthweight[pat_r == 1] ~ mat_smoking_status[pat_r == 1] + mat_bmi[pat_r == 1] + sep[pat_r == 1, ]))
    m.model8[I, ] <- c(fit8$coef[2, 1], fit8$coef[2, 2], fit8$coef[3, 1], fit8$coef[3, 2], fit8$coef[4, 1], fit8$coef[4, 2], fit8$coef[5, 1], fit8$coef[5, 2]) 
    
    
    ##   JOINT EFFECTS   ##
    
    ## Observational effect in the whole sample.
    fit1 <- summary(lm(birthweight ~ mat_smoking_status + pat_smoking_status))
    j.model1[I, ] <- c(fit1$coef[2, 1], fit1$coef[2, 2], fit1$coef[3, 1], fit1$coef[3, 2]) 
    ## Effect adjusting for parental BMI.
    fit2 <- summary(lm(birthweight ~ mat_smoking_status + pat_smoking_status + mat_bmi + pat_bmi))
    j.model2[I, ] <- c(fit2$coef[2, 1], fit2$coef[2, 2], fit2$coef[3, 1], fit2$coef[3, 2], fit2$coef[4, 1], fit2$coef[4, 2], fit2$coef[5, 1], fit2$coef[5, 2]) 
    ## Effect adjusting for socioeconomic status.
    fit3 <- summary(lm(birthweight ~ mat_smoking_status + pat_smoking_status + sep))
    j.model3[I, ] <- c(fit3$coef[2, 1], fit3$coef[2, 2], fit3$coef[3, 1], fit3$coef[3, 2], fit3$coef[4, 1], fit3$coef[4, 2], fit3$coef[5, 1], fit3$coef[5, 2]) 
    ## Effect adjusting for parental BMI and socioeconomic status.
    fit4 <- summary(lm(birthweight ~ mat_smoking_status + pat_smoking_status + mat_bmi + pat_bmi + sep))
    j.model4[I, ] <- c(fit4$coef[2, 1], fit4$coef[2, 2], fit4$coef[3, 1], fit4$coef[3, 2], fit4$coef[4, 1], fit4$coef[4, 2], fit4$coef[5, 1], fit4$coef[5, 2], fit4$coef[6, 1], fit4$coef[6, 2], fit4$coef[7, 1], fit4$coef[7, 2]) 
    
    ## Observational effect in families with participating fathers.
    fit5 <- summary(lm(birthweight[pat_r == 1] ~ mat_smoking_status[pat_r == 1] + pat_smoking_status[pat_r == 1]))
    j.model5[I, ] <- c(fit5$coef[2, 1], fit5$coef[2, 2], fit5$coef[3, 1], fit5$coef[3, 2]) 
    ## Effect adjusting for parental BMI.
    fit6 <- summary(lm(birthweight[pat_r == 1] ~ mat_smoking_status[pat_r == 1] + pat_smoking_status[pat_r == 1] + mat_bmi[pat_r == 1] + pat_bmi[pat_r == 1]))
    j.model6[I, ] <- c(fit6$coef[2, 1], fit6$coef[2, 2], fit6$coef[3, 1], fit6$coef[3, 2], fit6$coef[4, 1], fit6$coef[4, 2], fit6$coef[5, 1], fit6$coef[5, 2]) 
    ## Effect adjusting for socioeconomic status.
    fit7 <- summary(lm(birthweight[pat_r == 1] ~ mat_smoking_status[pat_r == 1] + pat_smoking_status[pat_r == 1] + sep[pat_r == 1, ]))
    j.model7[I, ] <- c(fit7$coef[2, 1], fit7$coef[2, 2], fit7$coef[3, 1], fit7$coef[3, 2], fit7$coef[4, 1], fit7$coef[4, 2], fit7$coef[5, 1], fit7$coef[5, 2]) 
    ## Effect adjusting for parental BMI and socioeconomic status.
    fit8 <- summary(lm(birthweight[pat_r == 1] ~ mat_smoking_status[pat_r == 1] + pat_smoking_status[pat_r == 1] + mat_bmi[pat_r == 1] + pat_bmi[pat_r == 1] + sep[pat_r == 1, ]))
    j.model8[I, ] <- c(fit8$coef[2, 1], fit8$coef[2, 2], fit8$coef[3, 1], fit8$coef[3, 2], fit8$coef[4, 1], fit8$coef[4, 2], fit8$coef[5, 1], fit8$coef[5, 2], fit8$coef[6, 1], fit8$coef[6, 2], fit8$coef[7, 1], fit8$coef[7, 2]) 
    
    
    ## Store pairwise trait correlations.
    corrs <- corrs + cor(cbind(pat_smoking_status, mat_smoking_status, sep[, 2], sep[, 1], pat_bmi, mat_bmi, birthweight)) / iter
    
    ## Print progress.
    if (I %% 100 == 0) print(paste(I, " done."))
    
  }
  
  ## Save results for the specified selection effects.
  filename <- paste("Confounded_Simulation_", J, ".RData", sep = "")
  save(p.model1, p.model2, p.model3, p.model4, p.model5, p.model6, p.model7, p.model8, 
       m.model1, m.model2, m.model3, m.model4, m.model5, m.model6, m.model7, m.model8, 
       j.model1, j.model2, j.model3, j.model4, j.model5, j.model6, j.model7, j.model8, 
       missing.prop, smoking.prop, corrs, z.stats, file = filename)
  
}

#################################################

## Now create Tables and Plots with the main results.

##########   CREATE RESULTS TABLES   ##########

## Three Tables to create: one for the main simulation with 
## no BMI adjustment, one for the main simulation with BMI 
## adjustment and one for the confounded simulation.



## Table 1 - Main simulation, no BMI adjustment.
## This is actually Supplementary Table 2 in the paper.

## Set up the table.
Table1 <- matrix(0, 11, 10)
rownames(Table1) <- print(paste("Sc", 1:11, sep = ""))
colnames(Table1) <- rep(c("Bias (Abs)", "Bias (Rel)", "StdErr", "95% Cov", "B-E Cov"), times = 2)

## Compute the true values as the mean estimated effects
## ("true" contains multiple pathways so here it's easier
## to take "oracle" true values across simulations, rather
## than theoretical parameter values).
tm <- matrix(0, 11, 4)
for (j in 1:11) {
  filename <- paste("Main_Simulation_", j, ".RData", sep = "")
  load(filename)
  tm[j, ] <- c(mean(p.model3[, 1]), mean(m.model3[, 1]), mean(j.model3[, 3]), mean(j.model3[, 1]))
}
tm0 <- colMeans(tm)

## Store results from each Scenario.
for (j in 1:11) {
  filename <- paste("Main_Simulation_", j, ".RData", sep = "")
  load(filename)
  Table1[j, 1:5] <- c(mean(j.model7[, 3] - tm0[3]), mean((j.model7[, 3] - tm0[3]) / tm0[3]), sqrt(mean(j.model7[, 4]^2)), mean( (j.model7[, 3] - tm0[3] - 1.96 * j.model7[, 4]) * (j.model7[, 3] - tm0[3] + 1.96 * j.model7[, 4]) < 0 ), mean( (j.model7[, 3] - mean(j.model7[, 3]) - 1.96 * j.model7[, 4]) * (j.model7[, 3] - mean(j.model7[, 3]) + 1.96 * j.model7[, 4]) < 0 ))
  Table1[j, 6:10] <- c(mean(j.model7[, 1] - tm0[4]), mean((j.model7[, 1] - tm0[4]) / tm0[4]), sqrt(mean(j.model7[, 2]^2)), mean( (j.model7[, 1] - tm0[4] - 1.96 * j.model7[, 2]) * (j.model7[, 1] - tm0[4] + 1.96 * j.model7[, 2]) < 0 ), mean( (j.model7[, 1] - mean(j.model7[, 1]) - 1.96 * j.model7[, 2]) * (j.model7[, 1] - mean(j.model7[, 1]) + 1.96 * j.model7[, 2]) < 0 ))
}



## Table 2 - Main simulation, with BMI adjustment.
## This is actually Supplementary Table 3 in the paper.

## Set up the table.
Table2 <- matrix(0, 11, 10)
rownames(Table2) <- print(paste("Sc", 1:11, sep = ""))
colnames(Table2) <- rep(c("Bias (Abs)", "Bias (Rel)", "StdErr", "95% Cov", "B-E Cov"), times = 2)

## Compute the true values as the mean estimated effects.
tn <- matrix(0, 11, 4)
for (j in 1:11) {
  filename <- paste("Main_Simulation_", j, ".RData", sep = "")
  load(filename)
  tn[j, ] <- c(mean(p.model4[, 1]), mean(m.model4[, 1]), mean(j.model4[, 3]), mean(j.model4[, 1]))
}
tn0 <- colMeans(tn)

## Store results from each Scenario.
for (j in 1:11) {
  filename <- paste("Main_Simulation_", j, ".RData", sep = "")
  load(filename)
  Table2[j, 1:5] <- c(mean(j.model8[, 3] - tn0[3]), mean((j.model8[, 3] - tn0[3]) / tn0[3]), sqrt(mean(j.model8[, 4]^2)), mean( (j.model8[, 3] - tn0[3] - 1.96 * j.model8[, 4]) * (j.model8[, 3] - tn0[3] + 1.96 * j.model8[, 4]) < 0 ), mean( (j.model8[, 3] - mean(j.model8[, 3]) - 1.96 * j.model8[, 4]) * (j.model8[, 3] - mean(j.model8[, 3]) + 1.96 * j.model8[, 4]) < 0 ))
  Table2[j, 6:10] <- c(mean(j.model8[, 1] - tn0[4]), mean((j.model8[, 1] - tn0[4]) / tn0[4]), sqrt(mean(j.model8[, 2]^2)), mean( (j.model8[, 1] - tn0[4] - 1.96 * j.model8[, 2]) * (j.model8[, 1] - tn0[4] + 1.96 * j.model8[, 2]) < 0 ), mean( (j.model8[, 1] - mean(j.model8[, 1]) - 1.96 * j.model8[, 2]) * (j.model8[, 1] - mean(j.model8[, 1]) + 1.96 * j.model8[, 2]) < 0 ))
}



## Table 3 - Confounded simulation, no BMI adjustment.
## This is actually Supplementary Table 4 in the paper.

## Set up the table.
Table3 <- matrix(0, 11, 10)
rownames(Table3) <- paste("Sc", 1:11, sep = "")
colnames(Table3) <- rep(c("Bias (Abs)", "Bias (Rel)", "StdErr", "95% Cov", "B-E Cov"), times = 2)

## Compute the true values as the mean estimated effects.
tp <- matrix(0, 11, 4)
for (j in 1:11) {
  filename <- paste("Confounded_Simulation_", j, ".RData", sep = "")
  load(filename)
  tp[j, ] <- c(mean(p.model3[, 1]), mean(m.model3[, 1]), mean(j.model3[, 3]), mean(j.model3[, 1]))
}
tp0 <- colMeans(tp)

## Store results from each Scenario.
for (j in 1:11) {
  filename <- paste("Confounded_Simulation_", j, ".RData", sep = "")
  load(filename)
  Table3[j, 1:5] <- c(mean(j.model7[, 3] - tp0[3]), mean((j.model7[, 3] - tp0[3]) / tp0[3]), sqrt(mean(j.model7[, 4]^2)), mean( (j.model7[, 3] - tp0[3] - 1.96 * j.model7[, 4]) * (j.model7[, 3] - tp0[3] + 1.96 * j.model7[, 4]) < 0 ), mean( (j.model7[, 3] - mean(j.model7[, 3]) - 1.96 * j.model7[, 4]) * (j.model7[, 3] - mean(j.model7[, 3]) + 1.96 * j.model7[, 4]) < 0 ))
  Table3[j, 6:10] <- c(mean(j.model7[, 1] - tp0[4]), mean((j.model7[, 1] - tp0[4]) / tp0[4]), sqrt(mean(j.model7[, 2]^2)), mean( (j.model7[, 1] - tp0[4] - 1.96 * j.model7[, 2]) * (j.model7[, 1] - tp0[4] + 1.96 * j.model7[, 2]) < 0 ), mean( (j.model7[, 1] - mean(j.model7[, 1]) - 1.96 * j.model7[, 2]) * (j.model7[, 1] - mean(j.model7[, 1]) + 1.96 * j.model7[, 2]) < 0 ))
}

##################################################

##########   PLOT RESULTS   ##########

## Simplify further, keep only results that are not adjusted for BMI.

## Start plotting.
pdf(file = "SmokingAndBirthweight.pdf", width = 13, height = 6, pointsize = 16)

## Set up main parameters.
par(mfrow = c(1, 2))
par(mar = c(5, 5, 3, 0))

## Set up colors.
green1 <- rgb(0.2, 0.7, 0.5)
#colors <- rep("green1", 11)

## Arguments for both plots.
plot.means6 <- rep(0, 11); plot.se6 <- rep(0, 11)   ## Joint effects, maternal, without BMI.
plot.means8 <- rep(0, 11); plot.se8 <- rep(0, 11)   ## Joint effects, paternal, without BMI.
for (i in 1:11) {
  load(paste("Main_Simulation_", i, ".RData", sep = ""))
  plot.means6[i] <- mean(j.model7[, 1]); plot.se6[i] <- mean(j.model7[, 2])
  plot.means8[i] <- mean(j.model7[, 3]); plot.se8[i] <- mean(j.model7[, 4])
}
plot.means6 <- rev(plot.means6); plot.se6 <- rev(plot.se6)
plot.means8 <- rev(plot.means8); plot.se8 <- rev(plot.se8)

## Do plot 1.
plot(x = plot.means6, y = 1:11, type = "p", axes = FALSE, main = "Maternal Effects", xlab = "Effect on Birthweight", ylab = "Scenario", xlim = c(-0.4, -0.25), ylim = c(0.5, 11.5), pch = 15, cex.lab = 1.1, col = green1)
for (i in 1:11) {
  lines(x = c(plot.means6[i] - 1.96 * plot.se6[i], plot.means6[i] + 1.96 * plot.se6[i]), y = c(i, i), col = green1)
  lines(x = c(plot.means6[i] - 1.96 * plot.se6[i], plot.means6[i] - 1.96 * plot.se6[i]), y = c(i - 0.1, i + 0.1), col = green1)
  lines(x = c(plot.means6[i] + 1.96 * plot.se6[i], plot.means6[i] + 1.96 * plot.se6[i]), y = c(i - 0.1, i + 0.1), col = green1)
}
axis(side = 2, at = 1:11, labels = 11:1, las = 1, cex.axis = 0.9, cex.lab = 1.1)
axis(side = 1, at = c(-0.4, -0.35, -0.3, -0.25), las = 1, cex.lab = 1.1)
abline(v = c(-0.4, -0.35, -0.3, -0.25), lty = 2, col = "grey")
abline(v = -0.3254, lty = 1, col = "brown")

## Update main parameters.
par(mar = c(5, 3, 3, 2))

## Do plot 2.
plot(x = plot.means8, y = 1:11, type = "p", axes = FALSE, main = "Paternal Effects", xlab = "Effect on Birthweight", ylab = "", xlim = c(-0.25, -0.1), ylim = c(0.5, 11.5), pch = 15, cex.lab = 1.1, col = green1)
for (i in 1:11) {
  lines(x = c(plot.means8[i] - 1.96 * plot.se8[i], plot.means8[i] + 1.96 * plot.se8[i]), y = c(i, i), col = green1)
  lines(x = c(plot.means8[i] - 1.96 * plot.se8[i], plot.means8[i] - 1.96 * plot.se8[i]), y = c(i - 0.1, i + 0.1), col = green1)
  lines(x = c(plot.means8[i] + 1.96 * plot.se8[i], plot.means8[i] + 1.96 * plot.se8[i]), y = c(i - 0.1, i + 0.1), col = green1)
}
#axis(side = 2, at = 1:11, labels = 1:11, las = 1, cex.axis = 0.9, cex.lab = 1.1)
axis(side = 1, at = c(-0.25, -0.2, -0.15, -0.1), las = 1, cex.lab = 1.1)
abline(v = c(-0.25, -0.2, -0.15, -0.1), lty = 2, col = "grey")
abline(v = -0.1608, lty = 1, col = "brown")

## Goodbye.
dev.off()

##################################################



#################################################
