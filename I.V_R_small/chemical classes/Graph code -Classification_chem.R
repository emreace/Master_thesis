install.packages("ggplot2")
library(ggplot2)
install.packages("scales")
library(scales)
install.packages("fun")
library(fun)

# Plot 1 CLphep1 vs KineticIV.Clearance..mL.min.kg. MFE

stat_function_01 <- function(x){
  2 * x
}
stat_function_02 <- function(x){
  (1/2) * x
}
stat_function_03 <- function(x){
  5 * x
}
stat_function_04 <- function(x){
  (1/5) * x
}

Class.List..user. <- ADMETRH$Class.List..user.

x <- ADMETRH$KineticIV.Clearance..mL.min.kg.
y <- ADMETRH$CLphep1*5.978587

CLphep1vsKineticIV.Clearance..mL.min.kg._MFE <- ggplot(data = ADMETRH, mapping = aes(x = x,y = y))+
  geom_point(mapping = aes(ADMETRH$CLphep1*5.978587,x=ADMETRH$KineticIV.Clearance..mL.min.kg.), colour = "#529EFF")+
  scale_y_log10(limits = c(0.01,100), labels = comma)+
  scale_x_log10(limits = c(0.01,100), labels = comma)+
  geom_abline(intercept = 0, slope = 1)+
  stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
  stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
  geom_smooth(method=lm)+
  
  geom_point(aes(colour=Class.List..user., group=Class.List..user.))+
  
  theme_classic()+
  labs(
    title = "CL_IVIVE_hep_MFE vs CLobs",
    subtitle = "",
    x = "CLobs (mL/min/kg)",
    y = "CL_IVIVE_hep_MFE (mL/min/kg)")

CLphep1vsKineticIV.Clearance..mL.min.kg._MFE
ggsave(plot = CLphep1vsKineticIV.Clearance..mL.min.kg._MFE, file = "CLphep1vsKineticIV.Clearance..mL.min.kg._MFE.png")

# Plot 2 CLp,micros1 vs KineticIV.Clearance..mL.min.kg. MFE

stat_function_01 <- function(x){
  2 * x
}
stat_function_02 <- function(x){
  (1/2) * x
}
stat_function_03 <- function(x){
  5 * x
}
stat_function_04 <- function(x){
  (1/5) * x
}

Class.List..user.<-ADMETRH$Class.List..user.

x <- ADMETRH$KineticIV.Clearance..mL.min.kg.
y <- ADMETRH$CLp.micros1*6.336172
fm <- lm(log10(y)~log10(x))
ff <- summary(fm)

CLp.micros1vsKineticIV.Clearance..mL.min.kg._MFE <- ggplot(data = ADMETRH, mapping = aes(x = x,y = y))+
  geom_point(mapping = aes(y=ADMETRH$CLp.micros1*6.336172,x=ADMETRH$KineticIV.Clearance..mL.min.kg.), colour = "#529EFF")+
  scale_y_log10(limits = c(0.01,210), labels = comma)+
  scale_x_log10(limits = c(0.01,210), labels = comma)+
  geom_abline(intercept = 0, slope = 1)+
  stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
  stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
  geom_smooth(method=lm)+
  
  geom_point(aes(colour=Class.List..user., group=Class.List..user.))+
  
  theme_classic()+
  labs(
    title = "CL_IVIVE_mic_MFE vs CLobs",
    subtitle = "",
    x = "CLobs(mL/min/kg)",
    y = "CL_IVIVE_mic_MFE (mL/min/kg)")

CLp.micros1vsKineticIV.Clearance..mL.min.kg._MFE
ggsave(plot = CLp.micros1vsKineticIV.Clearance..mL.min.kg._MFE, file = "CLp.micros1vsKineticIV.Clearance..mL.min.kg._MFE.png")


# Plot 1 CLphep1 vs KineticIV.Clearance..mL.min.kg.

stat_function_01 <- function(x){
  2 * x
}
stat_function_02 <- function(x){
  (1/2) * x
}
stat_function_03 <- function(x){
  5 * x
}
stat_function_04 <- function(x){
  (1/5) * x
}

Class.List..user.<-ADMETRH$Class.List..user.

x <- ADMETRH$KineticIV.Clearance..mL.min.kg.
y <- ADMETRH$CLphep1
fm <- lm(log10(y)~log10(x))
ff <- summary(fm)

CLphep1vsKineticIV.Clearance..mL.min.kg. <- ggplot(data = ADMETRH, mapping = aes(x = x,y = y))+
  geom_point(mapping = aes(y=ADMETRH$CLphep1,x=ADMETRH$KineticIV.Clearance..mL.min.kg.), colour = "#529EFF")+
  scale_y_log10(limits = c(0.01,210), labels = comma)+
  scale_x_log10(limits = c(0.01,210), labels = comma)+
  geom_abline(intercept = 0, slope = 1)+
  stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
  stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
  geom_smooth(method=lm)+
  geom_point(aes(colour=Class.List..user., group=Class.List..user.))+
  
  theme_classic()+
  labs(
    title = "CL_IVIVE_hep vs CLobs",
    subtitle = "",
    x = "CLobs (mL/min/kg)",
    y = "CL_IVIVE_hep (mL/min/kg)")

CLphep1vsKineticIV.Clearance..mL.min.kg.
ggsave(plot = CLphep1vsKineticIV.Clearance..mL.min.kg., file = "CLphep1vsKineticIV.Clearance..mL.min.kg..png")


# Plot 2 CLp,micros1 vs KineticIV.Clearance..mL.min.kg.

stat_function_01 <- function(x){
  2 * x
}
stat_function_02 <- function(x){
  (1/2) * x
}
stat_function_03 <- function(x){
  5 * x
}
stat_function_04 <- function(x){
  (1/5) * x
}

Class.List..user.<-ADMETRH$Class.List..user.

x <- ADMETRH$KineticIV.Clearance..mL.min.kg.
y <- ADMETRH$CLp.micros1
fm <- lm(log10(y)~log10(x))
ff <- summary(fm)

CLp.micros1vsKineticIV.Clearance..mL.min.kg. <- ggplot(data = ADMETRH, mapping = aes(x = x,y = y))+
  geom_point(mapping = aes(y=ADMETRH$CLp.micros1,x=ADMETRH$KineticIV.Clearance..mL.min.kg.), colour = "#529EFF")+
  scale_y_log10(limits = c(0.01,210), labels = comma)+
  scale_x_log10(limits = c(0.01,210), labels = comma)+
  geom_abline(intercept = 0, slope = 1)+
  stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
  stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
  geom_smooth(method=lm)+
  geom_point(aes(colour=Class.List..user., group=Class.List..user.))+

  theme_classic()+
  labs(
    title = "CL_IVIVE_mic vs CLobs",
    subtitle = "",
    x = "CLobs(mL/min/kg)",
    y = "CL_IVIVE_mic (mL/min/kg)")

CLp.micros1vsKineticIV.Clearance..mL.min.kg.
ggsave(plot = CLp.micros1vsKineticIV.Clearance..mL.min.kg., file = "CLp.micros1vsKineticIV.Clearance..mL.min.kg..png")

# TOTAL
# STAND
# Plot 3 CL_totpred_H (ADMET prediction) vs KineticIV.Clearance..mL.min.kg.

stat_function_01 <- function(x){
  2 * x
}
stat_function_02 <- function(x){
  (1/2) * x
}
stat_function_03 <- function(x){
  5 * x
}
stat_function_04 <- function(x){
  (1/5) * x
}

Class.List..user.<-ADMETRH$Class.List..user.

x <- ADMETRH$KineticIV.Clearance..mL.min.kg.
y <- ADMETRH$CL_rat.Dose_mg*66.6668


CL_totpred_HvsKineticIV.Clearance..mL.min.kg. <- ggplot(data = ADMETRH, mapping = aes(x = x,y = y))+
  geom_point(mapping = aes(y=ADMETRH$CL_rat.Dose_mg*66.6668,x=ADMETRH$KineticIV.Clearance..mL.min.kg.), colour = "#529EFF")+
  scale_y_log10(limits = c(0.1,210), labels = comma)+
  scale_x_log10(limits = c(0.1,210), labels = comma)+
  geom_abline(intercept = 0, slope = 1)+
  stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
  stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
  geom_smooth(method=lm)+
  geom_point(aes(colour=Class.List..user., group=Class.List..user.))+
  
  theme_classic()+
  labs(
    title = "pred_CLp_tot_H vs observed_CLp",
    subtitle = "",
    x = "observed_CLp (mL/min/kg)",
    y = "pred_CLp_tot_H (mL/min/kg)")

CL_totpred_HvsKineticIV.Clearance..mL.min.kg.
ggsave(plot = CL_totpred_HvsKineticIV.Clearance..mL.min.kg., file = "CL_totpred_HvsKineticIV.Clearance..mL.min.kg._chem.png")

# Plot 4 CLtotpred_RLM (ADMET prediction) vs KineticIV.Clearance..mL.min.kg.

stat_function_01 <- function(x){
  2 * x
}
stat_function_02 <- function(x){
  (1/2) * x
}
stat_function_03 <- function(x){
  5 * x
}
stat_function_04 <- function(x){
  (1/5) * x
}

Class.List..user.<-ADMETRLM$Class.List..user.

x <- ADMETRLM$KineticIV.Clearance..mL.min.kg.
y <- ADMETRLM$CL_rat.Dose_mg*66.6668
fm <- lm(log10(y)~log10(x))
ff <- summary(fm)

CLtotpred_RLMvsKineticIV.Clearance..mL.min.kg. <- ggplot(data = ADMETRLM, mapping = aes(x = x,y = y))+
  geom_point(mapping = aes(y=ADMETRLM$CL_rat.Dose_mg*66.6668,x=ADMETRLM$KineticIV.Clearance..mL.min.kg.), colour = "#529EFF")+
  scale_y_log10(limits = c(0.1,210), labels = comma)+
  scale_x_log10(limits = c(0.1,210), labels = comma)+
  geom_abline(intercept = 0, slope = 1)+
  stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
  stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
  geom_smooth(method=lm)+
  geom_point(aes(colour=Class.List..user., group=Class.List..user.))+
  
  theme_classic()+
  labs(
    title = "pred_CLp_tot_RLM vs observed_CLp",
    subtitle = "",
    x = "observed_CLp(mL/min/kg)",
    y = "pred_CLp_tot_RLM (mL/min/kg)")

CLtotpred_RLMvsKineticIV.Clearance..mL.min.kg.
ggsave(plot = CLtotpred_RLMvsKineticIV.Clearance..mL.min.kg., file = "CLtotpred_RLMvsKineticIV.Clearance..mL.min.kg._chem.png")

# Plot 3 rCLp_H (ADMET prediction) vs KineticIV.Clearance..mL.min.kg.

stat_function_01 <- function(x){
  2 * x
}
stat_function_02 <- function(x){
  (1/2) * x
}
stat_function_03 <- function(x){
  5 * x
}
stat_function_04 <- function(x){
  (1/5) * x
}

Class.List..user.<-ADMETRH$Class.List..user.

x <- ADMETRH$KineticIV.Clearance..mL.min.kg.
y <- ADMETRH$rCLp_H_mL.min.kg
fm <- lm(log10(y)~log10(x))
ff <- summary(fm)

rCLp_H_ml.min.kgvsKineticIV.Clearance..mL.min.kg. <- ggplot(data = ADMETRH, mapping = aes(x = x, y = y))+
  geom_point(mapping = aes(y=ADMETRH$rCLp_H_mL.min.kg,x=ADMETRH$KineticIV.Clearance..mL.min.kg.), colour = "#529EFF")+
  scale_y_log10(limits = c(0.1,210), labels = comma)+
  scale_x_log10(limits = c(0.1,210), labels = comma)+
  geom_abline(intercept = 0, slope = 1)+
  stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
  stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
  geom_smooth(method=lm)+
  geom_point(aes(colour=Class.List..user., group=Class.List..user.))+

  theme_classic()+
  labs(
    title = "pred_CLp_H vs observed_CLp",
    subtitle = "",
    x = "observed_CLp (mL/min/kg)",
    y = "pred_CLp_H (mL/min/kg)")

rCLp_H_ml.min.kgvsKineticIV.Clearance..mL.min.kg.
ggsave(plot = rCLp_H_ml.min.kgvsKineticIV.Clearance..mL.min.kg., file = "rCLp_H_ml.min.kgvsKineticIV.Clearance..mL.min.kg..png")

# Plot 4 rCLp_H_RLM (ADMET prediction) vs KineticIV.Clearance..mL.min.kg.

stat_function_01 <- function(x){
  2 * x
}
stat_function_02 <- function(x){
  (1/2) * x
}
stat_function_03 <- function(x){
  5 * x
}
stat_function_04 <- function(x){
  (1/5) * x
}

Class.List..user.<-ADMETRLM$Class.List..user.

x <- ADMETRLM$KineticIV.Clearance..mL.min.kg.
y <- ADMETRLM$rCLp_H_RLM_mg.mL.kg
fm <- lm(log10(y)~log10(x))
ff <- summary(fm)

rCLp_H_RLM_ml.min.kgvsKineticIV.Clearance..mL.min.kg. <- ggplot(data = ADMETRLM, mapping = aes(x = x, y = y))+
  geom_point(mapping = aes(y=ADMETRLM$rCLp_H_RLM_mg.mL.kg,x=ADMETRLM$KineticIV.Clearance..mL.min.kg.), colour = "#529EFF")+
  scale_y_log10(limits = c(0.1,210), labels = comma)+
  scale_x_log10(limits = c(0.1,210), labels = comma)+
  geom_abline(intercept = 0, slope = 1)+
  stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
  stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
  geom_smooth(method=lm)+
  geom_point(aes(colour=Class.List..user., group=Class.List..user.))+

  theme_classic()+
  labs(
    title = "pred_CLp_H_RLM vs observed_Clp",
    subtitle = "",
    x = "observed_Clp(mL/min/kg)",
    y = "pred_CLp_H_RLM (mL/min/kg)")

rCLp_H_RLM_ml.min.kgvsKineticIV.Clearance..mL.min.kg.
ggsave(plot = rCLp_H_RLM_ml.min.kgvsKineticIV.Clearance..mL.min.kg., file = "rCLp_H_RLM_ml.min.kgvsKineticIV.Clearance..mL.min.kg..png")

# Plot 5 AUChep

dd <- ADMETRH

#AUCinf

stat_function_01 <- function(x){
  2 * x
}
stat_function_02 <- function(x){
  (1/2) * x
}
stat_function_03 <- function(x){
  5 * x
}
stat_function_04 <- function(x){
  (1/5) * x
}

Class.List..user.<-ADMETRH$Class.List..user.

x <- ADMETRH$KineticIV.AUC..ng.h.mL.
y <- ADMETRH$AUC_rat.Dose_mg
fm <- lm(log10(y)~log10(x))
ff <- summary(fm)

pred_AUCinf_ratvsAUC_obs <- ggplot(data = ADMETRH, mapping = aes(x = x,y = y))+
  geom_point(mapping = aes(y=ADMETRH$AUC_rat.Dose_mg,x=ADMETRH$KineticIV.AUC..ng.h.mL.), colour = "#529EFF")+
  scale_y_log10(limits = c(1,40000), labels = comma)+
  scale_x_log10(limits = c(1,40000), labels = comma)+
  geom_abline(intercept = 0, slope = 1)+
  stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
  stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
  geom_smooth(method=lm)+
  
  geom_point(aes(colour=Class.List..user., group=Class.List..user.))+
  
  theme_classic()+
  labs(
    title = "pred_AUC_rat vs AUC_obs",
    subtitle = "",
    x = "AUC_obs(ng*h/mL)",
    y = "pred_AUC_rat (ng*h/mL)")

pred_AUCinf_ratvsAUC_obs
ggsave(plot = pred_AUCinf_ratvsAUC_obs, file = "pred_AUCinf_ratvsAUC_obs_chem.png")


stat_function_01 <- function(x){
  2 * x
}
stat_function_02 <- function(x){
  (1/2) * x
}
stat_function_03 <- function(x){
  5 * x
}
stat_function_04 <- function(x){
  (1/5) * x
}

Class.List..user.<-ADMETRLM$Class.List..user.

x <- ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RLM$KineticIV.AUC..ng.h.mL.
y <- ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RLM$AUC_rat.Dose_mg
fm <- lm(log10(y)~log10(x))
ff <- summary(fm)

pred_AUCinf_ratvsAUC_obs <- ggplot(data = ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RLM, mapping = aes(x = x,y = y))+
  geom_point(mapping = aes(y=ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RLM$AUC_rat.Dose_mg,x=ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RLM$KineticIV.AUC..ng.h.mL.), colour = "#529EFF")+
  scale_y_log10(limits = c(1,40000), labels = comma)+
  scale_x_log10(limits = c(1,40000), labels = comma)+
  geom_abline(intercept = 0, slope = 1)+
  stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
  stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
  geom_smooth(method=lm)+
  geom_point(aes(colour=Class.List..user., group=Class.List..user.))+
  
  theme_classic()+
  labs(
    title = "Simulation All In Silico with logP (RLM)",
    subtitle = "",
    x = "observed AUC [ng*h/mL]",
    y = "predicted AUC [ng*h/mL]")

pred_AUCinf_ratvsAUC_obs
ggsave(plot = pred_AUCinf_ratvsAUC_obs, file = "AUC_RLM_chem.png", width = 10, height = 10)



# Plot 6 CLphep vs rCLp_H

stat_function_01 <- function(x){
  2 * x
}
stat_function_02 <- function(x){
  (1/2) * x
}
stat_function_03 <- function(x){
  5 * x
}
stat_function_04 <- function(x){
  (1/5) * x
}

Class.List..user.<-ADMETRH$Class.List..user.

x <- ADMETRH$CLphep1
y <- ADMETRH$rCLp_H_mL.min.kg
fm <- lm(log10(y)~log10(x))
ff <- summary(fm)

rCLp_H_ml.min.kgvsCLphep1 <- ggplot(data = ADMETRH, mapping = aes(x = x,y = y))+
  geom_point(mapping = aes(y=ADMETRH$rCLp_H_mL.min.kg,x=ADMETRH$CLphep1), colour = "#529EFF")+
  scale_y_log10(limits = c(0.01,50), labels = comma)+
  scale_x_log10(limits = c(0.01,50), labels = comma)+
  geom_abline(intercept = 0, slope = 1)+
  stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
  stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
  geom_smooth(method=lm)+
  geom_point(aes(colour=Class.List..user., group=Class.List..user.))+
  
  theme_classic()+
  labs(
    title = "CLpredhep vs CL_IVIVE_hep",
    subtitle = "",
    x = "CL_IVIVE_hep(mL/min/kg)",
    y = "CLpredhep (mL/min/kg)")

rCLp_H_ml.min.kgvsCLphep1
ggsave(plot = rCLp_H_ml.min.kgvsCLphep1, file = "rCLp_H_ml.min.kgvsCLphep1.png")

# Plot 6.1 Clp,micros vs rCL_H_RLM

stat_function_01 <- function(x){
  2 * x
}
stat_function_02 <- function(x){
  (1/2) * x
}
stat_function_03 <- function(x){
  5 * x
}
stat_function_04 <- function(x){
  (1/5) * x
}

Class.List..user.<-ADMETRLM$Class.List..user.

x <- ADMETRLM$CLp.micros1
y <- ADMETRLM$rCLp_H_RLM_mg.mL.kg
fm <- lm(log10(y)~log10(x))
ff <- summary(fm)

rCLp_H_RLM_ml.min.kgvsCLp.micros1 <- ggplot(data = ADMETRLM, mapping = aes(x = x,y = y))+
  geom_point(mapping = aes(y=ADMETRLM$rCLp_H_RLM_mg.mL.kg,x=ADMETRLM$CLp.micros1), colour = "#529EFF")+
  scale_y_log10(limits = c(0.01,22), labels = comma)+
  scale_x_log10(limits = c(0.01,22), labels = comma)+
  geom_abline(intercept = 0, slope = 1)+
  stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
  stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
  geom_smooth(method=lm)+
  geom_point(aes(colour=Class.List..user., group=Class.List..user.))+
  
  theme_classic()+
  labs(
    title = "CLpredmic vs CL_IVIVE_mic",
    subtitle = "",
    x = "CL_IVIVE_mic (mL/min/kg)",
    y = "CLpredmic (mL/min/kg)")

rCLp_H_RLM_ml.min.kgvsCLp.micros1
ggsave(plot = rCLp_H_RLM_ml.min.kgvsCLp.micros1, file = "rCLp_H_RLM_ml.min.kgvsCLp.micros1.png")

# Plot 7 fu hep Austin (für H und RLM gleich)
#gecheckt kein < oder >

stat_function_01 <- function(x){
  2 * x
}
stat_function_02 <- function(x){
  (1/2) * x
}
stat_function_03 <- function(x){
  5 * x
}
stat_function_04 <- function(x){
  (1/5) * x
}

Class.List..user.<-ADMETRH$Class.List..user.

x <- ADMETRH$Hepatocyte.binding.fu.hep......conc..1./100
y <- ADMETRH$fu_hep_Austin
fm <- lm(log10(y)~log10(x))
ff <- summary(fm)

fu_hep_Austin.vsHepatocyte.binding.fu.hep......conc..1. <- ggplot(data = ADMETRH, mapping = aes(x = x,y = y))+
  geom_point(mapping = aes(y=ADMETRH$fu_hep_Austin,x=ADMETRH$Hepatocyte.binding.fu.hep......conc..1./100), colour = "#529EFF")+
  scale_y_log10(limits = c(0.01,1), labels = comma)+
  scale_x_log10(limits = c(0.01,1), labels = comma)+
  geom_abline(intercept = 0, slope = 1)+
  stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
  stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
  geom_smooth(method=lm)+
  
  geom_point(aes(colour=Class.List..user., group=Class.List..user.))+
  
  theme_classic()+
  labs(
    title = "fu_hep_Austin. vs obs_fuhep_conc.1",
    subtitle = "",
    x = "obs_fuhep_conc.1",
    y = "fu_hep_Austin. ")

fu_hep_Austin.vsHepatocyte.binding.fu.hep......conc..1.
ggsave(plot = fu_hep_Austin.vsHepatocyte.binding.fu.hep......conc..1., file = "fu_hep_Austin.vsHepatocyte.binding.fu.hep.conc.1_chem.png")

# Plot 8 fu mic 0.5 (für H und RLM gleich)
#gecheckt kein < oder >

stat_function_01 <- function(x){
  2 * x
}
stat_function_02 <- function(x){
  (1/2) * x
}
stat_function_03 <- function(x){
  5 * x
}
stat_function_04 <- function(x){
  (1/5) * x
}

Class.List..user.<-ADMETRH$Class.List..user.

x <- ADMETRH$Microsomal.binding.fu.mic......comp.conc.1..conc.HLM.0.5./100
y <- ADMETRH$fumic_half_mg_ml_conc
fm <- lm(log10(y)~log10(x))
ff <- summary(fm)

fumic_half_mg_ml_conc.vsMicrosomal.binding.fu.mic......comp.conc.1..conc.HLM.0.5. <- ggplot(data = ADMETRH, mapping = aes(x = x,y = y))+
  geom_point(mapping = aes(y=ADMETRH$fumic_half_mg_ml_conc,x=ADMETRH$Microsomal.binding.fu.mic......comp.conc.1..conc.HLM.0.5./100), colour = "#529EFF")+
  scale_y_log10(limits = c(0.01,1), labels = comma)+
  scale_x_log10(limits = c(0.01,1), labels = comma)+
  geom_abline(intercept = 0, slope = 1)+
  stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
  stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
  geom_smooth(method=lm)+
  
  geom_point(aes(colour=Class.List..user., group=Class.List..user.))+
  
  theme_classic()+
  labs(
    title = "pred_fumic_0.5conc vs fumic_comp.conc.1.conc.HLM.0.5.",
    subtitle = "",
    x = "fumic_comp.conc.1.conc.HLM.0.5.",
    y = "pred_fumic_0.5conc")

fumic_half_mg_ml_conc.vsMicrosomal.binding.fu.mic......comp.conc.1..conc.HLM.0.5.
ggsave(plot = fumic_half_mg_ml_conc.vsMicrosomal.binding.fu.mic......comp.conc.1..conc.HLM.0.5., file = "fumic_half_mg_ml_conc.vsMicrosomal.binding.fu.mic......comp.conc.1..conc.HLM.0.5._chem.png")

# Plot 9 rat_fup (für H und RLM gleich)
#hat<> 0.1 darin!!!

stat_function_01 <- function(x){
  2 * x
}
stat_function_02 <- function(x){
  (1/2) * x
}
stat_function_03 <- function(x){
  5 * x
}
stat_function_04 <- function(x){
  (1/5) * x
}

Class.List..user.<-ADMETRH$Class.List..user.

x <- ADMETRH$PlasmaProteinBinding.Concentration..uM....1/100
y <- ADMETRH$rat_fup./100

rat_fup.vsPlasmaProteinBinding.Concentration..uM....1 <- ggplot(data = ADMETRH, mapping = aes(x = x,y = y))+
  geom_point(mapping = aes(y=ADMETRH$rat_fup./100,x=ADMETRH$PlasmaProteinBinding.Concentration..uM....1/100), colour = "#529EFF")+
  scale_y_log10(limits = c(0.001,1), labels = comma)+
  scale_x_log10(limits = c(0.001,1), labels = comma)+
  geom_abline(intercept = 0, slope = 1)+
  stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
  stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
  geom_smooth(method=lm)+
  
  geom_point(aes(colour=Class.List..user., group=Class.List..user.))+
  
  theme_classic()+
  labs(
    title = "pred_rat_fup vs observed_fup_conc.1",
    subtitle = "",
    x = "observed_fup_conc.1",
    y = "pred_rat_fup")

rat_fup.vsPlasmaProteinBinding.Concentration..uM....1
ggsave(plot = rat_fup.vsPlasmaProteinBinding.Concentration..uM....1, file = "rat_fup.vsPlasmaProteinBinding.Concentration..uM....1._chem.png")

# Plot 10 RBP_rat vs measured (für H und RLM gleich)

stat_function_01 <- function(x){
  2 * x
}
stat_function_02 <- function(x){
  (1/2) * x
}
stat_function_03 <- function(x){
  5 * x
}
stat_function_04 <- function(x){
  (1/5) * x
}

Class.List..user.<-ADMETRH$Class.List..user.

x <- ADMETRH$Blood.Plasma
y <- ADMETRH$RBP_rat
fm <- lm(log10(y)~log10(x))
ff <- summary(fm)

RBP_ratvsBlood.Plasma <- ggplot(data = ADMETRH, mapping = aes(x = x,y = y))+
  geom_point(mapping = aes(y=ADMETRH$RBP_rat,x=ADMETRH$Blood.Plasma), colour = "#529EFF")+
  scale_y_log10(limits = c(0.1,25), labels = comma)+
  scale_x_log10(limits = c(0.1,25), labels = comma)+
  geom_abline(intercept = 0, slope = 1)+
  stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
  stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
  geom_smooth(method=lm)+
  
  geom_point(aes(colour=Class.List..user., group=Class.List..user.))+
  
  theme_classic()+
  labs(
    title = "pred_blood-to-plasma ratio vs blood-to-plasma ratio_chem",
    subtitle = "",
    x = "blood-to-plasma ratio",
    y = "pred_blood-to-plasma ratio")

RBP_ratvsBlood.Plasma
ggsave(plot = RBP_ratvsBlood.Plasma, file = "RBP_ratvsBlood.Plasma.png")

# Plot 11 t1/2_hepatocytes vs measured

stat_function_01 <- function(x){
  2 * x
}
stat_function_02 <- function(x){
  (1/2) * x
}
stat_function_03 <- function(x){
  5 * x
}
stat_function_04 <- function(x){
  (1/5) * x
}

Class.List..user.<-ADMETRH$Class.List..user.

x <- ADMETRH$KineticIV.Thalf..h.
y <- ADMETRH$THalf_rat.Dose_mg
fm <- lm(log10(y)~log10(x))
ff <- summary(fm)

THalf_rat.Dose_mgvsKineticIV.Thalf..h. <- ggplot(data = ADMETRH, mapping = aes(x = x,y = y))+
  geom_point(mapping = aes(y=ADMETRH$THalf_rat.Dose_mg,x=ADMETRH$KineticIV.Thalf..h.), colour = "#529EFF")+
  scale_y_log10(limits = c(0.1,100), labels = comma)+
  scale_x_log10(limits = c(0.1,100), labels = comma)+
  geom_abline(intercept = 0, slope = 1)+
  stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
  stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
  geom_smooth(method=lm)+
  
  geom_point(aes(colour=Class.List..user., group=Class.List..user.))+
  
  theme_classic()+
  labs(
    title = "THalf_rat.Dose_mg vs KineticIV.Thalf..h.",
    subtitle = "",
    x = "KineticIV.Thalf..h. (h)",
    y = "THalf_rat.Dose_mg (h)")

THalf_rat.Dose_mgvsKineticIV.Thalf..h.
ggsave(plot = THalf_rat.Dose_mgvsKineticIV.Thalf..h., file = "THalf_rat.Dose_mgvsKineticIV.Thalf..h._chem.png")

# Plot 11.1 t1/2_microsomes vs measured

stat_function_01 <- function(x){
  2 * x
}
stat_function_02 <- function(x){
  (1/2) * x
}
stat_function_03 <- function(x){
  5 * x
}
stat_function_04 <- function(x){
  (1/5) * x
}

Class.List..user.<-ADMETRLM$Class.List..user.

x <- ADMETRLM$KineticIV.Thalf..h.
y <- ADMETRLM$THalf_rat.Dose_mg
fm <- lm(log10(y)~log10(x))
ff <- summary(fm)

THalf_rat.Dose_mgvsKineticIV.Thalf..h._RLM <- ggplot(data = ADMETRLM, mapping = aes(x = x,y = y))+
  geom_point(mapping = aes(y=ADMETRLM$THalf_rat.Dose_mg,x=ADMETRLM$KineticIV.Thalf..h.), colour = "#529EFF")+
  scale_y_log10(limits = c(0.1,100), labels = comma)+
  scale_x_log10(limits = c(0.1,100), labels = comma)+
  geom_abline(intercept = 0, slope = 1)+
  stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
  stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
  geom_smooth(method=lm)+
  
  geom_point(aes(colour=Class.List..user., group=Class.List..user.))+
  
  theme_classic()+
  labs(
    title = "THalf_rat.Dose_mg vs KineticIV.Thalf_RLM_chem",
    subtitle = "",
    x = "KineticIV.Thalf..h. (h)",
    y = "THalf_rat.Dose_mg (h)")

THalf_rat.Dose_mgvsKineticIV.Thalf..h._RLM
ggsave(plot = THalf_rat.Dose_mgvsKineticIV.Thalf..h._RLM, file = "THalf_rat.Dose_mgvsKineticIV.Thalf..h._RLM.png")

# Plot 12 Vd (gleiche bei beiden)

stat_function_01 <- function(x){
  2 * x
}
stat_function_02 <- function(x){
  (1/2) * x
}
stat_function_03 <- function(x){
  5 * x
}
stat_function_04 <- function(x){
  (1/5) * x
}

Class.List..user.<-ADMETRH$Class.List..user.

x <- ADMETRH$KineticIV.Vss..L.kg.
y <- ADMETRH$Vd
fm <- lm(log10(y)~log10(x))
ff <- summary(fm)

VdvsKineticIV.Vss..L.kg. <- ggplot(data = ADMETRH, mapping = aes(x = x,y = y))+
  geom_point(mapping = aes(y=ADMETRH$Vd,x=ADMETRH$KineticIV.Vss..L.kg.), colour = "#529EFF")+
  scale_y_log10(limits = c(0.1,100), labels = comma)+
  scale_x_log10(limits = c(0.1,100), labels = comma)+
  geom_abline(intercept = 0, slope = 1)+
  stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
  stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
  geom_smooth(method=lm)+
  
  geom_point(aes(colour=Class.List..user., group=Class.List..user.))+
  
  theme_classic()+
  labs(
    title = "Vss vs pred_Vd",
    subtitle = "",
    x = "Vss (L/kg)",
    y = "pred_Vd (L/kg)")

VdvsKineticIV.Vss..L.kg.
ggsave(plot = VdvsKineticIV.Vss..L.kg., file = "VdvsKineticIV.Vss..L.kg..png")

# Plot 13 S.rVd_PBPK (gleiche bei beiden)

stat_function_01 <- function(x){
  2 * x
}
stat_function_02 <- function(x){
  (1/2) * x
}
stat_function_03 <- function(x){
  5 * x
}
stat_function_04 <- function(x){
  (1/5) * x
}

Class.List..user.<-ADMETRH$Class.List..user.

x <- ADMETRH$KineticIV.Vss..L.kg.
y <- ADMETRH$S.rVd_PBPK
fm <- lm(log10(y)~log10(x))
ff <- summary(fm)

S.rVd_PBPKvsKineticIV.Vss..L.kg. <- ggplot(data = ADMETRH, mapping = aes(x = x,y = y))+
  geom_point(mapping = aes(y=ADMETRH$S.rVd_PBPK,x=ADMETRH$KineticIV.Vss..L.kg.), colour = "#529EFF")+
  scale_y_log10(limits = c(0.1,100), labels = comma)+
  scale_x_log10(limits = c(0.1,100), labels = comma)+
  geom_abline(intercept = 0, slope = 1)+
  stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
  stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
  geom_smooth(method=lm)+

    geom_point(aes(colour=Class.List..user., group=Class.List..user.))+
  
  theme_classic()+
  labs(
    title = "pred_Vd_PBPK vs Vss",
    subtitle = "",
    x = "Vss (L/kg)",
    y = "pred_Vd_PBPK (L/kg)")

S.rVd_PBPKvsKineticIV.Vss..L.kg.
ggsave(plot = S.rVd_PBPKvsKineticIV.Vss..L.kg., file = "S.rVd_PBPKvsKineticIV.Vss..L.kg._chem.png")

# Plot 14 pKa vs pred

# Plot 15 logD vs pred

stat_function_01 <- function(x){
  2 * x
}
stat_function_02 <- function(x){
  (1/2) * x
}
stat_function_03 <- function(x){
  5 * x
}
stat_function_04 <- function(x){
  (1/5) * x
}

Class.List..user.<-ADMETRH$Class.List..user.

x <- ADMETRH$logD.logD..RT.
y <- ADMETRH$S.logD
fm <- lm(log10(y)~log10(x))
ff <- summary(fm)

S.logDvslogD.logD..RT. <- ggplot(data = ADMETRH, mapping = aes(x = x,y = y))+
  geom_point(mapping = aes(y=ADMETRH$S.logD,x=ADMETRH$logD.logD..RT.), colour = "#529EFF")+
  scale_y_log10(limits = c(2,5), labels = comma)+
  scale_x_log10(limits = c(2,5), labels = comma)+
  geom_abline(intercept = 0, slope = 1)+
  stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
  stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
  geom_smooth(method=lm)+
  
  geom_point(aes(colour=Class.List..user., group=Class.List..user.))+
  
  theme_classic()+
  labs(
    title = "logDpred vs logD",
    subtitle = "",
    x = "logD",
    y = "logDpred")

S.logDvslogD.logD..RT.
ggsave(plot = S.logDvslogD.logD..RT., file = "S.logDvslogD.logD..RT..png")

# Solubilities

dd <- ADMET74

# Plot 16 solFeSSIF

stat_function_01 <- function(x){
  2 * x
}
stat_function_02 <- function(x){
  (1/2) * x
}
stat_function_03 <- function(x){
  5 * x
}
stat_function_04 <- function(x){
  (1/5) * x
}

Class.List..user.<-ADMETRH$Class.List..user.

x <- ADMETRH$solFeSSIF.solubility.mg.mL.
y <- ADMETRH$S.FeSSIF
fm <- lm(log10(y)~log10(x))
ff <- summary(fm)

solFeSSIF.solubilityvspred_solFeSSIF.solubility <- ggplot(data = ADMETRH, mapping = aes(x = x,y = y))+
  geom_point(mapping = aes(y=ADMETRH$S.FeSSIF,x=ADMETRH$solFeSSIF.solubility.mg.mL.), colour = "#529EFF")+
  scale_y_log10(limits = c(0.001,5), labels = comma)+
  scale_x_log10(limits = c(0.001,5), labels = comma)+
  geom_abline(intercept = 0, slope = 1)+
  stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
  stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
  geom_smooth(method=lm)+
  
  geom_point(aes(colour=Class.List..user., group=Class.List..user.))+
  
  theme_classic()+
  labs(
    title = "solFeSSIF.solubility vs pred_solFeSSIF.solubility",
    subtitle = "",
    x = "solFeSSIF.solubility (mg/mL)",
    y = "pred_solFeSSIF.solubility (mg/mL)")

solFeSSIF.solubilityvspred_solFeSSIF.solubility
ggsave(plot = solFeSSIF.solubilityvspred_solFeSSIF.solubility, file = "solFeSSIF.solubilityvspred_solFeSSIF.solubility.png")

# Plot 17 solFaSSIF

stat_function_01 <- function(x){
  2 * x
}
stat_function_02 <- function(x){
  (1/2) * x
}
stat_function_03 <- function(x){
  5 * x
}
stat_function_04 <- function(x){
  (1/5) * x
}

Class.List..user.<-ADMETRH$Class.List..user.

x <- ADMETRH$solFaSSIF.solubility.mg.mL.
y <- ADMETRH$S.FaSSIF
fm <- lm(log10(y)~log10(x))
ff <- summary(fm)

solFaSSIF.solubilityvspred_solFaSSIF.solubility <- ggplot(data = ADMETRH, mapping = aes(x = x,y = y))+
  geom_point(mapping = aes(y=ADMETRH$S.FaSSIF,x=ADMETRH$solFaSSIF.solubility.mg.mL.), colour = "#529EFF")+
  scale_y_log10(limits = c(0.001,5), labels = comma)+
  scale_x_log10(limits = c(0.001,5), labels = comma)+
  geom_abline(intercept = 0, slope = 1)+
  stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
  stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
  geom_smooth(method=lm)+
  geom_point(aes(colour=Class.List..user., group=Class.List..user.))+
  
  theme_classic()+
  labs(
    title = "solFaSSIF.solubility vs pred_solFaSSIF.solubility",
    subtitle = "",
    x = "solFaSSIF.solubility (mg/mL)",
    y = "pred_solFaSSIF.solubility (mg/mL)")

solFaSSIF.solubilityvspred_solFaSSIF.solubility
ggsave(plot = solFaSSIF.solubilityvspred_solFaSSIF.solubility, file = "solFaSSIF.solubilityvspred_solFaSSIF.solubility.png")

# Plot 18 solpH7.4

stat_function_01 <- function(x){
  2 * x
}
stat_function_02 <- function(x){
  (1/2) * x
}
stat_function_03 <- function(x){
  5 * x
}
stat_function_04 <- function(x){
  (1/5) * x
}

Class.List..user.<-ADMETRH$Class.List..user.

x <- ADMET7.4$solPBSsolubility.mg.mL.
y <- ADMET7.4$S.S_pH
fm <- lm(log10(y)~log10(x))
ff <- summary(fm)

solPBSsolubility.mg.mL.vsS.S_pH <- ggplot(data = ADMET7.4, mapping = aes(x = x,y = y))+
  geom_point(mapping = aes(y=ADMET7.4$S.S_pH,x=ADMET7.4$solPBSsolubility.mg.mL.), colour = "#529EFF")+
  scale_y_log10(limits = c(0.001,25), labels = comma)+
  scale_x_log10(limits = c(0.001,25), labels = comma)+
  geom_abline(intercept = 0, slope = 1)+
  stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
  stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
  geom_smooth(method=lm)+
  geom_point(aes(colour=Class.List..user., group=Class.List..user.))+
  
  theme_classic()+
  labs(
    title = "PBS_solubility vs pred_PBS_solubility",
    subtitle = "",
    x = "PBS_solubility (mg/mL)",
    y = "pred_PBS_solubility (mg/mL)")

solPBSsolubility.mg.mL.vsS.S_pH
ggsave(plot = solPBSsolubility.mg.mL.vsS.S_pH, file = "solPBSsolubility.mg.mL.vsS.S_pH.png")

# Plot 19 solHCl

stat_function_01 <- function(x){
  2 * x
}
stat_function_02 <- function(x){
  (1/2) * x
}
stat_function_03 <- function(x){
  5 * x
}
stat_function_04 <- function(x){
  (1/5) * x
}

Class.List..user.<-ADMETRH$Class.List..user.

x <- ADMETpH1$solHCLsolubility.mg.mL.
y <- ADMETpH1_2$S.S_pH
fm <- lm(log10(y)~log10(x))
ff <- summary(fm)

solHCLsolubility.mg.mL.vsS.S_pH <- ggplot(data = ADMETpH1_2, mapping = aes(x = x,y = y))+
  geom_point(mapping = aes(y=ADMETpH1_2$S.S_pH,x=ADMETpH1_2$solHCLsolubility.mg.mL.), colour = "#529EFF")+
  scale_y_log10(limits = c(0.001,232), labels = comma)+
  scale_x_log10(limits = c(0.001,232), labels = comma)+
  geom_abline(intercept = 0, slope = 1)+
  stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
  stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
  geom_smooth(method=lm)+
  
  geom_point(aes(colour=Class.List..user., group=Class.List..user.))+
  
  theme_classic()+
  labs(
    title = "HCl_solubility vs pred_HCl_solubility",
    subtitle = "",
    x = "solHCLsolubility.mg.mL. (mg/mL)",
    y = "S.S_pH (mg/mL)")

solHCLsolubility.mg.mL.vsS.S_pH
ggsave(plot = solHCLsolubility.mg.mL.vsS.S_pH, file = "solHCLsolubility.mg.mL.vsS.S_pH.png")

# Plot 20 CL_IVIVE fumic=1, fup=1 AB HIER DATEN MIT RH MERGEN! KINETIC IV UND FUMIC teilen und wenn 1 gibt überall gut

stat_function_01 <- function(x){
  2 * x
}
stat_function_02 <- function(x){
  (1/2) * x
}
stat_function_03 <- function(x){
  5 * x
}
stat_function_04 <- function(x){
  (1/5) * x
}

Class.List..user.<-ADMETRH$Class.List..user.

x <- IVIVE2$obs.CLp
y <- IVIVE2$CLp

plot_IVIVE2 <- ggplot(data = IVIVE2, mapping = aes(x = x,y = y))+
  geom_point(mapping = aes(y=IVIVE2$CLp,x=IVIVE2$obs.CLp), colour = "#529EFF")+
  scale_y_log10(limits = c(0.1,230), labels = comma)+
  scale_x_log10(limits = c(0.1,230), labels = comma)+
  geom_abline(intercept = 0, slope = 1)+
  stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
  stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
  geom_smooth(method=lm)+
  
  geom_point(aes(colour=Class.List..user., group=Class.List..user.))+
  
  theme_classic()+
  labs(
    title = "CLp_IVIVE_fumic_fup_1 vs CL_pobs",
    subtitle = "",
    x = "CLp_obs (mL/min/kg)",
    y = "CLp_IVIVE (mL/min/kg)")

plot_IVIVE2
ggsave(plot = plot_IVIVE2, file = "plot_IVIVE2.png")

# Plot 21 CL_IVIVE fuhep=1, fup=1

stat_function_01 <- function(x){
  2 * x
}
stat_function_02 <- function(x){
  (1/2) * x
}
stat_function_03 <- function(x){
  5 * x
}
stat_function_04 <- function(x){
  (1/5) * x
}

Class.List..user.<-ADMETRH$Class.List..user.

x <- IVIVE2_hep$obs.CLp
y <- IVIVE2_hep$CLp

plot_IVIVE2_hep <- ggplot(data = IVIVE2_hep, mapping = aes(x = x,y = y))+
  geom_point(mapping = aes(y=IVIVE2_hep$CLp,x=IVIVE2_hep$obs.CLp), colour = "#529EFF")+
  scale_y_log10(limits = c(0.1,230), labels = comma)+
  scale_x_log10(limits = c(0.1,230), labels = comma)+
  geom_abline(intercept = 0, slope = 1)+
  stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
  stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
  geom_smooth(method=lm)+
  
  geom_point(aes(colour=Class.List..user., group=Class.List..user.))+
  
  theme_classic()+
  labs(
    title = "CLp_IVIVE_fuhep_fup_1 vs CLp_obs",
    subtitle = "",
    x = "CLp_obs (mL/min/kg)",
    y = "CLp_IVIVE (mL/min/kg)")

plot_IVIVE2_hep
ggsave(plot = plot_IVIVE2_hep, file = "plot_IVIVE2_hep.png")

# Plot 22 CL_IVIVE fumic=ADMETP fup=obs

stat_function_01 <- function(x){
  2 * x
}
stat_function_02 <- function(x){
  (1/2) * x
}
stat_function_03 <- function(x){
  5 * x
}
stat_function_04 <- function(x){
  (1/5) * x
}

Class.List..user.<-ADMETRH$Class.List..user.

x <- IVIVE3$obs.CLp
y <- IVIVE3$CLp

plot_IVIVE3 <- ggplot(data = IVIVE3, mapping = aes(x = x,y = y))+
  geom_point(mapping = aes(y=IVIVE3$CLp,x=IVIVE3$obs.CLp), colour = "#529EFF")+
  scale_y_log10(limits = c(0.01,230), labels = comma)+
  scale_x_log10(limits = c(0.01,230), labels = comma)+
  geom_abline(intercept = 0, slope = 1)+
  stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
  stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
  geom_smooth(method=lm)+
  
  geom_point(aes(colour=Class.List..user., group=Class.List..user.))+
  
  theme_classic()+
  labs(
    title = "CLp_IVIVE_pred_fumic_measured_fup vs CLp_obs",
    subtitle = "",
    x = "CLp_obs (mL/min/kg)",
    y = "CLp_IVIVE (mL/min/kg)")

plot_IVIVE3
ggsave(plot = plot_IVIVE3, file = "plot_IVIVE3.png")


# Plot 23 CL_IVIVE fuhep=Dilution method fup=obs BIS HIER EINFACH CLASSS VON RH KOPIEREN

stat_function_01 <- function(x){
  2 * x
}
stat_function_02 <- function(x){
  (1/2) * x
}
stat_function_03 <- function(x){
  5 * x
}
stat_function_04 <- function(x){
  (1/5) * x
}

Class.List..user.<-ADMETRH$Class.List..user.

x <- IVIVE3_hep$obs.CLp
y <- IVIVE3_hep$CLp
fm <- lm(log10(y)~log10(x))
ff <- summary(fm)

plot_IVIVE3_hep <- ggplot(data = IVIVE3_hep, mapping = aes(x = x,y = y))+
  geom_point(mapping = aes(y=IVIVE3_hep$CLp,x=IVIVE3_hep$obs.CLp), colour = "#529EFF")+
  scale_y_log10(limits = c(0.1,230), labels = comma)+
  scale_x_log10(limits = c(0.1,230), labels = comma)+
  geom_abline(intercept = 0, slope = 1)+
  stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
  stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
  geom_smooth(method=lm)+
  
  geom_point(aes(colour=Class.List..user., group=Class.List..user.))+
  
  theme_classic()+
  labs(
    title = "CLp_IVIVE_dilut_fuhep_measured_fup vs CLp_obs",
    subtitle = "",
    x = "CLp_obs (mL/min/kg)",
    y = "CLp_IVIVE (mL/min/kg)")

plot_IVIVE3_hep
ggsave(plot = plot_IVIVE3_hep, file = "plot_IVIVE3_hep.png")

# Plot 24 Vss NACH CHEM CLASS CHECKEN UND DANN CLASS KOPIEREN

stat_function_01 <- function(x){
  2 * x
}
stat_function_02 <- function(x){
  (1/2) * x
}
stat_function_03 <- function(x){
  5 * x
}
stat_function_04 <- function(x){
  (1/5) * x
}

Class.List..user.<-optim_RLM_chem_class$Class.List..user.

x <- optim_RLM_chem_class$KineticIV.Vss..L.kg.
y <- optim_RLM_chem_class$S.rVd_PBPK

Vss1 <- ggplot(data = optim_RLM_chem_class, mapping = aes(x = x,y = y))+
  geom_point(mapping = aes(y=optim_RLM_chem_class$S.rVd_PBPK,x=optim_RLM_chem_class$KineticIV.Vss..L.kg.), colour = "#529EFF")+
  scale_y_log10(limits = c(0.1,12), labels = comma)+
  scale_x_log10(limits = c(0.1,12), labels = comma)+
  geom_abline(intercept = 0, slope = 1)+
  stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
  stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
  geom_smooth(method=lm)+
  
  geom_point(aes(colour=Class.List..user., group=Class.List..user.))+
  
  theme_classic()+
  labs(
    title = "Vd_pred_input_fup_RBPrat vs Vss_obs",
    subtitle = "",
    x = "Vss_obs (L/kg)",
    y = "Vd_pred_input_fup_RBPrat (L/kg)")

Vss1
ggsave(plot = Vss1, file = "Vss1_chem.png")

# Plot 25 Vss2 REIHENFOLGE ANDERS ALS BEI RH VLT NICHT ALS BEI OPTIM...

stat_function_01 <- function(x){
  2 * x
}
stat_function_02 <- function(x){
  (1/2) * x
}
stat_function_03 <- function(x){
  5 * x
}
stat_function_04 <- function(x){
  (1/5) * x
}

Class.List..user.<-only_fup_input_RLM_chem_class$Class.List..user.

x <- only_fup_input_RLM_chem_class$KineticIV.Vss..L.kg.
y <- only_fup_input_RLM_chem_class$S.rVd_PBPK


Vss2 <- ggplot(data = only_fup_input_RLM_chem_class, mapping = aes(x = x,y = y))+
  geom_point(mapping = aes(y=only_fup_input_RLM_chem_class$S.rVd_PBPK,x=only_fup_input_RLM_chem_class$KineticIV.Vss..L.kg.), colour = "#529EFF")+
  scale_y_log10(limits = c(0.1,12), labels = comma)+
  scale_x_log10(limits = c(0.1,12), labels = comma)+
  geom_abline(intercept = 0, slope = 1)+
  stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
  stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
  geom_smooth(method=lm)+
  geom_point(aes(colour=Class.List..user., group=Class.List..user.))+
  
  theme_classic()+
  labs(
    title = "Vd_pred_input_fup vs Vss_obs",
    subtitle = "",
    x = "Vss_obs (L/kg)",
    y = "Vd_pred_input_fup (L/kg)")

Vss2
ggsave(plot = Vss2, file = "Vss2_chem.png")


# Plot 25 Vss3 gleiche reihenfolge wie Vss2

stat_function_01 <- function(x){
  2 * x
}
stat_function_02 <- function(x){
  (1/2) * x
}
stat_function_03 <- function(x){
  5 * x
}
stat_function_04 <- function(x){
  (1/5) * x
}

Class.List..user.<-only_RBP_input_RLM_chem_class$Class.List..user.

x <- only_RBP_input_RLM_chem_class$KineticIV.Vss..L.kg.
y <- only_RBP_input_RLM_chem_class$S.rVd_PBPK


Vss3 <- ggplot(data = only_RBP_input_RLM_chem_class, mapping = aes(x = x,y = y))+
  geom_point(mapping = aes(y=only_RBP_input_RLM_chem_class$S.rVd_PBPK,x=only_RBP_input_RLM_chem_class$KineticIV.Vss..L.kg.), colour = "#529EFF")+
  scale_y_log10(limits = c(0.1,12), labels = comma)+
  scale_x_log10(limits = c(0.1,12), labels = comma)+
  geom_abline(intercept = 0, slope = 1)+
  stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
  stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
  geom_smooth(method=lm)+
  geom_point(aes(colour=Class.List..user., group=Class.List..user.))+
  
  theme_classic()+
  labs(
    title = "Vd_pred_input_RBP vs Vss_obs",
    subtitle = "",
    x = "Vss_obs (L/kg)",
    y = "Vd_pred_input_RBPrat (L/kg)")

Vss3
ggsave(plot = Vss3, file = "Vss3_chem.png")

# Plot 26 Vss mit logD und fup RBP NOCH MACHEN

stat_function_01 <- function(x){
  2 * x
}
stat_function_02 <- function(x){
  (1/2) * x
}
stat_function_03 <- function(x){
  5 * x
}
stat_function_04 <- function(x){
  (1/5) * x
}

Class.List..user.<-VssmitlogD2$Class.List

x <- VssmitlogD2$KineticIV.Vss..L.kg.
y <- VssmitlogD2$S.rVd_PBPK

Vss_logD_fup_BP <- ggplot(data = VssmitlogD2, mapping = aes(x = x,y = y))+
  geom_point(mapping = aes(y=VssmitlogD2$S.rVd_PBPK,x=VssmitlogD2$KineticIV.Vss..L.kg.), colour = "#529EFF")+
  scale_y_log10(limits = c(0.1,12), labels = comma)+
  scale_x_log10(limits = c(0.1,12), labels = comma)+
  geom_abline(intercept = 0, slope = 1)+
  stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
  stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
  geom_smooth(method=lm)+
  geom_point(aes(colour=Class.List, group=Class.List))+
  
  theme_classic()+
  labs(
    title = "",
    subtitle = "",
    x = "Vss_obs (L/kg)",
    y = "Vd_pred_logD_fup_BP (L/kg)")

Vss_logD_fup_BP
ggsave(plot = Vss_logD_fup_BP, file = "Vss_logD_fup_BP_chem.png", width = 10, height = 10)



# Plot 20 CLintu_obs vs pred_CL_intu_RLM

stat_function_01 <- function(x){
  2 * x
}
stat_function_02 <- function(x){
  (1/2) * x
}
stat_function_03 <- function(x){
  5 * x
}
stat_function_04 <- function(x){
  (1/5) * x
}

Class.List <- ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$Class.List..user.

x <- ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$Clintu_mic_obs
y <- ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$CYP_RLM_Clintu

CLintuvspred_CL_intu_RLM <- ggplot(data = ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH, mapping = aes(x = x,y = y))+
  geom_point(mapping = aes(y=ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$CYP_RLM_Clintu,x=ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$Clintu_mic_obs), colour = "#529EFF")+
  scale_y_log10(limits = c(1,13000), labels = comma)+
  scale_x_log10(limits = c(1,13000), labels = comma)+
  geom_abline(intercept = 0, slope = 1)+
  stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
  stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
  geom_smooth(method=lm)+
  geom_point(aes(colour=Class.List, group=Class.List))+
  
  theme_classic()+
  labs(
    title = "CLintu_obs vs pred_CL_intu_RLM",
    subtitle = "",
    x = "CLintu_obs (mL/min/kg)",
    y = "pred_CL_intu_RLM (mL/min/kg)")

CLintuvspred_CL_intu_RLM
ggsave(plot = CLintuvspred_CL_intu_RLM, file = "CLintuvspred_CL_intu_RLM_chem.png", width = 10, height = 10)


# Plot 21 CLintu_obs vs pred_CL_intu_H

stat_function_01 <- function(x){
  2 * x
}
stat_function_02 <- function(x){
  (1/2) * x
}
stat_function_03 <- function(x){
  5 * x
}
stat_function_04 <- function(x){
  (1/5) * x
}

Class.List <- ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$Class.List..user.

x <- ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$Clintu_hep_obs
y <- ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$HEP_rCLintu

CLintuvspred_CL_intu_H <- ggplot(data = ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH, mapping = aes(x = x,y = y))+
  geom_point(mapping = aes(y=ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$HEP_rCLintu,x=ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$Clintu_hep_obs), colour = "#529EFF")+
  scale_y_log10(limits = c(1,5000), labels = comma)+
  scale_x_log10(limits = c(1,5000), labels = comma)+
  geom_abline(intercept = 0, slope = 1)+
  stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
  stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
  geom_smooth(method=lm)+
  geom_point(aes(colour=Class.List, group=Class.List))+
  
  theme_classic()+
  labs(
    title = "CLintu_obs vs pred_CL_intu_H",
    subtitle = "",
    x = "CLintu_obs (mL/min/kg)",
    y = "pred_CL_intu_H (mL/min/kg)")

CLintuvspred_CL_intu_H
ggsave(plot = CLintuvspred_CL_intu_H, file = "CLintuvspred_CL_intu_H_chem.png", width = 10, height = 10)

# Plot 20 CLintu_obs vs pred_CL_intu_RLM CALCULATED WITH obs.fumic

stat_function_01 <- function(x){
  2 * x
}
stat_function_02 <- function(x){
  (1/2) * x
}
stat_function_03 <- function(x){
  5 * x
}
stat_function_04 <- function(x){
  (1/5) * x
}
Class.List <- ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$Class.List..user.

x <- ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$Clintu_mic_obs
y <- ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$pred_Clint_obsu_mic

CLintuvspred_CL_intu_obsu_RLM <- ggplot(data = ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH, mapping = aes(x = x,y = y))+
  geom_point(mapping = aes(y=ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$pred_Clint_obsu_mic,x=ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$Clintu_mic_obs), colour = "#529EFF")+
  scale_y_log10(limits = c(1,13000), labels = comma)+
  scale_x_log10(limits = c(1,13000), labels = comma)+
  geom_abline(intercept = 0, slope = 1)+
  stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
  stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
  geom_smooth(method=lm)+
  geom_point(aes(colour=Class.List, group=Class.List))+
  
  theme_classic()+
  labs(
    title = "CLintu_obs vs pred_CL_intu_obsu_RLM",
    subtitle = "",
    x = "CLintu_obs (mL/min/kg)",
    y = "pred_CL_intu_obsu_RLM (mL/min/kg)")

CLintuvspred_CL_intu_obsu_RLM
ggsave(plot = CLintuvspred_CL_intu_obsu_RLM, file = "CLintuvspred_CL_intu_obsu_RLM_chem.png", width = 10, height = 10)

# Plot 21 CLintu_obs vs pred_CL_intu_H CALCULATED WITH obs.fuhep

stat_function_01 <- function(x){
  2 * x
}
stat_function_02 <- function(x){
  (1/2) * x
}
stat_function_03 <- function(x){
  5 * x
}
stat_function_04 <- function(x){
  (1/5) * x
}
Class.List <- ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$Class.List..user.

x <- ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$Clintu_hep_obs
y <- ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$pred_Clint_obsu_hep

CLintuvspred_CL_intu_obsu_hep <- ggplot(data = ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH, mapping = aes(x = x,y = y))+
  geom_point(mapping = aes(y=ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$pred_Clint_obsu_hep,x=ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$Clintu_hep_obs), colour = "#529EFF")+
  scale_y_log10(limits = c(1,5000), labels = comma)+
  scale_x_log10(limits = c(1,5000), labels = comma)+
  geom_abline(intercept = 0, slope = 1)+
  stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
  stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
  geom_smooth(method=lm)+
  geom_point(aes(colour=Class.List, group=Class.List))+
  
  theme_classic()+
  labs(
    title = "CLintu_obs vs pred_CL_intu_obsu_hep",
    subtitle = "",
    x = "CLintu_obs (mL/min/kg)",
    y = "pred_CL_intu_obsu_RLM (mL/min/kg)")

CLintuvspred_CL_intu_obsu_hep
ggsave(plot = CLintuvspred_CL_intu_obsu_hep, file = "CLintuvspred_CL_intu_obsu_hep_chem.png", width = 10, height = 10)
