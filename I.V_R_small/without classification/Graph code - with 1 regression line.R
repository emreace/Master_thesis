install.packages("ggplot2")
library(ggplot2)
install.packages("scales")
library(scales)
install.packages("fun")
library(fun)
install.packages("tidyverse")
library(tidyverse)
library(ggpubr)

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

x <- `All_final_to_share_B2P%_7.4`$as.numeric(logD.logD)
y <- `All_final_to_share_B2P%_7.4`$S.logD

CLphep1vsKineticIV.Clearance..mL.min.kg._MFE <- ggplot(data = `All_final_to_share_B2P%_7.4`, mapping = aes(x = x,y = y))+
  geom_point(mapping = aes(`All_final_to_share_B2P%_7.4`$logD.logD,x=`All_final_to_share_B2P%_7.4`$S.logD), colour = "#529EFF")+
  scale_y_log10(limits = c(-10,10), labels = comma)+
  scale_x_log10(limits = c(-10,10), labels = comma)+
  geom_abline(intercept = 0, slope = 1)+
  stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
  stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
  geom_smooth(method=lm)+
  
  theme_classic()+
  labs(
    title = "IVIVE1_RH with Factor",
    subtitle = "",
    x = "observed CLp [mL/min/kg]",
    y = "predicted CLp * Factor [mL/min/kg]")+
     theme(text = element_text(size = 20))

CLphep1vsKineticIV.Clearance..mL.min.kg._MFE
ggsave(plot = CLphep1vsKineticIV.Clearance..mL.min.kg._MFE, file = "IVIVE_hep_MFE.png", width = 10, height = 10)

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

x <- `All_final_to_share_B2P%_7.4`$log
y <- ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$CLp.micros1*6.336172
fm <- lm(log10(y)~log10(x))
ff <- summary(fm)

CLp.micros1vsKineticIV.Clearance..mL.min.kg._MFE <- ggplot(data = ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH, mapping = aes(x = x,y = y))+
  geom_point(mapping = aes(y=ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$CLp.micros1*6.336172,x=ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$KineticIV.Clearance..mL.min.kg.), colour = "#529EFF")+
  scale_y_log10(limits = c(0.01,210), labels = comma)+
  scale_x_log10(limits = c(0.01,210), labels = comma)+
  geom_abline(intercept = 0, slope = 1)+
  stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
  stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
  geom_smooth(method=lm)+
  
  
  theme_classic()+
  labs(
    title = "IVIVE1_RLM with Factor",
    subtitle = "",
    x = "observed CLp [mL/min/kg]",
    y = "predicted CLp * Factor [mL/min/kg]")

CLp.micros1vsKineticIV.Clearance..mL.min.kg._MFE
ggsave(plot = CLp.micros1vsKineticIV.Clearance..mL.min.kg._MFE, file = "IVIVE1_RLM_MFE.png", width = 10, height = 10)

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

x <- ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$KineticIV.Clearance..mL.min.kg.
y <- ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$CLphep1


CLphep1vsKineticIV.Clearance..mL.min.kg._no_MFE <- ggplot(data = ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH, mapping = aes(x = x,y = y))+
  geom_point(mapping = aes(y=ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$CLphep1,x=ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$KineticIV.Clearance..mL.min.kg.), colour = "#529EFF")+
  scale_y_log10(limits = c(0.01,100), labels = comma)+
  scale_x_log10(limits = c(0.01,100), labels = comma)+
  geom_abline(intercept = 0, slope = 1)+
  stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
  stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
  geom_smooth(method=lm)+
  
  theme_classic()+
  labs(
    title = "IVIVE1_RH",
    subtitle = "",
    x = "observed CLp [mL/min/kg]",
    y = "predicted CLp [mL/min/kg]")

CLphep1vsKineticIV.Clearance..mL.min.kg._no_MFE
ggsave(plot = CLphep1vsKineticIV.Clearance..mL.min.kg._no_MFE, file = "IVIVE1_RH.png", width = 10, height = 10)

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

x <- ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$KineticIV.Clearance..mL.min.kg.
y <- ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$CLp.micros1
fm <- lm(log10(y)~log10(x))
ff <- summary(fm)

CLp.micros1vsKineticIV.Clearance..mL.min.kg._no_MFE <- ggplot(data = ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH, mapping = aes(x = x,y = y))+
  geom_point(mapping = aes(y=ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$CLp.micros1,x=ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$KineticIV.Clearance..mL.min.kg.), colour = "#529EFF")+
  scale_y_log10(limits = c(0.01,210), labels = comma)+
  scale_x_log10(limits = c(0.01,210), labels = comma)+
  geom_abline(intercept = 0, slope = 1)+
  stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
  stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
  geom_smooth(method=lm)+
  
  
  theme_classic()+
  labs(
    title = "IVIVE1_RLM",
    subtitle = "",
    x = "observed CLp [mL/min/kg]",
    y = "predicted CLp [mL/min/kg]")

CLp.micros1vsKineticIV.Clearance..mL.min.kg._no_MFE
ggsave(plot = CLp.micros1vsKineticIV.Clearance..mL.min.kg._no_MFE, file = "IVIVE1_RLM.png", width = 10, height = 10)


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

x <- ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$KineticIV.Clearance..mL.min.kg.
y <- ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$CL_tot_ML_min_kg
fm <- lm(log10(y)~log10(x))
ff <- summary(fm)

CL_totpred_HvsKineticIV.Clearance..mL.min.kg. <- ggplot(data = ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH, mapping = aes(x = x,y = y))+
  geom_point(mapping = aes(y=ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$CL_tot_ML_min_kg,x=ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$KineticIV.Clearance..mL.min.kg.), colour = "#529EFF")+
  scale_y_log10(limits = c(0.1,210), labels = comma)+
  scale_x_log10(limits = c(0.1,210), labels = comma)+
  geom_abline(intercept = 0, slope = 1)+
  stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
  stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
  geom_smooth(method=lm)+
  
  theme_classic()+
  labs(
    title = "Simulation All In Silico with logP (RH)",
    subtitle = "",
    x = "observed CLp [mL/min/kg]",
    y = "predicted CLp [mL/min/kg]")

CL_totpred_HvsKineticIV.Clearance..mL.min.kg.
ggsave(plot = CL_totpred_HvsKineticIV.Clearance..mL.min.kg., file = "Simulation All In Silico with logP (RH).png", width = 10, height = 10)


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


x <- ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RLM$KineticIV.Clearance..mL.min.kg.
y <- ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RLM$CL_total_pred
fm <- lm(log10(y)~log10(x))
ff <- summary(fm)

CLtotpred_RLMvsKineticIV.Clearance..mL.min.kg. <- ggplot(data = ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RLM, mapping = aes(x = x,y = y))+
  geom_point(mapping = aes(y=ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RLM$CL_total_pred,x=ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RLM$KineticIV.Clearance..mL.min.kg.), colour = "#529EFF")+
  scale_y_log10(limits = c(0.1,210), labels = comma)+
  scale_x_log10(limits = c(0.1,210), labels = comma)+
  geom_abline(intercept = 0, slope = 1)+
  stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
  stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
  geom_smooth(method=lm)+
  
  theme_classic()+
  labs(
    title = "Simulation All In Silico with logP (RLM)",
    subtitle = "",
    x = "observed CLp [mL/min/kg]",
    y = "predicted CLp [mL/min/kg]")

CLtotpred_RLMvsKineticIV.Clearance..mL.min.kg.
ggsave(plot = CLtotpred_RLMvsKineticIV.Clearance..mL.min.kg., file = "Simulation All In Silico with logP (RLM)Clearance.png", width = 10, height = 10)

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

x <- ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$KineticIV.Clearance..mL.min.kg.
y <- ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$rCLp_H_mL.min.kg
fm <- lm(log10(y)~log10(x))
ff <- summary(fm)

rCLp_H_ml.min.kgvsKineticIV.Clearance..mL.min.kg. <- ggplot(data = ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH, mapping = aes(x = x,y = y))+
  geom_point(mapping = aes(y=ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$rCLp_H_mL.min.kg,x=ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$KineticIV.Clearance..mL.min.kg.), colour = "#529EFF")+
  scale_y_log10(limits = c(0.1,210), labels = comma)+
  scale_x_log10(limits = c(0.1,210), labels = comma)+
  geom_abline(intercept = 0, slope = 1)+
  stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
  stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
  geom_smooth(method=lm)+
  
  theme_classic()+
  labs(
    title = "pred_CLp_H vs observed_CLp",
    subtitle = "",
    x = "observed_CLp (mL/min/kg)",
    y = "pred_CLp_H (mL/min/kg) (mL/min/kg)")

rCLp_H_ml.min.kgvsKineticIV.Clearance..mL.min.kg.
ggsave(plot = rCLp_H_ml.min.kgvsKineticIV.Clearance..mL.min.kg., file = "rCLp_H_ml.min.kgvsKineticIV.Clearance..mL.min.kg..png", width = 10, height = 10)

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

x <- ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RLM$KineticIV.Clearance..mL.min.kg.
y <- ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RLM$rCLp_H_RLM_mg.mL.kg
fm <- lm(log10(y)~log10(x))
ff <- summary(fm)

rCLp_H_RLM_ml.min.kgvsKineticIV.Clearance..mL.min.kg. <- ggplot(data = ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RLM, mapping = aes(x = x,y = y))+
  geom_point(mapping = aes(y=ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RLM$rCLp_H_RLM_mg.mL.kg,x=ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RLM$KineticIV.Clearance..mL.min.kg.), colour = "#529EFF")+
  scale_y_log10(limits = c(0.1,210), labels = comma)+
  scale_x_log10(limits = c(0.1,210), labels = comma)+
  geom_abline(intercept = 0, slope = 1)+
  stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
  stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
  geom_smooth(method=lm)+
  
  theme_classic()+
  labs(
    title = "pred_CLp_H_RLM vs observed_CLp",
    subtitle = "",
    x = "observed_CLp(mL/min/kg)",
    y = "pred_CLp_H_RLM (mL/min/kg)")

rCLp_H_RLM_ml.min.kgvsKineticIV.Clearance..mL.min.kg.
ggsave(plot = rCLp_H_RLM_ml.min.kgvsKineticIV.Clearance..mL.min.kg., file = "rCLp_H_RLM_ml.min.kgvsKineticIV.Clearance..mL.min.kg..png", width = 10, height = 10)

# Plot 5 AUChep

dd <- ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH

#AUC mic

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
  
  theme_classic()+
  labs(
    title = "Simulation All In Silico with logP (RLM)",
    subtitle = "",
    x = "observed AUC [ng*h/mL]",
    y = "predicted AUC [ng*h/mL]")

pred_AUCinf_ratvsAUC_obs
ggsave(plot = pred_AUCinf_ratvsAUC_obs, file = "AUCinf all in silico logP.png", width = 10, height = 10)

#AUC inf hep

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

x <- ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$KineticIV.AUC..ng.h.mL.
y <- ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$AUC_rat.Dose_mg
fm <- lm(log10(y)~log10(x))
ff <- summary(fm)

pred_AUCinf_ratvsAUC_obs_hep <- ggplot(data = ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH, mapping = aes(x = x,y = y))+
  geom_point(mapping = aes(y=ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$AUC_rat.Dose_mg,x=ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$KineticIV.AUC..ng.h.mL.), colour = "#529EFF")+
  scale_y_log10(limits = c(1,40000), labels = comma)+
  scale_x_log10(limits = c(1,40000), labels = comma)+
  geom_abline(intercept = 0, slope = 1)+
  stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
  stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
  geom_smooth(method=lm)+
  
  theme_classic()+
  labs(
    title = "Simulation All In Silico with logP (RH)",
    subtitle = "",
    x = "observed AUC [ng*h/mL]",
    y = "predicted AUC [ng*h/mL]")

pred_AUCinf_ratvsAUC_obs_hep
ggsave(plot = pred_AUCinf_ratvsAUC_obs_hep, file = "AUCinf all in silico logP(RH).png", width = 10, height = 10)


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

x <- ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$CLphep1
y <- ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$rCLp_H_mL.min.kg
fm <- lm(log10(y)~log10(x))
ff <- summary(fm)

rCLp_H_ml.min.kgvsCLphep1 <- ggplot(data = ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH, mapping = aes(x = x,y = y))+
  geom_point(mapping = aes(y=ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$rCLp_H_mL.min.kg,x=ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$CLphep1), colour = "#529EFF")+
  scale_y_log10(limits = c(0.01,50), labels = comma)+
  scale_x_log10(limits = c(0.01,50), labels = comma)+
  geom_abline(intercept = 0, slope = 1)+
  stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
  stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
  geom_smooth(method=lm)+
  
  theme_classic()+
  labs(
    title = "CLpredhep vs CL_IVIVE_hep",
    subtitle = "",
    x = "CL_IVIVE_hep(mL/min/kg)",
    y = "CLpredhep (mL/min/kg)")

rCLp_H_ml.min.kgvsCLphep1
ggsave(plot = rCLp_H_ml.min.kgvsCLphep1, file = "rCLp_H_ml.min.kgvsCLphep1.png", width = 10, height = 10)

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

x <- ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RLM$CLp.micros1
y <- ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RLM$rCLp_H_RLM_mg.mL.kg
fm <- lm(log10(y)~log10(x))
ff <- summary(fm)

rCLp_H_RLM_ml.min.kgvsCLp.micros1 <- ggplot(data = ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RLM, mapping = aes(x = x,y = y))+
  geom_point(mapping = aes(y=ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RLM$rCLp_H_RLM_mg.mL.kg,x=ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RLM$CLp.micros1), colour = "#529EFF")+
  scale_y_log10(limits = c(0.01,22), labels = comma)+
  scale_x_log10(limits = c(0.01,22), labels = comma)+
  geom_abline(intercept = 0, slope = 1)+
  stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
  stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
  geom_smooth(method=lm)+
  
  theme_classic()+
  labs(
    title = "CLpredmic vs CL_IVIVE_mic",
    subtitle = "",
    x = "CL_IVIVE_mic (mL/min/kg)",
    y = "CLpredmic (mL/min/kg)")

rCLp_H_RLM_ml.min.kgvsCLp.micros1
ggsave(plot = rCLp_H_RLM_ml.min.kgvsCLp.micros1, file = "rCLp_H_RLM_ml.min.kgvsCLp.micros1.png", width = 10, height = 10)

# Plot 7 fu hep Austin (für H und RLM gleich) AB HIER WEITER
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

x <- ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$fuhep_obs_fraction
y <- ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$fu_hep_Austin
fm <- lm(log10(y)~log10(x))
ff <- summary(fm)

fu_hep_Austin.vsHepatocyte.binding.fu.hep......conc..1. <- ggplot(data = ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH, mapping = aes(x = x,y = y))+
  geom_point(mapping = aes(y=ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$fu_hep_Austin,x=ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$fuhep_obs_fraction), colour = "#529EFF")+
  scale_y_log10(limits = c(0.01,1), labels = comma)+
  scale_x_log10(limits = c(0.01,1), labels = comma)+
  geom_abline(intercept = 0, slope = 1)+
  stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
  stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
  geom_smooth(method=lm)+
  
  theme_classic()+
  labs(
    title = "fuhep Austin",
    subtitle = "",
    x = "observed fuhep",
    y = "predicted fuhep Austin")

fu_hep_Austin.vsHepatocyte.binding.fu.hep......conc..1.
ggsave(plot = fu_hep_Austin.vsHepatocyte.binding.fu.hep......conc..1., file = "fuhep_Austin.png", width = 10, height = 10)

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

x <- ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$fumic_obs_fraction
y <- ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$fumic_half_mg_ml_conc
fm <- lm(log10(y)~log10(x))
ff <- summary(fm)

fumic_half_mg_ml_conc.vsMicrosomal.binding.fu.mic......comp.conc.1..conc.HLM.0.5. <- ggplot(data = ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH, mapping = aes(x = x,y = y))+
  geom_point(mapping = aes(y=ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$fumic_half_mg_ml_conc,x=ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$fumic_obs_fraction), colour = "#529EFF")+
  scale_y_log10(limits = c(0.01,1), labels = comma)+
  scale_x_log10(limits = c(0.01,1), labels = comma)+
  geom_abline(intercept = 0, slope = 1)+
  stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
  stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
  geom_smooth(method=lm)+
  
  theme_classic()+
  labs(
    title = "fumic_half",
    subtitle = "",
    x = "observed fumic_half",
    y = "predicted fumic_half")

fumic_half_mg_ml_conc.vsMicrosomal.binding.fu.mic......comp.conc.1..conc.HLM.0.5.
ggsave(plot = fumic_half_mg_ml_conc.vsMicrosomal.binding.fu.mic......comp.conc.1..conc.HLM.0.5., file = "fumic_half.png", width = 10, height = 10)

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

x <- ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$fup_obs_fraction
y <- ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$rat_fup./100
fm <- lm(log10(y)~log10(x))
ff <- summary(fm)

rat_fup.vsPlasmaProteinBinding.Concentration..uM....1 <- ggplot(data = ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH, mapping = aes(x = x,y = y))+
  geom_point(mapping = aes(y=ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$rat_fup./100,x=ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$fup_obs_fraction), colour = "#529EFF")+
  scale_y_log10(limits = c(0.001,1), labels = comma)+
  scale_x_log10(limits = c(0.001,1), labels = comma)+
  geom_abline(intercept = 0, slope = 1)+
  stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
  stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
  geom_smooth(method=lm)+
  
  theme_classic()+
  labs(
    title = "fup",
    subtitle = "",
    x = "observed fup",
    y = "predicted fup")

rat_fup.vsPlasmaProteinBinding.Concentration..uM....1
ggsave(plot = rat_fup.vsPlasmaProteinBinding.Concentration..uM....1, file = "rat_fup.png", width = 10, height = 10)

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

x <- ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$Blood.Plasma
y <- ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$RBP_rat
fm <- lm(log10(y)~log10(x))
ff <- summary(fm)

RBP_ratvsBlood.Plasma <- ggplot(data = ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH, mapping = aes(x = x,y = y))+
  geom_point(mapping = aes(y=ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$RBP_rat,x=ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$Blood.Plasma), colour = "#529EFF")+
  scale_y_log10(limits = c(0.1,25), labels = comma)+
  scale_x_log10(limits = c(0.1,25), labels = comma)+
  geom_abline(intercept = 0, slope = 1)+
  stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
  stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
  geom_smooth(method=lm)+
  
  theme_classic()+
  labs(
    title = "RBP",
    subtitle = "",
    x = "observed RBP",
    y = "predicted RBP")

RBP_ratvsBlood.Plasma
ggsave(plot = RBP_ratvsBlood.Plasma, file = "RBP.png", width = 10, height = 10)

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

x <- ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$KineticIV.Thalf..h.
y <- ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$THalf_rat.Dose_mg
fm <- lm(log10(y)~log10(x))
ff <- summary(fm)

THalf_rat.Dose_mg_hepvsKineticIV.Thalf..h. <- ggplot(data = ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH, mapping = aes(x = x,y = y))+
  geom_point(mapping = aes(y=ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$THalf_rat.Dose_mg,x=ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$KineticIV.Thalf..h.), colour = "#529EFF")+
  scale_y_log10(limits = c(0.1,100), labels = comma)+
  scale_x_log10(limits = c(0.1,100), labels = comma)+
  geom_abline(intercept = 0, slope = 1)+
  stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
  stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
  geom_smooth(method=lm)+
  
  theme_classic()+
  labs(
    title = "Simulation All In Silico (RH)",
    subtitle = "",
    x = "observed Thalf [h]",
    y = "predicted Thalf [h]")

THalf_rat.Dose_mg_hepvsKineticIV.Thalf..h.
ggsave(plot = THalf_rat.Dose_mg_hepvsKineticIV.Thalf..h., file = "Thalf_RH.png", width = 10, height = 10)

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

x <- ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RLM$KineticIV.Thalf..h.
y <- ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RLM$THalf_rat.Dose_mg
fm <- lm(log10(y)~log10(x))
ff <- summary(fm)

THalf_rat.Dose_mgvsKineticIV.Thalf..h._RLM <- ggplot(data = ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RLM, mapping = aes(x = x,y = y))+
  geom_point(mapping = aes(y=ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RLM$THalf_rat.Dose_mg,x=ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RLM$KineticIV.Thalf..h.), colour = "#529EFF")+
  scale_y_log10(limits = c(0.1,100), labels = comma)+
  scale_x_log10(limits = c(0.1,100), labels = comma)+
  geom_abline(intercept = 0, slope = 1)+
  stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
  stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
  geom_smooth(method=lm)+
  theme_classic()+
  labs(
    title = "Simulation All In Silico (RLM)",
    subtitle = "",
    x = "observed Thalf [h]",
    y = "predicted Thalf [h]")

THalf_rat.Dose_mgvsKineticIV.Thalf..h._RLM
ggsave(plot = THalf_rat.Dose_mgvsKineticIV.Thalf..h._RLM, file = "THalf_RLM.png", width = 10, height = 10)

ggsave(plot = VdvsKineticIV.Vss..L.kg., file = "Vd_ADMET_prop", width = 10, height = 10)
gsave(plot = VdvsKineticIV.Vss..L.kg., file = "Vd_ADMET_prop", width = 10, height = 10)

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

x <- ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$KineticIV.Vss..L.kg.
y <- ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$S.rVd_PBPK
fm <- lm(log10(y)~log10(x))
ff <- summary(fm)

S.rVd_PBPKvsKineticIV.Vss..L.kg. <- ggplot(data = ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH, mapping = aes(x = x,y = y))+
  geom_point(mapping = aes(y=ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$S.rVd_PBPK,x=ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$KineticIV.Vss..L.kg.), colour = "#529EFF")+
  scale_y_log10(limits = c(0.1,100), labels = comma)+
  scale_x_log10(limits = c(0.1,100), labels = comma)+
  geom_abline(intercept = 0, slope = 1)+
  stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
  stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
  geom_smooth(method=lm)+
  
  theme_classic()+
  labs(
    title = "Simulation All In Silico (RH)",
    subtitle = "",
    x = "observed Vss [L/kg]",
    y = "predicted Vss [L/kg]")

S.rVd_PBPKvsKineticIV.Vss..L.kg.
ggsave(plot = S.rVd_PBPKvsKineticIV.Vss..L.kg., file = "Simulation All In Silico (RH)Vss.png", width = 10, height = 10)

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

ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4$logD.logD..RT. <- str_replace(ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4$logD.logD..RT.,replacement = "", pattern = "<")
ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4$logD.logD..RT. <- as.numeric(ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4$logD.logD..RT.)
x <- ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4$logD.logD..RT.
y <- ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4$S.logD
fm <- lm(log10(y)~log10(x))
ff <- summary(fm)

S.logDvslogD.logD..RT. <- ggplot(data = ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4, mapping = aes(x = x,y = y))+
  geom_point(mapping = aes(y=ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4$S.logD,x=ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4$logD.logD..RT.), colour = "#529EFF")+
  scale_y_continuous(limits = c(2,5), labels = comma)+
  scale_x_continuous(limits = c(2,5), labels = comma)+
  geom_abline(intercept = 0, slope = 1)+
  stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
  stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
  geom_smooth(method=lm)+
  
  theme_classic()+
  labs(
    title = "logD",
    subtitle = "",
    x = "observed logD",
    y = "predicted logD")

S.logDvslogD.logD..RT.
ggsave(plot = S.logDvslogD.logD..RT., file = "logD.png", width = 10, height = 10)

# Solubilities

dd <- ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4

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

x <- ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$solFeSSIF.solubility.mg.mL.
y <- ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$S.FeSSIF
fm <- lm(log10(y)~log10(x))
ff <- summary(fm)

solFeSSIF.solubilityvspred_solFeSSIF.solubility <- ggplot(data = ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH, mapping = aes(x = x,y = y))+
  geom_point(mapping = aes(y=ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$S.FeSSIF,x=ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$solFeSSIF.solubility.mg.mL.), colour = "#529EFF")+
  scale_y_log10(limits = c(0.001,1), labels = comma)+
  scale_x_log10(limits = c(0.001,1), labels = comma)+
  geom_abline(intercept = 0, slope = 1)+
  stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
  stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
  geom_smooth(method=lm)+
  
  theme_classic()+
  labs(
    title = "solFeSSIF",
    subtitle = "",
    x = "predicted solFeSSIF [mg/mL]",
    y = "observed solFeSSIF [mg/mL]")

solFeSSIF.solubilityvspred_solFeSSIF.solubility
ggsave(plot = solFeSSIF.solubilityvspred_solFeSSIF.solubility, file = "solFeSSIF.png", width = 10, height = 10)

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

x <- ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$solFaSSIF.solubility.mg.mL.
y <- ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$S.FaSSIF
fm <- lm(log10(y)~log10(x))
ff <- summary(fm)

solFaSSIF.solubilityvspred_solFaSSIF.solubility <- ggplot(data = ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH, mapping = aes(x = x,y = y))+
  geom_point(mapping = aes(y=ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$S.FaSSIF,x=ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$solFaSSIF.solubility.mg.mL.), colour = "#529EFF")+
  scale_y_log10(limits = c(0.001,1), labels = comma)+
  scale_x_log10(limits = c(0.001,1), labels = comma)+
  geom_abline(intercept = 0, slope = 1)+
  stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
  stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
  geom_smooth(method=lm)+
  
  theme_classic()+
  labs(
    title = "solFaSSIF",
    subtitle = "",
    x = "predicted solFaSSIF [mg/mL]",
    y = "observed solFaSSIF [mg/mL]")

solFaSSIF.solubilityvspred_solFaSSIF.solubility
ggsave(plot = solFaSSIF.solubilityvspred_solFaSSIF.solubility, file = "solFaSSIF.png", width = 10, height = 10)

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
  
  theme_classic()+
  labs(
    title = "solPBS",
    subtitle = "",
    x = "predicted solPBS [mg/mL]",
    y = "observed solPBS [mg/mL]")

solPBSsolubility.mg.mL.vsS.S_pH
ggsave(plot = solPBSsolubility.mg.mL.vsS.S_pH, file = "solPBS.png", width = 10, height = 10)

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

x <- ADMETpH1_2$solHCLsolubility.mg.mL.
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
  
  theme_classic()+
  labs(
    title = "solHCl",
    subtitle = "",
    x = "predicted solHCl [mg/mL]",
    y = "observed solHCl [mg/mL]")
solHCLsolubility.mg.mL.vsS.S_pH
ggsave(plot = solHCLsolubility.mg.mL.vsS.S_pH, file = "solHCl.png", width = 10, height = 10)

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

x <- IVIVE2$obs.CLp
y <- IVIVE2$CLp
fm <- lm(log10(y)~log10(x))
ff <- summary(fm)

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
  
  
  theme_classic()+
  labs(
    title = "IVIVE2_RLM",
    subtitle = "",
    x = "observed CLp [mL/min/kg]",
    y = "predicted CLp [mL/min/kg]")

plot_IVIVE2
ggsave(plot = plot_IVIVE2, file = "IVIVE2_RLM.png", width = 10, height = 10)

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

x <- IVIVE2_hep$obs.CLp
y <- IVIVE2_hep$CLp
fm <- lm(log10(y)~log10(x))
ff <- summary(fm)

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
  
  theme_classic()+
  labs(
    title = "IVIVE2_RH",
    subtitle = "",
    x = "observed CLp [mL/min/kg]",
    y = "predicted CLp [mL/min/kg]")

plot_IVIVE2_hep
ggsave(plot = plot_IVIVE2_hep, file = "IVIVE2_RH.png", width = 10, height = 10)


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

x <- IVIVE3$obs.CLp
y <- IVIVE3$CLp
fm <- lm(log10(y)~log10(x))
ff <- summary(fm)

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
  
  theme_classic()+
  labs(
    title = "IVIVE3_RLM",
    subtitle = "",
    x = "observed CLp [mL/min/kg]",
    y = "predicted CLp [mL/min/kg]")

plot_IVIVE3
ggsave(plot = plot_IVIVE3, file = "IVIVE3_RLM.png", width = 10, height = 10)


# Plot 23 CL_IVIVE fuhep=Dilution method fup=obs

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
 
  theme_classic()+
  labs(
    title = "IVIVE3_RH",
    subtitle = "",
    x = "observed CLp [mL/min/kg]",
    y = "predicted CLp [mL/min/kg]")

plot_IVIVE3_hep
ggsave(plot = plot_IVIVE3_hep, file = "IVIVE3_RH.png", width = 10, height = 10)

# Plot 24 Vss

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

x <- optim_RLM$KineticIV.Vss..L.kg.
y <- optim_RLM$S.rVd_PBPK

  

Vss1 <- ggplot(data = optim_RLM, mapping = aes(x = x,y = y))+
  geom_point(mapping = aes(y=optim_RLM$S.rVd_PBPK,x=optim_RLM$KineticIV.Vss..L.kg.), colour = "#529EFF")+
  scale_y_log10(limits = c(0.1,12), labels = comma)+
  scale_x_log10(limits = c(0.1,12), labels = comma)+
  geom_abline(intercept = 0, slope = 1)+
  stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
  stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
  geom_smooth(method=lm)+
  
  
  theme_classic()+
  labs(
    title = "fup_RBP_RLM",
    subtitle = "",
    x = "observed Vss [L/kg]",
    y = "predicted Vss [L/kg]")

Vss1
ggsave(plot = Vss1, file = "Vss1(fup_RBP).png", width = 10, height = 10)

# Plot 25 Vss2

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

x <- only_fup_input_RLM$KineticIV.Vss..L.kg.
y <- only_fup_input_RLM$S.rVd_PBPK


Vss2 <- ggplot(data = only_fup_input_RLM, mapping = aes(x = x,y = y))+
  geom_point(mapping = aes(y=only_fup_input_RLM$S.rVd_PBPK,x=only_fup_input_RLM$KineticIV.Vss..L.kg.), colour = "#529EFF")+
  scale_y_log10(limits = c(0.1,12), labels = comma)+
  scale_x_log10(limits = c(0.1,12), labels = comma)+
  geom_abline(intercept = 0, slope = 1)+
  stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
  stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
  geom_smooth(method=lm)+
  
  theme_classic()+
  labs(
    title = "fup_RLM",
    subtitle = "",
    x = "Vss_obs (L/kg)",
    y = "Vd_pred_input_fup (L/kg)")

Vss2
ggsave(plot = Vss2, file = "Vss2(fup_RLM).png", width = 10, height = 10)


# Plot 25 Vss3

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

x <- only_RBP_input_RLM$KineticIV.Vss..L.kg.
y <- only_RBP_input_RLM$S.rVd_PBPK

Vss3 <- ggplot(data = only_RBP_input_RLM, mapping = aes(x = x,y = y))+
  geom_point(mapping = aes(y=only_RBP_input_RLM$S.rVd_PBPK,x=only_RBP_input_RLM$KineticIV.Vss..L.kg.), colour = "#529EFF")+
  scale_y_log10(limits = c(0.1,12), labels = comma)+
  scale_x_log10(limits = c(0.1,12), labels = comma)+
  geom_abline(intercept = 0, slope = 1)+
  stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
  stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
  geom_smooth(method=lm)+

  theme_classic()+
  labs(
    title = "RBP_RLM",
    subtitle = "",
    x = "observed Vss [L/kg]",
    y = "predicted Vss [L/kg]")

Vss3
ggsave(plot = Vss3, file = "Vss3(RBP_RLM).png", width = 10, height = 10)


# Plot 26 Vss mit logD und fup RBP

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

  theme_classic()+
  labs(
    title = "fup_RBP_logD",
    subtitle = "",
    x = "observed Vss [L/kg]",
    y = "predicted Vss [L/kg]")


Vss_logD_fup_BP
ggsave(plot = Vss_logD_fup_BP, file = "Vss_logD_fup_RBP.png", width = 10, height = 10)





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

  theme_classic()+
  labs(
    title = "CLintu_RLM",
    subtitle = "",
    x = "observed CLintu_RLM (mL/min/kg)",
    y = "predicted CLintu_RLM (mL/min/kg)")

CLintuvspred_CL_intu_RLM
ggsave(plot = CLintuvspred_CL_intu_RLM, file = "CLintuvspred_CL_intu_RLM.png", width = 10, height = 10)


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

    theme_classic()+
  labs(
    title = "CLintu_obs vs pred_CL_intu_H",
    subtitle = "",
    x = "CLintu_obs (mL/min/kg)",
    y = "pred_CL_intu_H (mL/min/kg)")

CLintuvspred_CL_intu_H
ggsave(plot = CLintuvspred_CL_intu_H, file = "CLintuvspred_CL_intu_Hsteigung.png", width = 10, height = 10)

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
  
  theme_classic()+
  labs(
    title = "CLintu_obs vs pred_CL_intu_obsu_RLM",
    subtitle = "",
    x = "CLintu_obs (mL/min/kg)",
    y = "pred_CL_intu_obsu_RLM (mL/min/kg)")

CLintuvspred_CL_intu_obsu_RLM
ggsave(plot = CLintuvspred_CL_intu_obsu_RLM, file = "CLintuvspred_CL_intu_obsu_RLM.png", width = 10, height = 10)

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
  
  theme_classic()+
  labs(
    title = "CLintu_obs vs pred_CL_intu_obsu_hep",
    subtitle = "",
    x = "CLintu_obs (mL/min/kg)",
    y = "pred_CL_intu_obsu_RLM (mL/min/kg)")

CLintuvspred_CL_intu_obsu_hep
ggsave(plot = CLintuvspred_CL_intu_obsu_hep, file = "CLintuvspred_CL_intu_obsu_hep.png", width = 10, height = 10)


# images saved 10*10




