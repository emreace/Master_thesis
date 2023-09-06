install.packages("ggplot2")
library(ggplot2)
install.packages("scales")
library(scales)
install.packages("fun")
library(fun)
install.packages("ggplot2")
library(ggplot2)
install.packages("scales")
library(scales)
install.packages("fun")
library(fun)

# Plot 1 input_fup_RBPrat_RLM vs KineticIV.Clearance..mL.min.kg.

stat_function_01 <- function(x){
  3 * x
}
stat_function_02 <- function(x){
  (1/3) * x
}
stat_function_03 <- function(x){
  10 * x
}
stat_function_04 <- function(x){
  0.1 * x
}
PPB1<-optim_RLM_chem_class$PlasmaProteinBinding.Concentration..uM....1
PPB<-ifelse(PPB1<2,"high","low")

x <- optim_RLM_chem_class$KineticIV.Clearance..mL.min.kg.
y <- optim_RLM_chem_class$CL_rat.Dose_mg*66.6666666668


CLphep1vsKineticIV.Clearance..mL.min.kg. <- ggplot(data = optim_RLM_chem_class, mapping = aes(x = x,y = y))+
  geom_point(mapping = aes(y=optim_RLM_chem_class$CL_rat.Dose_mg*66.6666666668,x=optim_RLM_chem_class$KineticIV.Clearance..mL.min.kg.), colour = "#529EFF")+
  scale_y_log10(limits = c(0.01,210), labels = comma)+
  scale_x_log10(limits = c(0.01,210), labels = comma)+
  geom_abline(intercept = 0, slope = 1)+
  stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
  stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
  geom_smooth(method=lm)+
  geom_point(aes(colour=PPB, group=PPB))+
  
  
  theme_classic()+
  labs(
    title = "CL_input_fup_RBPrat_RLM vs CLobs",
    subtitle = "",
    x = "CLobs (mL/min/kg)",
    y = "CLpred (mL/min/kg)")

CLphep1vsKineticIV.Clearance..mL.min.kg.
ggsave(plot = CLphep1vsKineticIV.Clearance..mL.min.kg., file = "CL_optim_RLM(input_fup_RBP)_PPB.png", width = 10, height = 10)

# Plot 2 input_fup_RBPrat_RH vs KineticIV.Clearance..mL.min.kg.

stat_function_01 <- function(x){
  3 * x
}
stat_function_02 <- function(x){
  (1/3) * x
}
stat_function_03 <- function(x){
  10 * x
}
stat_function_04 <- function(x){
  0.1 * x
}

PPB1<-optim_RH$PlasmaProteinBinding.Concentration..uM....1
PPB<-ifelse(PPB1<2,"high","low")

x <- optim_RH$KineticIV.Clearance..mL.min.kg.
y <- optim_RH$CL_rat.Dose_mg*66.6666666668


CLphep1vsKineticIV.Clearance..mL.min.kg. <- ggplot(data = optim_RH, mapping = aes(x = x,y = y))+
  geom_point(mapping = aes(y = optim_RH$CL_rat.Dose_mg*66.6666666668,x=optim_RH$KineticIV.Clearance..mL.min.kg.), colour = "#529EFF")+
  scale_y_log10(limits = c(0.01,210), labels = comma)+
  scale_x_log10(limits = c(0.01,210), labels = comma)+
  geom_abline(intercept = 0, slope = 1)+
  stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
  stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
  geom_smooth(method=lm)+
  geom_point(aes(colour=PPB, group=PPB))+
  
  
  theme_classic()+
  labs(
    title = "CL_input_fup_RBPrat_RH vs CLobs",
    subtitle = "",
    x = "CLobs (mL/min/kg)",
    y = "CLpred (mL/min/kg)")

CLphep1vsKineticIV.Clearance..mL.min.kg.
ggsave(plot = CLphep1vsKineticIV.Clearance..mL.min.kg., file = "CL_optim_RH(input_fup_RBP)_PPB.png", width = 10, height = 10)

# Plot 3 only_fup_RLM vs KineticIV.Clearance..mL.min.kg.

stat_function_01 <- function(x){
  3 * x
}
stat_function_02 <- function(x){
  (1/3) * x
}
stat_function_03 <- function(x){
  10 * x
}
stat_function_04 <- function(x){
  0.1 * x
}

PPB1<-only_fup_input_RLM_ion_class$PlasmaProteinBinding.Concentration..uM....1
PPB<-ifelse(PPB1<2,"high","low")

x <- only_fup_input_RLM_ion_class$KineticIV.Clearance..mL.min.kg.
y <- only_fup_input_RLM_ion_class$CL_rat.Dose_mg*66.6666666668


CLphep1vsKineticIV.Clearance..mL.min.kg. <- ggplot(data = only_fup_input_RLM_ion_class, mapping = aes(x = x,y = y))+
  geom_point(mapping = aes(y = only_fup_input_RLM_ion_class$CL_rat.Dose_mg*66.6666666668,x=only_fup_input_RLM_ion_class$KineticIV.Clearance..mL.min.kg.), colour = "#529EFF")+
  scale_y_log10(limits = c(0.01,210), labels = comma)+
  scale_x_log10(limits = c(0.01,210), labels = comma)+
  geom_abline(intercept = 0, slope = 1)+
  stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
  stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
  geom_smooth(method=lm)+
  geom_point(aes(colour=PPB, group=PPB))+
  
  
  theme_classic()+
  labs(
    title = "CL_only_fup_RLM vs CLobs",
    subtitle = "",
    x = "CLobs (mL/min/kg)",
    y = "CLpred (mL/min/kg)")

CLphep1vsKineticIV.Clearance..mL.min.kg.
ggsave(plot = CLphep1vsKineticIV.Clearance..mL.min.kg., file = "CL_optim2_RLM(only_fup)_PPB.png", width = 10, height = 10)


# Plot 4 only_fup_RH vs KineticIV.Clearance..mL.min.kg.

stat_function_01 <- function(x){
  3 * x
}
stat_function_02 <- function(x){
  (1/3) * x
}
stat_function_03 <- function(x){
  10 * x
}
stat_function_04 <- function(x){
  0.1 * x
}

PPB1<-only_fup_input_RH$PlasmaProteinBinding.Concentration..uM....1
PPB<-ifelse(PPB1<2,"high","low")

x <- only_fup_input_RH$KineticIV.Clearance..mL.min.kg.
y <- only_fup_input_RH$CL_rat.Dose_mg*66.6666666668


CLphep1vsKineticIV.Clearance..mL.min.kg. <- ggplot(data = only_fup_input_RH, mapping = aes(x = x,y = y))+
  geom_point(mapping = aes(y = only_fup_input_RH$CL_rat.Dose_mg*66.6666666668,x=only_fup_input_RH$KineticIV.Clearance..mL.min.kg.), colour = "#529EFF")+
  scale_y_log10(limits = c(0.01,210), labels = comma)+
  scale_x_log10(limits = c(0.01,210), labels = comma)+
  geom_abline(intercept = 0, slope = 1)+
  stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
  stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
  geom_smooth(method=lm)+
  geom_point(aes(colour=PPB, group=PPB))+  
  
  theme_classic()+
  labs(
    title = "CL_only_fup_RH vs CLobs",
    subtitle = "",
    x = "CLobs (mL/min/kg)",
    y = "CLpred (mL/min/kg)")

CLphep1vsKineticIV.Clearance..mL.min.kg.
ggsave(plot = CLphep1vsKineticIV.Clearance..mL.min.kg., file = "CL_optim2_RH(only_fup)_PPB.png", width = 10, height = 10)

# Plot 5 only_RBP_input_RLM vs KineticIV.Clearance..mL.min.kg.

stat_function_01 <- function(x){
  3 * x
}
stat_function_02 <- function(x){
  (1/3) * x
}
stat_function_03 <- function(x){
  10 * x
}
stat_function_04 <- function(x){
  0.1 * x
}

PPB1<-only_RBP_input_RLM_ion_class$PlasmaProteinBinding.Concentration..uM....1
PPB<-ifelse(PPB1<2,"high","low")

x <- only_RBP_input_RLM_ion_class$KineticIV.Clearance..mL.min.kg.
y <- only_RBP_input_RLM_ion_class$CL_rat.Dose_mg*66.6666666668


CLphep1vsKineticIV.Clearance..mL.min.kg. <- ggplot(data = only_RBP_input_RLM_ion_class, mapping = aes(x = x,y = y))+
  geom_point(mapping = aes(y = only_RBP_input_RLM_ion_class$CL_rat.Dose_mg*66.6666666668,x=only_RBP_input_RLM_ion_class$KineticIV.Clearance..mL.min.kg.), colour = "#529EFF")+
  scale_y_log10(limits = c(0.01,210), labels = comma)+
  scale_x_log10(limits = c(0.01,210), labels = comma)+
  geom_abline(intercept = 0, slope = 1)+
  stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
  stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
  geom_smooth(method=lm)+
  geom_point(aes(colour=PPB, group=PPB))+    
  
  theme_classic()+
  labs(
    title = "CL_only_RBP_RLM vs CLobs",
    subtitle = "",
    x = "CLobs (mL/min/kg)",
    y = "CLpred (mL/min/kg)")

CLphep1vsKineticIV.Clearance..mL.min.kg.
ggsave(plot = CLphep1vsKineticIV.Clearance..mL.min.kg., file = "CL_optim3_RLM(only_RBP)_PPB.png", width = 10, height = 10)

# Plot 6 only_RBP_input_RH vs KineticIV.Clearance..mL.min.kg.

stat_function_01 <- function(x){
  3 * x
}
stat_function_02 <- function(x){
  (1/3) * x
}
stat_function_03 <- function(x){
  10 * x
}
stat_function_04 <- function(x){
  0.1 * x
}

PPB1<-only_RBP_input_RH$PlasmaProteinBinding.Concentration..uM....1
PPB<-ifelse(PPB1<2,"high","low")


x <- only_RBP_input_RH$KineticIV.Clearance..mL.min.kg.
y <- only_RBP_input_RH$CL_rat.Dose_mg*66.6666666668


CLphep1vsKineticIV.Clearance..mL.min.kg. <- ggplot(data = only_RBP_input_RH, mapping = aes(x = x,y = y))+
  geom_point(mapping = aes(y = only_RBP_input_RH$CL_rat.Dose_mg*66.6666666668,x=only_RBP_input_RH$KineticIV.Clearance..mL.min.kg.), colour = "#529EFF")+
  scale_y_log10(limits = c(0.01,210), labels = comma)+
  scale_x_log10(limits = c(0.01,210), labels = comma)+
  geom_abline(intercept = 0, slope = 1)+
  stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
  stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
  geom_smooth(method=lm)+
  geom_point(aes(colour=PPB, group=PPB))+    
  
  
  theme_classic()+
  labs(
    title = "CL_only_RBP_RH vs CLobs",
    subtitle = "",
    x = "CLobs (mL/min/kg)",
    y = "CLpred (mL/min/kg)")

CLphep1vsKineticIV.Clearance..mL.min.kg.
ggsave(plot = CLphep1vsKineticIV.Clearance..mL.min.kg., file = "CL_optim3_RH(only_RBP)_PPB.png", width = 10, height = 10)

#AB HIER WEITER
# Plot 7 systemic_IVIVE3_hep vs KineticIV.Clearance..mL.min.kg.

stat_function_01 <- function(x){
  3 * x
}
stat_function_02 <- function(x){
  (1/3) * x
}
stat_function_03 <- function(x){
  10 * x
}
stat_function_04 <- function(x){
  0.1 * x
}

PPB1<-systemic_IVIVE_hep_3_input_export$PlasmaProteinBinding.Concentration..uM....1
PPB<-ifelse(PPB1<2,"high","low")

x <- systemic_IVIVE_hep_3_input_export$KineticIV.Clearance..mL.min.kg.
y <- systemic_IVIVE_hep_3_input_export$CL_rat.Dose_mg*66.6666666668


CLphep1vsKineticIV.Clearance..mL.min.kg. <- ggplot(data = systemic_IVIVE_hep_3_input_export, mapping = aes(x = x,y = y))+
  geom_point(mapping = aes(y = systemic_IVIVE_hep_3_input_export$CL_rat.Dose_mg*66.6666666668,x=systemic_IVIVE_hep_3_input_export$KineticIV.Clearance..mL.min.kg.), colour = "#529EFF")+
  scale_y_log10(limits = c(0.01,210), labels = comma)+
  scale_x_log10(limits = c(0.01,210), labels = comma)+
  geom_abline(intercept = 0, slope = 1)+
  stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
  stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
  geom_smooth(method=lm)+
  geom_point(aes(colour=PPB, group=PPB))+    
  
  
  theme_classic()+
  labs(
    title = "systemic_IVIVE_hep_3_input vs CLobs",
    subtitle = "",
    x = "CLobs (mL/min/kg)",
    y = "CLpred (mL/min/kg)")

CLphep1vsKineticIV.Clearance..mL.min.kg.
ggsave(plot = CLphep1vsKineticIV.Clearance..mL.min.kg., file = "systemic_IVIVE_hep_3_input_PPB.png", width = 10, height = 10)


# Plot 8 systemic_IVIVE3_hep_RBP vs KineticIV.Clearance..mL.min.kg.

stat_function_01 <- function(x){
  3 * x
}
stat_function_02 <- function(x){
  (1/3) * x
}
stat_function_03 <- function(x){
  10 * x
}
stat_function_04 <- function(x){
  0.1 * x
}

PPB1<-systemic_IVIVE_hep_3_undRBPinput$PlasmaProteinBinding.Concentration..uM....1
PPB<-ifelse(PPB1<2,"high","low")

x <- systemic_IVIVE_hep_3_undRBPinput$KineticIV.Clearance..mL.min.kg.
y <- systemic_IVIVE_hep_3_undRBPinput$CL_rat.Dose_mg*66.6666666668


CLphep1vsKineticIV.Clearance..mL.min.kg. <- ggplot(data = systemic_IVIVE_hep_3_undRBPinput, mapping = aes(x = x,y = y))+
  geom_point(mapping = aes(y = systemic_IVIVE_hep_3_undRBPinput$CL_rat.Dose_mg*66.6666666668,x=systemic_IVIVE_hep_3_undRBPinput$KineticIV.Clearance..mL.min.kg.), colour = "#529EFF")+
  scale_y_log10(limits = c(0.01,210), labels = comma)+
  scale_x_log10(limits = c(0.01,210), labels = comma)+
  geom_abline(intercept = 0, slope = 1)+
  stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
  stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
  geom_smooth(method=lm)+
  geom_point(aes(colour=PPB, group=PPB))+    
  
  theme_classic()+
  labs(
    title = "systemic_IVIVE_hep_3_RBP_fup vs CLobs",
    subtitle = "",
    x = "CLobs (mL/min/kg)",
    y = "CLpred (mL/min/kg)")

CLphep1vsKineticIV.Clearance..mL.min.kg.
ggsave(plot = CLphep1vsKineticIV.Clearance..mL.min.kg., file = "systemic_IVIVE_hep_3_RBP_fupinput_PPB.png", width = 10, height = 10)



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

PPB1<-ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$PlasmaProteinBinding.Concentration..uM....1
PPB<-ifelse(PPB1<2,"high","low")

x <- ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$KineticIV.AUC..ng.h.mL.
y <- ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$AUC_rat.Dose_mg
fm <- lm(log10(y)~log10(x))
ff <- summary(fm)

pred_AUCinf_ratvsAUC_obs <- ggplot(data = ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH, mapping = aes(x = x,y = y))+
  geom_point(mapping = aes(y=ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$AUC_rat.Dose_mg,x=ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$KineticIV.AUC..ng.h.mL.), colour = "#529EFF")+
  scale_y_log10(limits = c(1,40000), labels = comma)+
  scale_x_log10(limits = c(1,40000), labels = comma)+
  geom_abline(intercept = 0, slope = 1)+
  stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
  stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
  geom_smooth(method=lm)+
  
  geom_point(aes(colour=PPB, group=PPB))+
  
  theme_classic()+
  labs(
    title = "pred_AUC_rat vs AUC_obs RH",
    subtitle = "",
    x = "AUC_obs(ng*h/mL)",
    y = "pred_AUCinf_rat (ng*h/mL)")

pred_AUCinf_ratvsAUC_obs
ggsave(plot = pred_AUCinf_ratvsAUC_obs, file = "pred_AUCinf_ratvsAUC_obs_PPB.png")



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

PPB1<-ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RLM$PlasmaProteinBinding.Concentration..uM....1
PPB<-ifelse(PPB1<2,"high","low")

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
  geom_point(aes(colour=PPB, group=PPB))+
  
  theme_classic()+
  labs(
    title = "Simulation All In Silico with logP (RLM)",
    subtitle = "",
    x = "observed AUC [ng*h/mL]",
    y = "predicted AUC [ng*h/mL]")

pred_AUCinf_ratvsAUC_obs
ggsave(plot = pred_AUCinf_ratvsAUC_obs, file = "AUC_RLM_PPB.png", width = 10, height = 10)


