install.packages("ggplot2")
library(ggplot2)
install.packages("scales")
library(scales)
install.packages("fun")
library(fun)

# Plot 1 CLintu_RLM vs predicted CLintu_RLM

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


Syst.in.vivo.CLb<-ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$KineticIV.Clearance..mL.min.kg.
classification <- c()
for(i in 1:length(Syst.in.vivo.CLb)){if(Syst.in.vivo.CLb[i]==0){
  classification[i] <- "NA"}else{
    if(Syst.in.vivo.CLb[i]<=36.6){
      classification[i] <- "low"  
    }else{
      if(Syst.in.vivo.CLb[i]>36.6&Syst.in.vivo.CLb[i]<85.4){
        classification[i] <- "moderate"
      }else{
        if(Syst.in.vivo.CLb[i]>=85.4){
          classification[i] <- "high"
        }
      }
    }
  }
 }
Syst.in.vivo.CLb<-classification

x <- ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$Clintu_hep_obs
y <- ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$HEP_rCLintu

CLintu_RH <- ggplot(data = ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH, mapping = aes(x = x,y = y))+
  geom_point(mapping = aes(y=ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$HEP_rCLintu,x=ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$Clintu_hep_obs), colour = "#529EFF")+
  scale_y_log10(limits = c(10,5000), labels = comma)+
  scale_x_log10(limits = c(10,5000), labels = comma)+
  geom_abline(intercept = 0, slope = 1)+
  stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
  stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
  geom_smooth(method=lm)+
  geom_point(aes(colour=Syst.in.vivo.CLb, group=Syst.in.vivo.CLb))+

  
  labs(
    title = paste("CLintu_RH"),
    subtitle = "",
    x = "observed CLintu_RH [µL/min/kg]",
    y = "predicted CLintu_RH [µL/min/kg]")

CLintu_RH
ggsave(plot = CLintu_RH, file = "CLintu_RH.png", width = 10, height = 10)

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


Syst.in.vivo.CLb<-ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$KineticIV.Clearance..mL.min.kg.

classification <- c()
for(i in 1:length(Syst.in.vivo.CLb)){if(Syst.in.vivo.CLb[i]==0){
  classification[i] <- "NA"}else{
    if(Syst.in.vivo.CLb[i]<=36.6){
      classification[i] <- "low"  
    }else{
      if(Syst.in.vivo.CLb[i]>36.6&Syst.in.vivo.CLb[i]<85.4){
        classification[i] <- "moderate"
      }else{
        if(Syst.in.vivo.CLb[i]>=85.4){
          classification[i] <- "high"
        }
      }
    }
  }
}
Syst.in.vivo.CLb<-classification

x <- ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$Clintu_mic_obs
y <- ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$CYP_RLM_Clintu

CLintu_RLM <- ggplot(data = ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH, mapping = aes(x = x,y = y))+
  geom_point(mapping = aes(y=ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$CYP_RLM_Clintu,x=ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$Clintu_mic_obs), colour = "#529EFF")+
  scale_y_log10(limits = c(10,5000), labels = comma)+
  scale_x_log10(limits = c(10,5000), labels = comma)+
  geom_abline(intercept = 0, slope = 1)+
  stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
  stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
  geom_smooth(method=lm)+
  geom_point(aes(colour=Syst.in.vivo.CLb, group=Syst.in.vivo.CLb))+
  
  labs(
    title = paste("CLintu_RLM"),
    subtitle = "",
    x = "observed CLintu_RLM [µL/min/kg]",
    y = "predicted CLintu_RLM [µL/min/kg]")

CLintu_RLM
ggsave(plot = CLintu_RLM, file = "CLintu_RLM.png", width = 10, height = 10)

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


Syst.in.vivo.CLb1<-ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$KineticIV.Clearance..mL.min.kg.

classification <- c()
for(i in 1:length(Syst.in.vivo.CLb1)){if(Syst.in.vivo.CLb1[i]==0){
  classification[i] <- "NA"}else{
    if(Syst.in.vivo.CLb1[i]<=36.6){
      classification[i] <- "low"  
    }else{
      if(Syst.in.vivo.CLb1[i]>36.6&Syst.in.vivo.CLb1[i]<85.4){
        classification[i] <- "moderate"
      }else{
        if(Syst.in.vivo.CLb1[i]>=85.4){
          classification[i] <- "high"
        }
      }
    }
  }
}
Syst.in.vivo.CLb<-classification

x <- ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$Clintu_hep_obs
y <- ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$Clintu_obs

CLintu_back_RH <- ggplot(data = ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH, mapping = aes(x = x,y = y))+
  geom_point(mapping = aes(y=ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$Clintu_obs,x=ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$Clintu_hep_obs), colour = "#529EFF")+
  scale_y_log10(limits = c(10,160000), labels = comma)+
  scale_x_log10(limits = c(10,160000), labels = comma)+
  geom_abline(intercept = 0, slope = 1)+
  stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
  stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
  geom_smooth(method=lm)+
  geom_point(aes(colour=Syst.in.vivo.CLb, group=Syst.in.vivo.CLb))+
  
  labs(
    title = paste("CLintu_back_RH"),
    subtitle = "",
    x = "observed CLintu [µL/min/kg]",
    y = "predicted CLintu [µL/min/kg]")

CLintu_back_RH
ggsave(plot = CLintu_back_RH, file = "CLintu_back_RH.png", width = 10, height = 10)

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


Syst.in.vivo.CLb1<-ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$KineticIV.Clearance..mL.min.kg.

classification <- c()
for(i in 1:length(Syst.in.vivo.CLb1)){if(Syst.in.vivo.CLb1[i]==0){
  classification[i] <- "NA"}else{
    if(Syst.in.vivo.CLb1[i]<=36.6){
      classification[i] <- "low"  
    }else{
      if(Syst.in.vivo.CLb1[i]>36.6&Syst.in.vivo.CLb1[i]<85.4){
        classification[i] <- "moderate"
      }else{
        if(Syst.in.vivo.CLb1[i]>=85.4){
          classification[i] <- "high"
        }
      }
    }
  }
}
x <- ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$Clintu_mic_obs
y <- ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$Clintu_obs

CLintu_back_RLM <- ggplot(data = ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH, mapping = aes(x = x,y = y))+
  geom_point(mapping = aes(y=ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$Clintu_obs,x=ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$Clintu_mic_obs), colour = "#529EFF")+
  scale_y_log10(limits = c(10,160000), labels = comma)+
  scale_x_log10(limits = c(10,160000), labels = comma)+
  geom_abline(intercept = 0, slope = 1)+
  stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
  stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
  geom_smooth(method=lm)+
  geom_point(aes(colour=Syst.in.vivo.CLb, group=Syst.in.vivo.CLb))+
  
  labs(
    title = paste("CLintu_back_RLM"),
    subtitle = "",
    x = "observed CLintu [µL/min/kg]",
    y = "predicted CLintu [µL/min/kg]")

CLintu_back_RLM
ggsave(plot = CLintu_back_RLM, file = "CLintu_back_RLM.png", width = 10, height = 10)

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


Syst.in.vivo.CLb1<-ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$KineticIV.Clearance..mL.min.kg.

classification <- c()
for(i in 1:length(Syst.in.vivo.CLb1)){if(Syst.in.vivo.CLb1[i]==0){
  classification[i] <- "NA"}else{
    if(Syst.in.vivo.CLb1[i]<=36.6){
      classification[i] <- "low"  
    }else{
      if(Syst.in.vivo.CLb1[i]>36.6&Syst.in.vivo.CLb1[i]<85.4){
        classification[i] <- "moderate"
      }else{
        if(Syst.in.vivo.CLb1[i]>=85.4){
          classification[i] <- "high"
        }
      }
    }
  }
  }
Syst.in.vivo.CLb<-classification

x <- ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$Clintu_mic_obs
y <- ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$pred_Clint_obsu_mic

CLint_obsu_RLM <- ggplot(data = ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH, mapping = aes(x = x,y = y))+
  geom_point(mapping = aes(y=ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$pred_Clint_obsu_mic,x=ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$Clintu_mic_obs), colour = "#529EFF")+
  scale_y_log10(limits = c(10,13000), labels = comma)+
  scale_x_log10(limits = c(10,13000), labels = comma)+
  geom_abline(intercept = 0, slope = 1)+
  stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
  stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
  geom_smooth(method=lm)+
  geom_point(aes(colour=Syst.in.vivo.CLb, group=Syst.in.vivo.CLb))+
  
  labs(
    title = paste("CLint_obsu_RLM"),
    subtitle = "",
    x = "observed CLintu [µL/min/kg]",
    y = "predicted CLintu [µL/min/kg]")

CLint_obsu_RLM
ggsave(plot = CLint_obsu_RLM, file = "CLint_obsu_RLM.png", width = 10, height = 10)

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


Syst.in.vivo.CLb1<-ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$KineticIV.Clearance..mL.min.kg.

classification <- c()
for(i in 1:length(Syst.in.vivo.CLb1)){if(Syst.in.vivo.CLb1[i]==0){
  classification[i] <- "NA"}else{
    if(Syst.in.vivo.CLb1[i]<=36.6){
      classification[i] <- "low"  
    }else{
        if(Syst.in.vivo.CLb1[i]>36.6&Syst.in.vivo.CLb1[i]<85.4){
          classification[i] <- "moderate"
        }else{
          if(Syst.in.vivo.CLb1[i]>=85.4){
            classification[i] <- "high"
          }
        }
      }
  }
}

Syst.in.vivo.CLb<-classification

x <- ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$Clintu_hep_obs
y <- ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$pred_Clint_obsu_hep

CLint_obsu_RH <- ggplot(data = ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH, mapping = aes(x = x,y = y))+
  geom_point(mapping = aes(y=ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$pred_Clint_obsu_hep,x=ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RH$Clintu_hep_obs), colour = "#529EFF")+
  scale_y_log10(limits = c(10,5000), labels = comma)+
  scale_x_log10(limits = c(10,5000), labels = comma)+
  geom_abline(intercept = 0, slope = 1)+
  stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
  stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
  geom_smooth(method=lm)+
  geom_point(aes(colour=Syst.in.vivo.CLb, group=Syst.in.vivo.CLb))+
  
  labs(
    title = paste("CLint_obsu_RH"),
    subtitle = "",
    x = "observed CLintu [µL/min/kg]",
    y = "predicted CLintu [µL/min/kg]")

CLint_obsu_RH
ggsave(plot = CLint_obsu_RH, file = "CLint_obsu_RH.png", width = 10, height = 10)

