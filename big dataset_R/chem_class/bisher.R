IVIVE1_RH <- read.csv("~/Big Dataset/IVIVE/IVIVE1_RH.csv")

setwd("~/Big Dataset/IVIVE")
IVIVE1_RH <- read.csv("IVIVE1_RH.csv")
IVIVE1_RLM <- read.csv("IVIVE1_RLM.csv")
IVIVE2_RH <- read.csv("IVIVE2_RH.csv")
IVIVE2_RLM <- read.csv("IVIVE2_RLM.csv")
IVIVE3_RH <- read.csv("IVIVE3_RH.csv")
IVIVE3_RLM <- read.csv("IVIVE3_RLM.csv")


setwd("~/Big Dataset/Simulations/ADMET Properties/chem_class")
ring_anchored_rep <- read.csv("~/Big Dataset/Simulations/ADMET Properties/chem_class/7.4_chemclass_ring_anchored_repr.struct.csv")
fingerprint <- read.csv("~/Big Dataset/Simulations/ADMET Properties/chem_class/All_final_to_share_B2P%_7.4_chemclass_fingerprint.csv")

setwd("~/Big Dataset/Simulations/ADMET Properties")
admed_physiol <-  read.csv("~/Big Dataset/Simulations/ADMET Properties/All_final_to_share_B2P%_7.4.csv")

setwd("~/Big Dataset/Simulations/All in silico")
insilico_logd_rh <-  read.csv("~/Big Dataset/Simulations/All in silico/All_final_to_share_template_onedose_RH_logD.csv")
insilico_logp_rh <-  read.csv("~/Big Dataset/Simulations/All in silico/All_final_to_share_template_onedose_RH_logP.csv")
insilico_logd_rlm <-  read.csv("~/Big Dataset/Simulations/All in silico/All_final_to_share_template_onedose_RLM_logD.csv")
insilico_logp_rlm <-  read.csv("~/Big Dataset/Simulations/All in silico/All_final_to_share_template_onedose_RLM_logP.csv")

setwd("~/Big Dataset/Simulations/fed properties fup,RBP,logD")
BtP_logD_RH <- read.csv("BtP_logD_RH.csv")
BtP_logD_RLM <- read.csv("BtP_logD_RLM.csv")
BtP_RH <- read.csv("BtP_RH.csv")
BtP_RLM <- read.csv("BtP_RLM.csv")
fup_BtP_logD_RH <- read.csv("fup_BtP_logD_RH.csv")
fup_BtP_logD_RLM <- read.csv("fup_BtP_logD_RLM.csv")
fup_BtP_RH <- read.csv("fup_BtP_RH.csv")
fup_BtP_RLM <- read.csv("fup_BtP_RLM.csv")
fup_logD_RH <- read.csv("fup_logD_RH.csv")
fup_logD_RLM <- read.csv("fup_logD_RLM.csv")
fup_RH <- read.csv("fup_RH.csv")
fup_RLM <- read.csv("fup_RLM.csv")
logD_RH <- read.csv("logD_RH.csv")
logD_RLM <- read.csv("logD_RLM.csv")
P.O.1 <- read.csv("~/Big Dataset/p.o/P.O.1/P.O.1.csv")
P.O.2 <- read.csv("~/Big Dataset/p.o/P.O.2/P.O.2.csv")
P.O.3 <- read.csv("~/Big Dataset/p.o/P.O.3/P.O.3.csv")
P.O.4 <- read.csv("~/Big Dataset/p.o/P.O.4/P.O.4.csv")
# P.O.5 <- read.csv("~/Big Dataset/p.o/P.O.5/P.O.5.csv")
# P.O.6 <- read.csv("~/Big Dataset/p.o/P.O.6/P.O.6.csv")
template <- read.csv("~/Big Dataset/p.o/template.csv")
template_all_geomean_predictions_fraction <- read.csv("~/Big Dataset/template_all_geomean_predictions_fraction.csv")

library(dplyr)
# install.packages("ggplot2")
library(ggplot2)
# install.packages("scales")
library(scales)
# install.packages("fun")
library(fun)
# install.packages("tidyverse")
library(tidyverse)
library(ggpubr)
library(stargazer)
aafe <- function(x,y){ #x observed, y predicted
  df <- data.frame(x,y)
  df <- na.omit(df)
  res <- 10 ** ((1/length(df$x))*sum(abs(log10(df$y/df$x))))
  
  return(res)
}
afe <- function(x,y){ #x observed, y predicted
  df <- data.frame(x,y)
  df <- na.omit(df)
  res <- 10 ** ((1/length(df$x))*sum((log10(df$y/df$x))))
  return(res)
}

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
setwd("~/Big Dataset/logd_sort")
#IVIVE
dm <- read.csv("~/Big Dataset/Simulations/ADMET Properties/chem_class/All_final_to_share_B2P%_7.4_chemclass_fingerprint.csv")
chem_class <- dm$Class.List
rango <- dm
# 


dl <- list(IVIVE1_RH,IVIVE1_RLM,IVIVE2_RH,IVIVE2_RLM,IVIVE3_RH,IVIVE3_RLM)
names <- c("IVIVE1_RH", "IVIVE1_RLM", "IVIVE2_RH", "IVIVE2_RLM", "IVIVE3_RH", "IVIVE3_RLM")
names_vec <- c("IVIVE1_RH", "IVIVE1_RLM", "IVIVE2_RH", "IVIVE2_RLM", "IVIVE3_RH", "IVIVE3_RLM")

# Create empty list to store plots
plot_list <- list()
ivive_mat <- matrix(data = NA, nrow = 6, ncol = 3, byrow = FALSE, dimnames = NULL)
for(i in 1:6){
  d <- dl[[i]]
  
  if(i ==2){ 
    d$Idorsia.No <-d$Identifier
  }
  if(i ==4){
    d$Idorsia.No <-d$Identifier
    
  }
  if(i ==6){
    d$Idorsia.No <-d$Identifier
    
  }
  d <- arrange(d,d$Idorsia.No)
  rango <- arrange(rango,rango$Idorsia.No)
  tem <- arrange(template, template$Idorsia.No)
  dm <- arrange(dm, dm$Idorsia.No)
  id <- intersect(dm$Idorsia.No,tem$Idorsia.No)
  id <- intersect(d$Idorsia.No,tem$Idorsia.No)
  id <- intersect(id,rango$Idorsia.No)
  d <- d[d$Idorsia.No%in%id,]
  dm <- dm[dm$Idorsia.No%in%id,]
  
  tem <- tem[tem$Idorsia.No%in%id,]
  rango <- rango[rango$Idorsia.No%in%id,]
  rango <- arrange(rango,rango$Idorsia.No)
  d <- arrange(d,d$Idorsia.No)
  tem <- arrange(tem, tem$Idorsia.No)
  sum(tem$Idorsia.No==d$Idorsia.No)
  
  x <- tem$KineticIV.Clearance..mL.min.kg.
  y <- as.numeric(d$CLp)
  chem_class <- dm$Class.List
  df <- data.frame(x,y,chem_class)
  df$chem_class <- ifelse(df$chem_class=="",NA,df$chem_class)
  df$y <- ifelse(df$y==0,NA,df$y)
  df$x <- ifelse(df$x==0,NA,df$x)
  # df <- data.frame(x = tem$KineticIV.Clearance..mL.min.kg., y = as.numeric(d$CLp))
  # df$chem_class <- chem_class

  
  # Iterate over names and create plot for each name
    plot_list <- ggplot(data = df, mapping = aes(x = df$x,y = df$y))+
      geom_point(mapping = aes(y=df$y,x=df$x), colour = "#529EFF")+
      scale_y_log10(limits = c(0.01,1000), labels = comma)+
      scale_x_log10(limits = c(0.01,1000), labels = comma)+
      geom_abline(intercept = 0, slope = 1)+
      stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
      stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
      stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
      stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
      geom_smooth(method=lm)+
      geom_point(aes(color=chem_class,group=chem_class))+
      guides(color="none")+
    
      labs(
        title = paste(names_vec[i]),
        subtitle = "",
        x = "observed CLp [mL/min/kg]",
        y = "predicted CLp [mL/min/kg]")
    
    # extract slope from ggplot
    # model <- lm(data = df, formula = log10(y) ~ log10(x))
    # slope <- summary(model)$coefficients[2]
    
    # print slope
    # print(slope)
    ggsave(plot = plot_list, file = paste(names_vec[i], ".png", sep = ""), width = 20, height = 20)
    # df <- na.omit(df)
    # ivive_mat[i,] <- c(afe(df$x,df$y), aafe(df$x,df$y),slope)
}
# colnames(ivive_mat) <- c("afe","aafe","slope")
# rownames(ivive_mat) <-(c(names_vec))
# ivive_mat
# 
# stargazer(ivive_mat)
# write.csv(ivive_mat,"ivive_mat.csv")


#insilico
# insilico_logd_rh <-  read.csv("~/Big Dataset/Simulations/All in silico/All_final_to_share_template_onedose_RH_logD.csv")
# insilico_logp_rh <-  read.csv("~/Big Dataset/Simulations/All in silico/All_final_to_share_template_onedose_RH_logP.csv")
# insilico_logd_rlm <-  read.csv("~/Big Dataset/Simulations/All in silico/All_final_to_share_template_onedose_RLM_logD.csv")
# insilico_logp_rlm <-  read.csv("~/Big Dataset/Simulations/All in silico/All_final_to_share_template_onedose_RLM_logP.csv")
# 


dl <- list(insilico_logd_rh,insilico_logp_rh,insilico_logd_rlm,insilico_logp_rlm)
names <- c("Simulation All In Silico with logD (RH)", "Simulation All In Silico with logP (RH)", "Simulation All In Silico with logD (RLM)", "Simulation All In Silico with logP (RLM)")
names_vec <- c("Simulation All In Silico with logD (RH)", "Simulation All In Silico with logP (RH)", "Simulation All In Silico with logD (RLM)", "Simulation All In Silico with logP (RLM)")


# Create empty list to store plots
plot_list <- list()

for(i in 1:6){
  d <- dl[[i]]
  
  if(i ==2){
    d$Idorsia.No <-d$Identifier
  }
  if(i ==4){
    d$Idorsia.No <-d$Identifier
    
  }
  if(i ==6){
    d$Idorsia.No <-d$Identifier
    
  }
  d <- arrange(d,d$Idorsia.No)
  tem <- arrange(template, template$Idorsia.No)
  id <- intersect(id,rango$Idorsia.No)
  rango <- arrange(rango,rango$Idorsia.No)
  rango <- rango[rango$Idorsia.No%in%id,]
  rango <- arrange(rango,rango$Idorsia.No)
  d <- arrange(d,d$Idorsia.No)
  tem <- arrange(tem, tem$Idorsia.No)
  sum(tem$Idorsia.No==d$Idorsia.No)
  id <- intersect(d$Idorsia.No,tem$Idorsia.No)
  d <- d[d$Idorsia.No%in%id,]
  tem <- tem[tem$Idorsia.No%in%id,]
  d <- arrange(d,d$Idorsia.No)
  tem <- arrange(tem, tem$Idorsia.No)
  sum(tem$Idorsia.No==d$Idorsia.No)
  
  x <- tem$KineticIV.Clearance..mL.min.kg.
  y <- as.numeric(d$CL_rat.dose_mg_i.v)*66.6666666668
  
  # df <- data.frame(x,y)
  # df$y <- ifelse(df$y==0,NA,df$y)
  # df$x <- ifelse(df$x==0,NA,df$x)
  df <- data.frame(x,y,chem_class)
  df$chem_class <- ifelse(df$chem_class=="",NA,df$chem_class)
  df$y <- ifelse(df$y==0,NA,df$y)
  df$x <- ifelse(df$x==0,NA,df$x) 
  
  # df <- data.frame(x = tem$KineticIV.Clearance..mL.min.kg., y = as.numeric(d$CLp))
  
  
  
  # Iterate over names and create plot for each name
  plot_list <- ggplot(data = df, mapping = aes(x = df$x,y = df$y))+
    geom_point(mapping = aes(y=df$y,x=df$x), colour = "#529EFF")+
    scale_y_log10(limits = c(0.01,1000), labels = comma)+
    scale_x_log10(limits = c(0.01,1000), labels = comma)+
    geom_abline(intercept = 0, slope = 1)+
    stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
    stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
    stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
    stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
    geom_smooth(method=lm)+
    geom_point(aes(color=chem_class,group=chem_class))+
    guides(color="none")+
    
    labs(
      title = paste(names_vec[i]),
      subtitle = "",
      x = "observed CLp [mL/min/kg]",
      y = "predicted CLp [mL/min/kg]")
  
  ggsave(plot = plot_list, file = paste(names_vec[i], ".png", sep = ""), width = 10, height = 10)
  
}
