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
template <- read.csv("~/Big Dataset/p.o/template.csv")

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
setwd("~/Big Dataset/chem_class")
#IVIVE

dl <- list(insilico_logd_rh,insilico_logp_rh,insilico_logd_rlm,insilico_logp_rlm, BtP_logD_RH, BtP_logD_RLM, BtP_RH, BtP_RLM, fup_BtP_logD_RH, fup_BtP_logD_RLM, fup_BtP_RH, fup_BtP_RLM, fup_logD_RH, fup_logD_RLM, fup_RH, fup_RLM, logD_RH, logD_RLM)
names  <- c("Simulation All In Silico with logD (RH)", "Simulation All In Silico with logP (RH)", "Simulation All In Silico with logD (RLM)", "Simulation All In Silico with logP (RLM)","BtP_logD_RH", "BtP_logD_RLM", "BtP_RH", "BtP_RLM", "fup_BtP_logD_RH", "fup_BtP_logD_RLM", "fup_BtP_RH", "fup_BtP_RLM", "fup_logD_RH", "fup_logD_RLM", "fup_RH", "fup_RLM", "logD_RH", "logD_RLM")

names_vec  <- c("Simulation All In Silico with logD (RH)", "Simulation All In Silico with logP (RH)", "Simulation All In Silico with logD (RLM)", "Simulation All In Silico with logP (RLM)","BtP_logD_RH", "BtP_logD_RLM", "BtP_RH", "BtP_RLM", "fup_BtP_logD_RH", "fup_BtP_logD_RLM", "fup_BtP_RH", "fup_BtP_RLM", "fup_logD_RH", "fup_logD_RLM", "fup_RH", "fup_RLM", "logD_RH", "logD_RLM")


# Create empty list to store plots
plot_list <- list()
sim_mat <- matrix(data = NA, nrow = 18, ncol = 3, byrow = FALSE, dimnames = NULL)
for(i in 1:18){
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
  id <- intersect(d$Idorsia.No,tem$Idorsia.No)
  
  id <- intersect(id,rango$Idorsia.No)
  rango <- arrange(rango,rango$Idorsia.No)
  rango <- rango[rango$Idorsia.No%in%id,]
  rango <- arrange(rango,rango$Idorsia.No)
  d <- arrange(d,d$Idorsia.No)
  tem <- arrange(tem, tem$Idorsia.No)
  sum(tem$Idorsia.No==d$Idorsia.No)
  d <- arrange(d,d$Idorsia.No)
  rango <- arrange(rango,rango$Idorsia.No)
  tem <- arrange(template, template$Idorsia.No)
  dm <- arrange(dm, dm$Idorsia.No)
  id <- intersect(dm$Idorsia.No,tem$Idorsia.No)
  id <- intersect(d$Idorsia.No,tem$Idorsia.No)
  id <- intersect(id,rango$Idorsia.No)
  d <- d[d$Idorsia.No%in%id,]
  dm <- dm[dm$Idorsia.No%in%id,]
  
  d <- d[d$Idorsia.No%in%id,]
  tem <- tem[tem$Idorsia.No%in%id,]
  d <- arrange(d,d$Idorsia.No)
  tem <- arrange(tem, tem$Idorsia.No)
  sum(tem$Idorsia.No==d$Idorsia.No)
  
  x <- as.numeric(tem$KineticIV.Clearance..mL.min.kg.)
  y <- as.numeric(d$CL_rat.dose_mg_i.v) * 66.6666666668
  
  
  
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
  
  # extract slope from ggplot
  # model <- lm(data = df, formula = log10(y) ~ log10(x))
  # slope <- summary(model)$coefficients[2]
  
  # print slope
  # print(slope)
  ggsave(plot = plot_list, file = paste(names_vec[i], "Clearance_mL_min_kg.png", sep = ""), width = 10, height = 10)
  # df <- na.omit(df)
  # print(paste(max(df),min(df)))
  
  # sim_mat[i,] <- c(afe(df$x,df$y), aafe(df$x,df$y),slope)
}
# colnames(sim_mat) <- c("afe","aafe","slope")
# rownames(sim_mat) <-(c(names_vec))
# sim_mat

# stargazer(sim_mat)
# write.csv(sim_mat,"sim_mat.csv")

# AUC

# Create empty list to store plots
plot_list <- list()
sim_mat_auc <- matrix(data = NA, nrow = 18, ncol = 3, byrow = FALSE, dimnames = NULL)
for(i in 1:18){
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
  id <- intersect(d$Idorsia.No,tem$Idorsia.No)
  
  id <- intersect(id,rango$Idorsia.No)
  rango <- arrange(rango,rango$Idorsia.No)
  rango <- rango[rango$Idorsia.No%in%id,]
  rango <- arrange(rango,rango$Idorsia.No)
  d <- arrange(d,d$Idorsia.No)
  tem <- arrange(tem, tem$Idorsia.No)
  sum(tem$Idorsia.No==d$Idorsia.No)
  d <- arrange(d,d$Idorsia.No)
  rango <- arrange(rango,rango$Idorsia.No)
  tem <- arrange(template, template$Idorsia.No)
  dm <- arrange(dm, dm$Idorsia.No)
  id <- intersect(dm$Idorsia.No,tem$Idorsia.No)
  id <- intersect(d$Idorsia.No,tem$Idorsia.No)
  id <- intersect(id,rango$Idorsia.No)
  d <- d[d$Idorsia.No%in%id,]
  dm <- dm[dm$Idorsia.No%in%id,]
  d <- d[d$Idorsia.No%in%id,]
  tem <- tem[tem$Idorsia.No%in%id,]
  d <- arrange(d,d$Idorsia.No)
  tem <- arrange(tem, tem$Idorsia.No)
  sum(tem$Idorsia.No==d$Idorsia.No)
  
  x <- as.numeric(tem$KineticIV.AUC..ng.h.mL.)
  y <- as.numeric(d$AUC_rat.dose_mg_i.v)
  
  df <- data.frame(x,y)
  df$y <- ifelse(df$y==0,NA,df$y)
  df$x <- ifelse(df$x==0,NA,df$x)
  df <- data.frame(x,y,chem_class)
  df$chem_class <- ifelse(df$chem_class=="",NA,df$chem_class)
  df$y <- ifelse(df$y==0,NA,df$y)
  df$x <- ifelse(df$x==0,NA,df$x)
  
  # df <- data.frame(x = tem$KineticIV.Clearance..mL.min.kg., y = as.numeric(d$CLp))
  
  
  
  # Iterate over names and create plot for each name
  plot_list <- ggplot(data = df, mapping = aes(x = df$x,y = df$y))+
    geom_point(mapping = aes(y=df$y,x=df$x), colour = "#529EFF")+
    scale_y_log10(limits = c(0.01,2500000), labels = comma)+
    scale_x_log10(limits = c(0.01,2500000), labels = comma)+
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
      x = "observed AUC [ng*h/mL]",
      y = "predicted AUC [ng*h/mL]")
  
  # extract slope from ggplot
  # model <- lm(data = df, formula = log10(y) ~ log10(x))
  # slope <- summary(model)$coefficients[2]
  # 
  # # print slope
  # print(slope)
  ggsave(plot = plot_list, file = paste(names_vec[i], "KineticIV_AUC_ng_h_mL.png", sep = ""), width = 10, height = 10)
  # df <- na.omit(df)
  # print(paste(max(df),min(df)))
  # 
  # sim_mat_auc[i,] <- c(afe(df$x,df$y), aafe(df$x,df$y),slope)
}
# colnames(sim_mat_auc) <- c("afe","aafe","slope")
# rownames(sim_mat_auc) <-(c(names_vec))
# sim_mat_auc
# 
# stargazer(sim_mat_auc)
# write.csv(sim_mat_auc,"sim_mat_auc.csv")

# Vss

# Create empty list to store plots
plot_list <- list()
sim_mat_Vss <- matrix(data = NA, nrow = 18, ncol = 3, byrow = FALSE, dimnames = NULL)
for(i in 1:18){
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
  id <- intersect(d$Idorsia.No,tem$Idorsia.No)
  
  id <- intersect(id,rango$Idorsia.No)
  rango <- arrange(rango,rango$Idorsia.No)
  rango <- rango[rango$Idorsia.No%in%id,]
  rango <- arrange(rango,rango$Idorsia.No)
  d <- arrange(d,d$Idorsia.No)
  tem <- arrange(tem, tem$Idorsia.No)
  sum(tem$Idorsia.No==d$Idorsia.No)
  d <- arrange(d,d$Idorsia.No)
  rango <- arrange(rango,rango$Idorsia.No)
  tem <- arrange(template, template$Idorsia.No)
  dm <- arrange(dm, dm$Idorsia.No)
  id <- intersect(dm$Idorsia.No,tem$Idorsia.No)
  id <- intersect(d$Idorsia.No,tem$Idorsia.No)
  id <- intersect(id,rango$Idorsia.No)
  d <- d[d$Idorsia.No%in%id,]
  dm <- dm[dm$Idorsia.No%in%id,]
  d <- d[d$Idorsia.No%in%id,]
  tem <- tem[tem$Idorsia.No%in%id,]
  d <- arrange(d,d$Idorsia.No)
  tem <- arrange(tem, tem$Idorsia.No)
  sum(tem$Idorsia.No==d$Idorsia.No)
  
  x <- as.numeric(tem$KineticIV.Vss..L.kg.)
  y <- as.numeric(d$S.rVd_PBPK)
  
  df <- data.frame(x,y)
  df$y <- ifelse(df$y==0,NA,df$y)
  df$x <- ifelse(df$x==0,NA,df$x)
  # df <- data.frame(x = tem$KineticIV.Clearance..mL.min.kg., y = as.numeric(d$CLp))
  df <- data.frame(x,y,chem_class)
  df$chem_class <- ifelse(df$chem_class=="",NA,df$chem_class)
  df$y <- ifelse(df$y==0,NA,df$y)
  df$x <- ifelse(df$x==0,NA,df$x)
  
  
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
      x = "observed Vss [L/kg]",
      y = "predicted Vss [L/kg]")
  
  # extract slope from ggplot
  # model <- lm(data = df, formula = log10(y) ~ log10(x))
  # slope <- summary(model)$coefficients[2]
  
  # print slope
  # print(slope)
  ggsave(plot = plot_list, file = paste(names_vec[i], "Vss.png", sep = ""), width = 10, height = 10)
  # df <- na.omit(df)
  # sim_mat_Vss[i,] <- c(afe(df$x,df$y), aafe(df$x,df$y),slope)
  # print(paste(max(df),min(df)))
}
# colnames(sim_mat_Vss) <- c("afe","aafe","slope")
# rownames(sim_mat_Vss) <-(c(names_vec))
# sim_mat_Vss

stargazer(sim_mat_Vss)
write.csv(sim_mat_Vss,"sim_mat_Vss.csv")

# t_half

# Create empty list to store plots
plot_list <- list()
sim_mat_t_half <- matrix(data = NA, nrow = 18, ncol = 3, byrow = FALSE, dimnames = NULL)
for(i in 1:18){
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
  id <- intersect(d$Idorsia.No,tem$Idorsia.No)
  
  id <- intersect(id,rango$Idorsia.No)
  rango <- arrange(rango,rango$Idorsia.No)
  rango <- rango[rango$Idorsia.No%in%id,]
  rango <- arrange(rango,rango$Idorsia.No)
  d <- arrange(d,d$Idorsia.No)
  tem <- arrange(tem, tem$Idorsia.No)
  sum(tem$Idorsia.No==d$Idorsia.No)
  d <- arrange(d,d$Idorsia.No)
  rango <- arrange(rango,rango$Idorsia.No)
  tem <- arrange(template, template$Idorsia.No)
  dm <- arrange(dm, dm$Idorsia.No)
  id <- intersect(dm$Idorsia.No,tem$Idorsia.No)
  id <- intersect(d$Idorsia.No,tem$Idorsia.No)
  id <- intersect(id,rango$Idorsia.No)
  d <- d[d$Idorsia.No%in%id,]
  dm <- dm[dm$Idorsia.No%in%id,]
  d <- d[d$Idorsia.No%in%id,]
  tem <- tem[tem$Idorsia.No%in%id,]
  d <- arrange(d,d$Idorsia.No)
  tem <- arrange(tem, tem$Idorsia.No)
  sum(tem$Idorsia.No==d$Idorsia.No)
  
  x <- as.numeric(tem$KineticIV.Thalf..h.)
  y <- as.numeric(d$THalf_rat.dose_mg_i.v)
  
  df <- data.frame(x,y)
  df$y <- ifelse(df$y==0,NA,df$y)
  df$x <- ifelse(df$x==0,NA,df$x)
  
  df <- data.frame(x,y,chem_class)
  df$chem_class <- ifelse(df$chem_class=="",NA,df$chem_class)
  df$y <- ifelse(df$y==0,NA,df$y)
  df$x <- ifelse(df$x==0,NA,df$x)
  
  # df <- data.frame(x = tem$KineticIV.Clearance..mL.min.kg., y = as.numeric(d$CLp))
  
  
  
  # Iterate over names and create plot for each name
  plot_list <- ggplot(data = df, mapping = aes(x = df$x,y = df$y))+
    geom_point(mapping = aes(y=df$y,x=df$x), colour = "#529EFF")+
    scale_y_log10(limits = c(0.01,100), labels = comma)+
    scale_x_log10(limits = c(0.01,100), labels = comma)+
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
      x = "observed Thalf [h]",
      y = "predicted Thalf [h]")
  
  # extract slope from ggplot
  # model <- lm(data = df, formula = log10(y) ~ log10(x))
  # slope <- summary(model)$coefficients[2]
  
  # print slope
  # print(slope)
  ggsave(plot = plot_list, file = paste(names_vec[i], "Thalf.png", sep = ""), width = 10, height = 10)
  # df <- na.omit(df)
  # sim_mat_t_half[i,] <- c(afe(df$x,df$y), aafe(df$x,df$y),slope)
  # print(paste(max(df),min(df)))
}
colnames(sim_mat_t_half) <- c("afe","aafe","slope")
rownames(sim_mat_t_half) <-(c(names_vec))
sim_mat_t_half

stargazer(sim_mat_t_half)
write.csv(sim_mat_t_half,"sim_mat_t_half.csv")














