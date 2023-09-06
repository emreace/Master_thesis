
setwd("~/Big Dataset/Simulations/fed properties fup,RBP,logD")
P.O.5 <- read.csv("~/Big Dataset/p.o/P.O.5/P.O.5.csv")
P.O.6 <- read.csv("~/Big Dataset/p.o/P.O.6/P.O.6.csv")
P.O.1 <- read.csv("~/Big Dataset/p.o/P.O.1/P.O.1.csv")
P.O.2 <- read.csv("~/Big Dataset/p.o/P.O.2/P.O.2.csv")
P.O.3 <- read.csv("~/Big Dataset/p.o/P.O.3/P.O.3.csv")
P.O.4 <- read.csv("~/Big Dataset/p.o/P.O.4/P.O.4.csv")
setwd("~/Big Dataset/Simulations/fed properties fup,RBP,logD")

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
setwd("~/Big Dataset/graphs and errors")


dl <- list(P.O.1,P.O.2,P.O.3,P.O.4)

names <- c("P.O.1","P.O.2","P.O.3","P.O.4")
names_vec   <- c("P.O.1","P.O.2","P.O.3","P.O.4")


# Create empty list to store plots
plot_list <- list()
sim_po_F <- matrix(data = NA, nrow = 4, ncol = 3, byrow = FALSE, dimnames = NULL)
for(i in 1:4){
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
  
  
  d <- d[d$Idorsia.No%in%id,]
  tem <- tem[tem$Idorsia.No%in%id,]
  d <- arrange(d,d$Idorsia.No)
  tem <- arrange(tem, tem$Idorsia.No)
  sum(tem$Idorsia.No==d$Idorsia.No)
  
  x <- as.numeric(tem$p.o_merged_F)
  y <- as.numeric(d$X.Fb_rat.Dose_mg_p.o) 
  
  df <- data.frame(x,y)
  df$y <- ifelse(df$y==0,NA,df$y)
  df$x <- ifelse(df$x==0,NA,df$x)
  # df <- data.frame(x = tem$KineticIV.Clearance..mL.min.kg., y = as.numeric(d$CLp))
  df <- data.frame(x,y,Ionization)
  df$Ionization <- ifelse(df$Ionization=="",NA,df$Ionization)
  df$y <- ifelse(df$y==0,NA,df$y)
  df$x <- ifelse(df$x==0,NA,df$x)
  
  
  # Iterate over names and create plot for each name
  plot_list <- ggplot(data = df, mapping = aes(x = df$x,y = df$y))+
    geom_point(mapping = aes(y=df$y,x=df$x), colour = "#529EFF")+
    scale_y_continuous(limits = c(0.01,1100), labels = comma)+
    scale_x_continuous(limits = c(0.01,1100), labels = comma)+
    geom_abline(intercept = 0, slope = 1)+
    stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
    stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
    stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
    stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
    geom_smooth(method=lm)+
    geom_point(aes(colour=Ionization, group=Ionization))+
    theme_classic()+
    labs(
      title = paste(names_vec[i]),
      subtitle = "",
      x = "observed F [%]",
      y = "predicted F [%]")
  
  # extract slope from ggplot
  # model <- lm(data = df, formula = (y) ~ (x))
  # slope <- summary(model)$coefficients[2]
  # 
  # # print slope
  # print(slope)
  ggsave(plot = plot_list, file = paste(names_vec[i], "po_F.png", sep = ""), width = 10, height = 10)
  # df <- na.omit(df)
  # print(paste(max(df),min(df)))
  # 
  # sim_po_F[i,] <- c(afe(df$x,df$y), aafe(df$x,df$y),slope)
}
# colnames(sim_po_F) <- c("afe","aafe","slope")
# rownames(sim_po_F) <-(c(names_vec))
# sim_po_F
# 
# stargazer(sim_po_F)
# write.csv(sim_po_F,"sim_po_F.csv")

# AUC

# Create empty list to store plots
plot_list <- list()
sim_po_auc <- matrix(data = NA, nrow = 4, ncol = 3, byrow = FALSE, dimnames = NULL)
for(i in 1:4){
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
  
  
  d <- d[d$Idorsia.No%in%id,]
  tem <- tem[tem$Idorsia.No%in%id,]
  d <- arrange(d,d$Idorsia.No)
  tem <- arrange(tem, tem$Idorsia.No)
  sum(tem$Idorsia.No==d$Idorsia.No)
  
  x <- as.numeric(tem$p.o_merged_AUC)
  y <- as.numeric(d$AUCinf_rat.Dose_mg_p.o)
  
  df <- data.frame(x,y)
  df$y <- ifelse(df$y==0,NA,df$y)
  df$x <- ifelse(df$x==0,NA,df$x)
  # df <- data.frame(x = tem$KineticIV.Clearance..mL.min.kg., y = as.numeric(d$CLp))
  df <- data.frame(x,y,Ionization)
  df$Ionization <- ifelse(df$Ionization=="",NA,df$Ionization)
  df$y <- ifelse(df$y==0,NA,df$y)
  df$x <- ifelse(df$x==0,NA,df$x)
  
  
  
  # Iterate over names and create plot for each name
  plot_list <- ggplot(data = df, mapping = aes(x = df$x,y = df$y))+
    geom_point(mapping = aes(y=df$y,x=df$x), colour = "#529EFF")+
    scale_y_log10(limits = c(0.01,5000000), labels = comma)+
    scale_x_log10(limits = c(0.01,5000000), labels = comma)+
    geom_abline(intercept = 0, slope = 1)+
    stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
    stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
    stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
    stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
    geom_smooth(method=lm)+
    geom_point(aes(colour=Ionization, group=Ionization))+
    theme_classic()+
    labs(
      title = paste(names_vec[i]),
      subtitle = "",
      x = "observed AUCinf [ng*h/mL]",
      y = "predicted AUCinf [ng*h/mL]")
  
  # extract slope from ggplot
  # model <- lm(data = df, formula = log10(y) ~ log10(x))
  # slope <- summary(model)$coefficients[2]
  # 
  # # print slope
  # print(slope)
  ggsave(plot = plot_list, file = paste(names_vec[i], "po_auc.png", sep = ""), width = 10, height = 10)
  # df <- na.omit(df)
  # print(paste(max(df),min(df)))
  
  # sim_po_auc[i,] <- c(afe(df$x,df$y), aafe(df$x,df$y),slope)
}
# colnames(sim_po_auc) <- c("afe","aafe","slope")
# rownames(sim_po_auc) <-(c(names_vec))
# sim_po_auc
# 
# stargazer(sim_po_auc)
# write.csv(sim_po_auc,"sim_po_auc.csv")

# cmax

# Create empty list to store plots
plot_list <- list()
sim_mat_cmax <- matrix(data = NA, nrow = 4, ncol = 3, byrow = FALSE, dimnames = NULL)
for(i in 1:4){
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
  
  
  d <- d[d$Idorsia.No%in%id,]
  tem <- tem[tem$Idorsia.No%in%id,]
  d <- arrange(d,d$Idorsia.No)
  tem <- arrange(tem, tem$Idorsia.No)
  sum(tem$Idorsia.No==d$Idorsia.No)
  
  x <- as.numeric(tem$p.o_merged_Cmax)
  y <- as.numeric(d$Cmax_rat.Dose_mg_p.o)
  
  df <- data.frame(x,y)
  df$y <- ifelse(df$y==0,NA,df$y)
  df$x <- ifelse(df$x==0,NA,df$x)
  # df <- data.frame(x = tem$KineticIV.Clearance..mL.min.kg., y = as.numeric(d$CLp))
  df <- data.frame(x,y,Ionization)
  df$Ionization <- ifelse(df$Ionization=="",NA,df$Ionization)
  df$y <- ifelse(df$y==0,NA,df$y)
  df$x <- ifelse(df$x==0,NA,df$x)
  
  
  # Iterate over names and create plot for each name
  plot_list <- ggplot(data = df, mapping = aes(x = df$x,y = df$y))+
    geom_point(mapping = aes(y=df$y,x=df$x), colour = "#529EFF")+
    scale_y_log10(limits = c(0.1,100000), labels = comma)+
    scale_x_log10(limits = c(0.1,100000), labels = comma)+
    geom_abline(intercept = 0, slope = 1)+
    stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
    stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
    stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
    stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
    geom_smooth(method=lm)+
    geom_point(aes(colour=Ionization, group=Ionization))+
    theme_classic()+
    labs(
      title = paste(names_vec[i]),
      subtitle = "",
      x = "observed cmax [ng/mL]",
      y = "predicted cmax [ng/mL]")
  
  # extract slope from ggplot
  # model <- lm(data = df, formula = log10(y) ~ log10(x))
  # slope <- summary(model)$coefficients[2]
  # 
  # # print slope
  # print(slope)
  ggsave(plot = plot_list, file = paste(names_vec[i], "cmax.png", sep = ""), width = 10, height = 10)
  # df <- na.omit(df)
  # sim_mat_cmax[i,] <- c(afe(df$x,df$y), aafe(df$x,df$y),slope)
  # print(paste(max(df),min(df)))
}
# colnames(sim_mat_cmax) <- c("afe","aafe","slope")
# rownames(sim_mat_cmax) <-(c(names_vec))
# sim_mat_cmax
# 
# stargazer(sim_mat_cmax)
# write.csv(sim_mat_cmax,"sim_mat_cmax.csv")

# t_max

# Create empty list to store plots
plot_list <- list()
sim_mat_t_max <- matrix(data = NA, nrow = 4, ncol = 3, byrow = FALSE, dimnames = NULL)
for(i in 1:4){
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
  
  
  d <- d[d$Idorsia.No%in%id,]
  tem <- tem[tem$Idorsia.No%in%id,]
  d <- arrange(d,d$Idorsia.No)
  tem <- arrange(tem, tem$Idorsia.No)
  sum(tem$Idorsia.No==d$Idorsia.No)
  
  x <- as.numeric(tem$p.o_merged_Tmax)
  y <- as.numeric(d$Tmax_rat.Dose_mg_p.o)
  
  df <- data.frame(x,y)
  df$y <- ifelse(df$y==0,NA,df$y)
  df$x <- ifelse(df$x==0,NA,df$x)
  # df <- data.frame(x = tem$KineticIV.Clearance..mL.min.kg., y = as.numeric(d$CLp))
  
  df <- data.frame(x,y,Ionization)
  df$Ionization <- ifelse(df$Ionization=="",NA,df$Ionization)
  df$y <- ifelse(df$y==0,NA,df$y)
  df$x <- ifelse(df$x==0,NA,df$x)
  
  
  
  # Iterate over names and create plot for each name
  plot_list <- ggplot(data = df, mapping = aes(x = df$x,y = df$y))+
    geom_point(mapping = aes(y=df$y,x=df$x), colour = "#529EFF")+
    scale_y_log10(limits = c(0.1,100), labels = comma)+
    scale_x_log10(limits = c(0.1,100), labels = comma)+
    geom_abline(intercept = 0, slope = 1)+
    stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
    stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
    stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
    stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
    geom_smooth(method=lm)+
    geom_point(aes(colour=Ionization, group=Ionization))+
    theme_classic()+
    labs(
      title = paste(names_vec[i]),
      subtitle = "",
      x = "observed Tmax [h]",
      y = "predicted Tmax [h]")
  
  # extract slope from ggplot
  # model <- lm(data = df, formula = log10(y) ~ log10(x))
  # slope <- summary(model)$coefficients[2]
  
  # print slope
  # print(slope)
  ggsave(plot = plot_list, file = paste(names_vec[i], "Tmax.png", sep = ""), width = 10, height = 10)
  # df <- na.omit(df)
  # sim_mat_t_max[i,] <- c(afe(df$x,df$y), aafe(df$x,df$y),slope)
  # print(paste(max(df),min(df)))
}
# colnames(sim_mat_t_max) <- c("afe","aafe","slope")
# rownames(sim_mat_t_max) <-(c(names_vec))
# sim_mat_t_max
# 
# stargazer(sim_mat_t_max)
# write.csv(sim_mat_t_max,"sim_mat_t_max.csv")
# 
# 




