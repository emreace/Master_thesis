
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

data <- All_final_to_share_template_onedose_RH_logP
data <- ERA_Rat_iv_PK_properties_reconstructed_v9_geomeans_cleaned_multiple_values_MDR1_pKa_merged_CLpmicros_CLphep_dosemg_ADMET7.4_PKSIM_RLM

fEccs <- function(x,y,z){
  d1 <- as.numeric(data[,x]) *0.015
  d2 <- as.numeric(data[,y])
  d3 <- data$Ionization_Class_Code==z
  
  dd <- data.frame(d1,d2,d3)
  dd <- subset(dd,dd$d3==TRUE)
  dd <- na.omit(dd)
  
  x1 <- afe(dd$d1,dd$d2)
  x2 <- aafe(dd$d1,dd$d2)
  return(data.frame(x1,x2))  
}

fEccs(x=which(colnames(data)=="KineticIV.Clearance..mL.min.kg."),y = which(colnames(data)=="CL_rat.dose_mg_i.v"),z = "Acid")

fEccs <- function(x,y,z){
  d1 <- as.numeric(data[,x]) *0.015
  d2 <- as.numeric(data[,y])
  d3 <- data$Ionization_Class_Code==z
  
  dd <- data.frame(d1,d2,d3)
  dd <- subset(dd,dd$d3==TRUE)
  dd <- na.omit(dd)
  
  x1 <- afe(dd$d1,dd$d2)
  x2 <- aafe(dd$d1,dd$d2)
  return(data.frame(x1,x2))  
}

fEccs(x=which(colnames(data)=="KineticIV.Clearance..mL.min.kg."),y = which(colnames(data)=="CL_rat.dose_mg_i.v"),z = "Base")

fEccs <- function(x,y,z){
  d1 <- as.numeric(data[,x]) *0.015
  d2 <- as.numeric(data[,y])
  d3 <- data$Ionization_Class_Code==z
  
  dd <- data.frame(d1,d2,d3)
  dd <- subset(dd,dd$d3==TRUE)
  dd <- na.omit(dd)
  
  x1 <- afe(dd$d1,dd$d2)
  x2 <- aafe(dd$d1,dd$d2)
  return(data.frame(x1,x2))  
}

fEccs(x=which(colnames(data)=="KineticIV.Clearance..mL.min.kg."),y = which(colnames(data)=="CL_rat.dose_mg_i.v"),z = "Neutral")
fEccs(x=which(colnames(data)=="KineticIV.Clearance..mL.min.kg."),y = which(colnames(data)=="CL_rat.dose_mg_i.v"),z = "Zwit")
# fEccs(x=which(colnames(data)=="KineticIV.Clearance..mL.min.kg."),y = which(colnames(data)=="CL_rat.dose_mg_i.v"),z = "Class_3B")
# fEccs(x=which(colnames(data)=="KineticIV.Clearance..mL.min.kg."),y = which(colnames(data)=="CL_rat.dose_mg_i.v"),z = "Class_4")









