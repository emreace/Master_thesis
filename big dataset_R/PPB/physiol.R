
setwd("~/Big Dataset/Simulations/ADMET Properties")
admed_physiol <-  read.csv("~/Big Dataset/Simulations/ADMET Properties/All_final_to_share_B2P%_7.4.csv")
solHCL <- read.csv("solHCL.csv")

setwd("~/Big Dataset/Simulations/fed properties fup,RBP,logD")
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
setwd("~/Big Dataset/PPB")



# 1
d <- admed_physiol
template <- solHCL

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

x <- as.numeric(tem$solHCl.solubility..ug.ml.)/1000
y <- as.numeric(template$S.S_pH) 

df <- data.frame(x,y)
df$y <- ifelse(df$y==0,NA,df$y)
df$x <- ifelse(df$x==0,NA,df$x)
# df <- data.frame(x = tem$KineticIV.Clearance..mL.min.kg., y = as.numeric(d$CLp))

df <- data.frame(x,y,PPB)
df$PPB <- ifelse(df$PPB=="",NA,df$PPB)
df$y <- ifelse(df$y==0,NA,df$y)
df$x <- ifelse(df$x==0,NA,df$x)


# Iterate over names and create plot for each name
plot_list <- ggplot(data = df, mapping = aes(x = df$x,y = df$y))+
  geom_point(mapping = aes(y=df$y,x=df$x), colour = "#529EFF")+
  scale_y_log10(limits = c(1.0e-05,1100), labels = comma)+
  scale_x_log10(limits = c(1.0e-05,1100), labels = comma)+
  geom_abline(intercept = 0, slope = 1)+
  stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
  stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
  stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
  geom_smooth(method=lm)+
  geom_point(aes(colour=PPB, group=PPB))+
  theme_classic()+
  labs(
    title = paste("solHCl"),
    subtitle = "",
    x = "observed solHCl [mg/mL]",
    y = "predicted solHCl [mg/mL]")



# # extract slope from ggplot
# model <- lm(data = df, formula = log10(y) ~ log10(x))
# slope <- summary(model)$coefficients[2]
# 
# 
# 
# 
# # print slope
# print(slope)
ggsave(plot = plot_list, file = paste(names_vec[i],"PPB", "solHCl.png", sep = ""), width = 10, height = 10)
# df <- na.omit(df)
# print(paste(max(df),min(df)))
# 
#  print(c(afe(df$x,df$y), aafe(df$x,df$y),slope))

# 2
 
 d <- admed_physiol
 template <- admed_physiol
 
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
 
 x <- as.numeric(tem$solFeSSIF.solubility..ug.ml.)/1000
 y <- as.numeric(d$S.FeSSIF) 
 
 df <- data.frame(x,y)
 df$y <- ifelse(df$y==0,NA,df$y)
 df$x <- ifelse(df$x==0,NA,df$x)
 # df <- data.frame(x = tem$KineticIV.Clearance..mL.min.kg., y = as.numeric(d$CLp))
 df <- data.frame(x,y,PPB)
 df$PPB <- ifelse(df$PPB=="",NA,df$PPB)
 df$y <- ifelse(df$y==0,NA,df$y)
 df$x <- ifelse(df$x==0,NA,df$x)
 
 
 # Iterate over names and create plot for each name
 plot_list <- ggplot(data = df, mapping = aes(x = df$x,y = df$y))+
   geom_point(mapping = aes(y=df$y,x=df$x), colour = "#529EFF")+
   scale_y_log10(limits = c(0.001,50), labels = comma)+
   scale_x_log10(limits = c(0.001,50), labels = comma)+
   geom_abline(intercept = 0, slope = 1)+
   stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
   stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
   stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
   stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
   geom_smooth(method=lm)+
   geom_point(aes(colour=PPB, group=PPB))+
   theme_classic()+
   labs(
     title = paste("solFeSSIF"),
     subtitle = "",
     x = "observed solFeSSIF [mg/mL]",
     y = "predicted solFeSSIF [mg/mL]")
 
 
 
 # # extract slope from ggplot
 # model <- lm(data = df, formula = log10(y) ~ log10(x))
 # slope <- summary(model)$coefficients[2]
 # 
 # # print slope
 # print(slope)
 ggsave(plot = plot_list, file = paste(names_vec[i],"PPB", "solFeSSIF.png", sep = ""), width = 10, height = 10)
 # df <- na.omit(df)
 # print(paste(max(df),min(df)))
 # 
 # print(c(afe(df$x,df$y), aafe(df$x,df$y),slope))
 # 
 # 3
 
 d <- admed_physiol
 template <- admed_physiol
 
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
 
 x <- as.numeric(tem$solFaSSIF.solubility..ug.ml.)/1000
 y <- as.numeric(d$S.FaSSIF) 
 
 df <- data.frame(x,y)
 df$y <- ifelse(df$y==0,NA,df$y)
 df$x <- ifelse(df$x==0,NA,df$x)
 # df <- data.frame(x = tem$KineticIV.Clearance..mL.min.kg., y = as.numeric(d$CLp))
 df <- data.frame(x,y,PPB)
 df$PPB <- ifelse(df$PPB=="",NA,df$PPB)
 df$y <- ifelse(df$y==0,NA,df$y)
 df$x <- ifelse(df$x==0,NA,df$x)
 
 
 
 # Iterate over names and create plot for each name
 plot_list <- ggplot(data = df, mapping = aes(x = df$x,y = df$y))+
   geom_point(mapping = aes(y=df$y,x=df$x), colour = "#529EFF")+
   scale_y_log10(limits = c(0.0001,60), labels = comma)+
   scale_x_log10(limits = c(0.0001,60), labels = comma)+
   geom_abline(intercept = 0, slope = 1)+
   stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
   stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
   stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
   stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
   geom_smooth(method=lm)+
   geom_point(aes(colour=PPB, group=PPB))+
   theme_classic()+
   labs(
     title = paste("solFaSSIF"),
     subtitle = "",
     x = "observed solFaSSIF [mg/mL]",
     y = "predicted solFaSSIF [mg/mL]")
 
 
 
 # # extract slope from ggplot
 # model <- lm(data = df, formula = log10(y) ~ log10(x))
 # slope <- summary(model)$coefficients[2]
 # 
 # # print slope
 # print(slope)
 ggsave(plot = plot_list, file = paste(names_vec[i],"PPB", "solFaSSIF.png", sep = ""), width = 10, height = 10)
 # df <- na.omit(df)
 # print(paste(max(df),min(df)))
 # 
 # print(c(afe(df$x,df$y), aafe(df$x,df$y),slope))
 # 4
 
 d <- admed_physiol
 template <- admed_physiol
 
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
 
 x <- as.numeric(tem$solPBS.solubility..ug.ml.)/1000
 y <- as.numeric(d$S.S_pH) 
 
 df <- data.frame(x,y)
 df$y <- ifelse(df$y==0,NA,df$y)
 df$x <- ifelse(df$x==0,NA,df$x)
 # df <- data.frame(x = tem$KineticIV.Clearance..mL.min.kg., y = as.numeric(d$CLp))
 df <- data.frame(x,y,PPB)
 df$PPB <- ifelse(df$PPB=="",NA,df$PPB)
 df$y <- ifelse(df$y==0,NA,df$y)
 df$x <- ifelse(df$x==0,NA,df$x)
 
 
 # Iterate over names and create plot for each name
 plot_list <- ggplot(data = df, mapping = aes(x = df$x,y = df$y))+
   geom_point(mapping = aes(y=df$y,x=df$x), colour = "#529EFF")+
   scale_y_log10(limits = c(0.00001,150), labels = comma)+
   scale_x_log10(limits = c(0.00001,150), labels = comma)+
   geom_abline(intercept = 0, slope = 1)+
   stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
   stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
   stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
   stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
   geom_smooth(method=lm)+
   geom_point(aes(colour=PPB, group=PPB))+
   theme_classic()+
   labs(
     title = paste("solPBS"),
     subtitle = "",
     x = "observed solPBS [mg/mL]",
     y = "predicted solPBS [mg/mL]")
 
 
 
 # # extract slope from ggplot
 # model <- lm(data = df, formula = log10(y) ~ log10(x))
 # slope <- summary(modepl)$coefficients[2]
 # 
 # # print slope
 # print(slope)
 ggsave(plot = plot_list, file = paste(names_vec[i],"PPB", "solPBS.png", sep = ""), width = 10, height = 10)
 # df <- na.omit(df)
 # print(paste(max(df),min(df)))
 # 
 # print(c(afe(df$x,df$y), aafe(df$x,df$y),slope))

 # 5
 
 
 d <- admed_physiol
 template <- admed_physiol
 
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
 
 x <- as.numeric(tem$logD.logD)
 y <- as.numeric(d$S.logD) 
 # x <- ifelse(x>0,x,NA)
 # y <- ifelse(y>0,y,NA)
 
 
 df <- data.frame(x,y)
 df$y <- ifelse(df$y==0,NA,df$y)
 df$x <- ifelse(df$x==0,NA,df$x)
 # df <- data.frame(x = tem$KineticIV.Clearance..mL.min.kg., y = as.numeric(d$CLp))
 df <- data.frame(x,y,PPB)
 df$PPB <- ifelse(df$PPB=="",NA,df$PPB)
 df$y <- ifelse(df$y==0,NA,df$y)
 df$x <- ifelse(df$x==0,NA,df$x)
 
 
 
 # Iterate over names and create plot for each name
 plot_list <- ggplot(data = df, mapping = aes(x = df$x,y = df$y))+
   geom_point(mapping = aes(y=df$y,x=df$x), colour = "#529EFF")+
   scale_x_continuous(limits = c(-10,10), labels = comma)+
   scale_y_continuous(limits = c(-10,10), labels = comma)+
   geom_abline(intercept = 0, slope = 1)+
   stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
   stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
   stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
   stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
   geom_smooth(method=lm)+
   geom_point(aes(colour=PPB, group=PPB))+
   theme_classic()+
   labs(
     title = paste("logD"),
     subtitle = "",
     x = "observed logD",
     y = "predicted logD")
 
 
 
 # # extract slope from ggplot
 # model <- lm(data = df, formula = y ~ x)
 # slope <- summary(model)$coefficients[2]
 # 
 # # print slope
 # print(slope)
 ggsave(plot = plot_list, file = paste(names_vec[i],"PPB", "logD.png", sep = ""), width = 10, height = 10)
 # df <- na.omit(df)
 # print(paste(max(df),min(df)))
 # 
 # print(c(afe(df$x,df$y), aafe(df$x,df$y),slope))
 
 # 6
 
 d <- admed_physiol
 template <- admed_physiol
 
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
 
 x <- as.numeric(tem$Hepatocyte.binding.fu.hep....)/100
 y <- as.numeric(d$fu.hep.Austin) 

 df <- data.frame(x,y)
 df$y <- ifelse(df$y==0,NA,df$y)
 
 df$x <- ifelse(df$x==0,NA,df$x)
 # df <- data.frame(x = tem$KineticIV.Clearance..mL.min.kg., y = as.numeric(d$CLp))
 
 df <- data.frame(x,y,PPB)
 df$PPB <- ifelse(df$PPB=="",NA,df$PPB)
 df$y <- ifelse(df$y==0,NA,df$y)
 df$x <- ifelse(df$x==0,NA,df$x)
 
 
 
 # Iterate over names and create plot for each name
 plot_list <- ggplot(data = df, mapping = aes(x = df$x,y = df$y))+
   geom_point(mapping = aes(y=df$y,x=df$x), colour = "#529EFF")+
   scale_x_log10(limits = c(0.001,2), labels = comma)+
   scale_y_log10(limits = c(0.001,2), labels = comma)+
   geom_abline(intercept = 0, slope = 1)+
   stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
   stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
   stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
   stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
   geom_smooth(method=lm)+
   geom_point(aes(colour=PPB, group=PPB))+
   theme_classic()+
   labs(
     title = paste("fuhep Austin"),
     subtitle = "",
     x = "observed fuhep",
     y = "predicted fuhep Austin")
 
 
 
 # # extract slope from ggplot
 # model <- lm(data = df, formula = y ~ x)
 # slope <- summary(model)$coefficients[2]
 # 
 # # print slope
 # print(slope)
 ggsave(plot = plot_list, file = paste(names_vec[i],"PPB", "fuhepAustin.png", sep = ""), width = 10, height = 10)
 # df <- na.omit(df)
 # print(paste(max(df),min(df)))
 # 
 # print(c(afe(df$x,df$y), aafe(df$x,df$y),slope))
 # 
 # 7
 
 d <- admed_physiol
 template <- admed_physiol
 
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
 
 x <- as.numeric(tem$Microsomal.binding.fu.mic....)/100
 y <- as.numeric(d$fumic_half_mg_ml_conc) 
 
 df <- data.frame(x,y)
 df$y <- ifelse(df$y==0,NA,df$y)
 df$x <- ifelse(df$x==0,NA,df$x)
 # df <- data.frame(x = tem$KineticIV.Clearance..mL.min.kg., y = as.numeric(d$CLp))
 df <- data.frame(x,y,PPB)
 df$PPB <- ifelse(df$PPB=="",NA,df$PPB)
 df$y <- ifelse(df$y==0,NA,df$y)
 df$x <- ifelse(df$x==0,NA,df$x)
 
 
 # Iterate over names and create plot for each name
 plot_list <- ggplot(data = df, mapping = aes(x = df$x,y = df$y))+
   geom_point(mapping = aes(y=df$y,x=df$x), colour = "#529EFF")+
   scale_x_log10(limits = c(0.01,2), labels = comma)+
   scale_y_log10(limits = c(0.01,2), labels = comma)+
   geom_abline(intercept = 0, slope = 1)+
   stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
   stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
   stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
   stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
   geom_smooth(method=lm)+
   geom_point(aes(colour=PPB, group=PPB))+
   theme_classic()+
   labs(
     title = paste("fumic_half"),
     subtitle = "",
     x = "observed fumic_half",
     y = "predicted fumic_half")
 
 
 
 # # extract slope from ggplot
 # model <- lm(data = df, formula = y ~ x)
 # slope <- summary(model)$coefficients[2]
 # 
 # # print slope
 # print(slope)
 ggsave(plot = plot_list, file = paste(names_vec[i],"PPB", "fumic_half.png", sep = ""), width = 10, height = 10)
 # df <- na.omit(df)
 # print(paste(max(df),min(df)))
 # 
 # print(c(afe(df$x,df$y), aafe(df$x,df$y),slope))
 
 # 8
 
 d <- admed_physiol
 template <- admed_physiol
 
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
 
 x <- as.numeric(tem$PlasmaProteinBinding.fu.plasma....)/100
 y <- as.numeric(d$rat_fup.)/100 
 
 df <- data.frame(x,y)
 df$y <- ifelse(df$y==0,NA,df$y)
 df$x <- ifelse(df$x==0,NA,df$x)
 # df <- data.frame(x = tem$KineticIV.Clearance..mL.min.kg., y = as.numeric(d$CLp))
 df <- data.frame(x,y,PPB)
 df$PPB <- ifelse(df$PPB=="",NA,df$PPB)
 df$y <- ifelse(df$y==0,NA,df$y)
 df$x <- ifelse(df$x==0,NA,df$x)
 
 
 # Iterate over names and create plot for each name
 plot_list <- ggplot(data = df, mapping = aes(x = df$x,y = df$y))+
   geom_point(mapping = aes(y=df$y,x=df$x), colour = "#529EFF")+
   scale_x_log10(limits = c(0.001,1), labels = comma)+
   scale_y_log10(limits = c(0.001,1), labels = comma)+
   geom_abline(intercept = 0, slope = 1)+
   stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
   stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
   stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
   stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
   geom_smooth(method=lm)+
   geom_point(aes(colour=PPB, group=PPB))+
   theme_classic()+
   labs(
     title = paste("fup"),
     subtitle = "",
     x = "observed fup",
     y = "predicted fup")
 
 
 
 # # extract slope from ggplot
 # model <- lm(data = df, formula = y ~ x)
 # slope <- summary(model)$coefficients[2]
 # 
 # # print slope
 # print(slope)
 ggsave(plot = plot_list, file = paste(names_vec[i],"PPB", "rat_fup.png", sep = ""), width = 10, height = 10)
 # df <- na.omit(df)
 # print(paste(max(df),min(df)))
 # 
 # print(c(afe(df$x,df$y), aafe(df$x,df$y),slope))
 # 
 # 9
 template_all_geomean_predictions_fraction <- read.csv("~/Big Dataset/template_all_geomean_predictions_fraction.csv")
 
 
 d <- template_all_geomean_predictions_fraction
 template <- admed_physiol
 
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
 
 x <- as.numeric(d$Blood.Plasma)
 y <- as.numeric(d$RBP_rat)
 
 df <- data.frame(x,y)
 df$y <- ifelse(df$y==0,NA,df$y)
 df$x <- ifelse(df$x==0,NA,df$x)
 # df <- data.frame(x = tem$KineticIV.Clearance..mL.min.kg., y = as.numeric(d$CLp))
 df <- data.frame(x,y,PPB)
 df$PPB <- ifelse(df$PPB=="",NA,df$PPB)
 df$y <- ifelse(df$y==0,NA,df$y)
 df$x <- ifelse(df$x==0,NA,df$x)
 
 
 # Iterate over names and create plot for each name
 plot_list <- ggplot(data = df, mapping = aes(x = df$x,y = df$y))+
   geom_point(mapping = aes(y=df$y,x=df$x), colour = "#529EFF")+
   scale_x_log10(limits = c(0.1,50), labels = comma)+
   scale_y_log10(limits = c(0.1,50), labels = comma)+
   geom_abline(intercept = 0, slope = 1)+
   stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
   stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
   stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
   stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
   geom_smooth(method=lm)+
   geom_point(aes(colour=PPB, group=PPB))+
   theme_classic()+
   labs(
     title = paste("RBP"),
     subtitle = "",
     x = "observed RBP",
     y = "predicted RBP")
 
 
 
 # # extract slope from ggplot
 # model <- lm(data = df, formula = y ~ x)
 # slope <- summary(model)$coefficients[2]
 # 
 # # print slope
 # print(slope)
 ggsave(plot = plot_list, file = paste(names_vec[i],"PPB", "RBP.png", sep = ""), width = 10, height = 10)
 # df <- na.omit(df)
 # print(paste(max(df),min(df)))
 # 
 # print(c(afe(df$x,df$y), aafe(df$x,df$y),slope))
 # 
 # 10
 template_all_geomean_predictions_fraction <- read.csv("~/Big Dataset/template_all_geomean_predictions_fraction.csv")
 
 
 d <- template_all_geomean_predictions_fraction
 template <- admed_physiol
 
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
 
 x <- as.numeric(d$MetStabRatMicros.intrClearance..ul..min.mg..)*1.66/((as.numeric(d$Microsomal.binding.fu.mic....)/100))
 y <- as.numeric(d$CYP_RLM_CLint)*1.66
 
 df <- data.frame(x,y)
 df$y <- ifelse(df$y==0,NA,df$y)
 df$x <- ifelse(df$x==0,NA,df$x)
 # df <- data.frame(x = tem$KineticIV.Clearance..mL.min.kg., y = as.numeric(d$CLp))
 df <- data.frame(x,y,PPB)
 df$PPB <- ifelse(df$PPB=="",NA,df$PPB)
 df$y <- ifelse(df$y==0,NA,df$y)
 df$x <- ifelse(df$x==0,NA,df$x)
 
 
 
 # Iterate over names and create plot for each name
 plot_list <- ggplot(data = df, mapping = aes(x = df$x,y = df$y))+
   geom_point(mapping = aes(y=df$y,x=df$x), colour = "#529EFF")+
   scale_x_log10(limits = c(10,15000), labels = comma)+
   scale_y_log10(limits = c(10,15000), labels = comma)+
   geom_abline(intercept = 0, slope = 1)+
   stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
   stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
   stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
   stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
   geom_smooth(method=lm)+
   geom_point(aes(colour=PPB, group=PPB))+
   theme_classic()+
   labs(
     title = paste("CLintu_RLM"),
     subtitle = "",
     x = "observed CLintu_RLM",
     y = "predicted CLintu_RLM")
 
 
 
 # # extract slope from ggplot
 # model <- lm(data = df, formula = y ~ x)
 # slope <- summary(model)$coefficients[2]
 # 
 # # print slope
 # print(slope)
 ggsave(plot = plot_list, file = paste(names_vec[i],"PPB", "CLintu_RLM.png", sep = ""), width = 10, height = 10)
 # df <- na.omit(df)
 # print(paste(max(df),min(df)))
 # 
 # print(c(afe(df$x,df$y), aafe(df$x,df$y),slope))
 # 11
 
 template_all_geomean_predictions_fraction <- read.csv("~/Big Dataset/template_all_geomean_predictions_fraction.csv")
 
 
 d <- template_all_geomean_predictions_fraction
 template <- admed_physiol
 
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
 
 x <- as.numeric(d$MetStabRatHepSusp.intrClearance..ul..min.1E6.cells..)*3.89/(as.numeric(d$Hepatocyte.binding.fu.hep....)/100)
 y <- as.numeric(d$HEP_rCLint)*3.89
 
 df <- data.frame(x,y)
 df$y <- ifelse(df$y==0,NA,df$y)
 df$x <- ifelse(df$x==0,NA,df$x)
 # df <- data.frame(x = tem$KineticIV.Clearance..mL.min.kg., y = as.numeric(d$CLp))
 df <- data.frame(x,y,PPB)
 df$PPB <- ifelse(df$PPB=="",NA,df$PPB)
 df$y <- ifelse(df$y==0,NA,df$y)
 df$x <- ifelse(df$x==0,NA,df$x)
 
 
 # Iterate over names and create plot for each name
 plot_list <- ggplot(data = df, mapping = aes(x = df$x,y = df$y))+
   geom_point(mapping = aes(y=df$y,x=df$x), colour = "#529EFF")+
   scale_x_log10(limits = c(6,18000), labels = comma)+
   scale_y_log10(limits = c(6,18000), labels = comma)+
   geom_abline(intercept = 0, slope = 1)+
   stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
   stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
   stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
   stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
   geom_smooth(method=lm)+
   geom_point(aes(colour=PPB, group=PPB))+
   theme_classic()+
   labs(
     title = paste("CLintu_RH"),
     subtitle = "",
     x = "observed CLintu_RH",
     y = "predicted CLintu_RH")
 
 
 
 # # extract slope from ggplot
 # model <- lm(data = df, formula = y ~ x)
 # slope <- summary(model)$coefficients[2]
 # 
 # # print slope
 # print(slope)
 ggsave(plot = plot_list, file = paste(names_vec[i],"PPB", "CLintu_RH.png", sep = ""), width = 10, height = 10)
 # df <- na.omit(df)
 # print(paste(max(df),min(df)))
 # 
 # print(c(afe(df$x,df$y), aafe(df$x,df$y),slope))
 # 
 # 
 # 
 
 # 12
 
 template <- read.csv("~/Big Dataset/p.o/template.csv")
 
 # template_all_geomean_predictions_fraction <- read.csv("~/Big Dataset/template_all_geomean_predictions_fraction.csv")
 # 
 # setwd("~/Big Dataset/IVIVE")
 
 ivive <- read.csv("~/Big Dataset/IVIVE/CLint_obsu_rückgerechnet.csv")
 # setwd("~/Big Dataset/graphs and errors")
 
 d <- ivive
 template <- admed_physiol
 
 d <- arrange(d,d$Idorsia.No)
 tem <- arrange(template, template$Idorsia.No)
 id <- intersect(d$Idorsia.No,tem$Idorsia.No)
 
 
 id <- intersect(id,rango$Idorsia.No)
 d <- d[d$Idorsia.No%in%id,]
 tem <- tem[tem$Idorsia.No%in%id,]
 rango <- arrange(rango,rango$Idorsia.No)
 rango <- rango[rango$Idorsia.No%in%id,]
 rango <- arrange(rango,rango$Idorsia.No)
 d <- arrange(d,d$Idorsia.No)
 tem <- arrange(tem, tem$Idorsia.No)
 sum(tem$Idorsia.No==d$Idorsia.No)
 

 d <- arrange(d,d$Idorsia.No)
 tem <- arrange(tem, tem$Idorsia.No)
 sum(tem$Idorsia.No==d$Idorsia.No)
 
 x <- as.numeric(template$MetStabRatHepSusp.intrClearance..ul..min.1E6.cells..)*3.89/(as.numeric(template$Hepatocyte.binding.fu.hep....)/100)
 y <- as.numeric(d$Clintu_obs_backcalc)
 
 df <- data.frame(x,y)
 df$y <- ifelse(df$y==0,NA,df$y)
 df$x <- ifelse(df$x==0,NA,df$x)
 # df <- data.frame(x = tem$KineticIV.Clearance..mL.min.kg., y = as.numeric(d$CLp))
 df <- data.frame(x,y,PPB)
 df$PPB <- ifelse(df$PPB=="",NA,df$PPB)
 df$y <- ifelse(df$y==0,NA,df$y)
 df$x <- ifelse(df$x==0,NA,df$x)
 
 
 # Iterate over names and create plot for each name
 plot_list <- ggplot(data = df, mapping = aes(x = df$x,y = df$y))+
   geom_point(mapping = aes(y=df$y,x=df$x), colour = "#529EFF")+
   scale_x_log10(limits = c(120,2500), labels = comma)+
   scale_y_log10(limits = c(120,2500), labels = comma)+
   geom_abline(intercept = 0, slope = 1)+
   stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
   stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
   stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
   stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
   geom_smooth(method=lm)+
   geom_point(aes(colour=PPB, group=PPB))+
   
   theme_classic()+
   labs(
     title = paste("CLintu_back_RH"),
     subtitle = "",
     x = "observed CLintu_RH [µL/min/kg]",
     y = "predicted CLintu_RH [µL/min/kg]")
 
 
 # extract slope from ggplot
 # model <- lm(data = df, formula = y ~ x)
 # slope <- summary(model)$coefficients[2]
 
 # print slope
 # print(slope)
 ggsave(plot = plot_list, file = paste(names_vec[i],"PPB", "CLintu_back_RH.png", sep = ""), width = 10, height = 10)
 # df <- na.omit(df)
 # print(paste(max(df),min(df)))
 # z12 <- (c(afe(df$x,df$y), aafe(df$x,df$y),slope))
 # 
 # 
 # 13
 
 
 d <- ivive
 template <- admed_physiol
 
 d <- arrange(d,d$Idorsia.No)
 tem <- arrange(template, template$Idorsia.No)
 id <- intersect(d$Idorsia.No,tem$Idorsia.No)
 
 id <- intersect(id,rango$Idorsia.No)
 rango <- arrange(rango,rango$Idorsia.No)
 rango <- rango[rango$Idorsia.No%in%id,]
 rango <- arrange(rango,rango$Idorsia.No)
 d <- arrange(d,d$Idorsia.No)
 d <- d[d$Idorsia.No%in%id,]
 tem <- tem[tem$Idorsia.No%in%id,]
 tem <- arrange(tem, tem$Idorsia.No)
 sum(tem$Idorsia.No==d$Idorsia.No)
 
 
 d <- arrange(d,d$Idorsia.No)
 tem <- arrange(tem, tem$Idorsia.No)
 sum(tem$Idorsia.No==d$Idorsia.No)
 
 x <- as.numeric(template$MetStabRatMicros.intrClearance..ul..min.mg..)*1.66/(as.numeric(template$Microsomal.binding.fu.mic....)/100)
 y <- as.numeric(d$Clintu_obs_backcalc)
 
 df <- data.frame(x,y)
 df$y <- ifelse(df$y==0,NA,df$y)
 df$x <- ifelse(df$x==0,NA,df$x)
 # df <- data.frame(x = tem$KineticIV.Clearance..mL.min.kg., y = as.numeric(d$CLp))
 df <- data.frame(x,y,PPB)
 df$PPB <- ifelse(df$PPB=="",NA,df$PPB)
 df$y <- ifelse(df$y==0,NA,df$y)
 df$x <- ifelse(df$x==0,NA,df$x)
 
 
 
 # Iterate over names and create plot for each name
 plot_list <- ggplot(data = df, mapping = aes(x = df$x,y = df$y))+
   geom_point(mapping = aes(y=df$y,x=df$x), colour = "#529EFF")+
   scale_x_log10(limits = c(800,3000), labels = comma)+
   scale_y_log10(limits = c(800,3000), labels = comma)+
   geom_abline(intercept = 0, slope = 1)+
   stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
   stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
   stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
   stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
   geom_smooth(method=lm)+
   geom_point(aes(colour=PPB, group=PPB))+
   
   theme_classic()+
   labs(
     title = paste("CLintu_back_RLM"),
     subtitle = "",
     x = "observed CLintu_RLM [µL/min/kg]",
     y = "predicted CLintu_RLM [µL/min/kg]")
 
 
 
 # # extract slope from ggplot
 # model <- lm(data = df, formula = y ~ x)
 # slope <- summary(model)$coefficients[2]
 # 
 # # print slope
 # print(slope)
 ggsave(plot = plot_list, file = paste(names_vec[i],"PPB", "CLintu_back_RLM.png", sep = ""), width = 10, height = 10)
 # print(paste(max(df),min(df)))
 
 # z13 <- (c(afe(df$x,df$y), aafe(df$x,df$y),slope))
 # 14
 
 # 14
 # template_all_geomean_predictions_fraction <- read.csv("~/Big Dataset/template_all_geomean_predictions_fraction.csv")
 
 
 d <- template_all_geomean_predictions_fraction
 template <- admed_physiol
 
 d <- arrange(d,d$Idorsia.No)
 tem <- arrange(template, template$Idorsia.No)
 id <- intersect(d$Idorsia.No,tem$Idorsia.No)
 
 id <- intersect(id,rango$Idorsia.No)
 rango <- arrange(rango,rango$Idorsia.No)
 rango <- rango[rango$Idorsia.No%in%id,]
 rango <- arrange(rango,rango$Idorsia.No)
 d <- arrange(d,d$Idorsia.No)
 tem <- arrange(tem, tem$Idorsia.No)
 d <- d[d$Idorsia.No%in%id,]
 tem <- tem[tem$Idorsia.No%in%id,]
 sum(tem$Idorsia.No==d$Idorsia.No)
 

 
 d <- arrange(d,d$Idorsia.No)
 tem <- arrange(tem, tem$Idorsia.No)
 sum(tem$Idorsia.No==d$Idorsia.No)
 
 x <- as.numeric(template$MetStabRatMicros.intrClearance..ul..min.mg..)*1.66/(as.numeric(template$Microsomal.binding.fu.mic....)/100)
 y <- (1.66 * ((as.numeric(d$CYP_RLM_CLint))*template$fumic_half_mg_ml_conc)/(as.numeric(template$Microsomal.binding.fu.mic....)/100))
 
 df <- data.frame(x,y)
 df$y <- ifelse(df$y==0,NA,df$y)
 df$x <- ifelse(df$x==0,NA,df$x)
 # df <- data.frame(x = tem$KineticIV.Clearance..mL.min.kg., y = as.numeric(d$CLp))
 
 df <- data.frame(x,y,PPB)
 df$PPB <- ifelse(df$PPB=="",NA,df$PPB)
 df$y <- ifelse(df$y==0,NA,df$y)
 df$x <- ifelse(df$x==0,NA,df$x)
 
 
 # Iterate over names and create plot for each name
 plot_list <- ggplot(data = df, mapping = aes(x = df$x,y = df$y))+
   geom_point(mapping = aes(y=df$y,x=df$x), colour = "#529EFF")+
   scale_x_log10(limits = c(10,150000), labels = comma)+
   scale_y_log10(limits = c(10,150000), labels = comma)+
   geom_abline(intercept = 0, slope = 1)+
   stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
   stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
   stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
   stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
   geom_smooth(method=lm)+
   geom_point(aes(colour=PPB, group=PPB))+
   
   theme_classic()+
   labs(
     title = paste("CLint_obsu_RLM"),
     subtitle = "",
     x = "observed CLintu_RLM [µL/min/kg]",
     y = "predicted CLintu_RLM [µL/min/kg]")
 
 
 
 # extract slope from ggplot
 # model <- lm(data = df, formula = y ~ x)
 # slope <- summary(model)$coefficients[2]
 
 # print slope
 # print(slope)
 ggsave(plot = plot_list, file = paste(names_vec[i],"PPB", "CLint_obsu_RLM.png", sep = ""), width = 10, height = 10)
 # df <- na.omit(df)
 # print(paste(max(df),min(df)))
 # 
 # z14 <- (c(afe(df$x,df$y), aafe(df$x,df$y),slope))
 # 
 
 # 15
 
 d <- template_all_geomean_predictions_fraction
 template <- admed_physiol
 
 d <- arrange(d,d$Idorsia.No)
 tem <- arrange(template, template$Idorsia.No)
 id <- intersect(d$Idorsia.No,tem$Idorsia.No)
 
 id <- intersect(id,rango$Idorsia.No)
 rango <- arrange(rango,rango$Idorsia.No)
 rango <- rango[rango$Idorsia.No%in%id,]
 rango <- arrange(rango,rango$Idorsia.No)
 d <- arrange(d,d$Idorsia.No)
 tem <- arrange(tem, tem$Idorsia.No)
 d <- d[d$Idorsia.No%in%id,]
 tem <- tem[tem$Idorsia.No%in%id,]
 sum(tem$Idorsia.No==d$Idorsia.No)
 
 
 
 d <- arrange(d,d$Idorsia.No)
 tem <- arrange(tem, tem$Idorsia.No)
 sum(tem$Idorsia.No==d$Idorsia.No)
 
 x <- as.numeric(template$MetStabRatHepSusp.intrClearance..ul..min.1E6.cells..)*3.89/(as.numeric(template$Hepatocyte.binding.fu.hep....)/100)
 y <- (3.89 * ((as.numeric(template$HEP_rCLint))*template$fu.hep.Austin)/(as.numeric(template$Hepatocyte.binding.fu.hep....)/100))
 
 df <- data.frame(x,y)
 df$y <- ifelse(df$y==0,NA,df$y)
 df$x <- ifelse(df$x==0,NA,df$x)
 # df <- data.frame(x = tem$KineticIV.Clearance..mL.min.kg., y = as.numeric(d$CLp))
 
 df <- data.frame(x,y,PPB)
 df$PPB <- ifelse(df$PPB=="",NA,df$PPB)
 df$y <- ifelse(df$y==0,NA,df$y)
 df$x <- ifelse(df$x==0,NA,df$x)
 
 
 # Iterate over names and create plot for each name
 plot_list <- ggplot(data = df, mapping = aes(x = df$x,y = df$y))+
   geom_point(mapping = aes(y=df$y,x=df$x), colour = "#529EFF")+
   scale_x_log10(limits = c(20,110000), labels = comma)+
   scale_y_log10(limits = c(20,110000), labels = comma)+
   geom_abline(intercept = 0, slope = 1)+
   stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
   stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
   stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
   stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
   geom_smooth(method=lm)+
   geom_point(aes(colour=PPB, group=PPB))+
   
   theme_classic()+
   labs(
     title = paste("CLintu_obsu_RH"),
     subtitle = "",
     x = "observed CLintu_RH [µL/min/kg]",
     y = "predicted CLintu_RH [µL/min/kg]")
 
 
 # extract slope from ggplot
 # model <- lm(data = df, formula = y ~ x)
 # slope <- summary(model)$coefficients[2]
 
 # print slope
 # print(slope)
 ggsave(plot = plot_list, file = paste(names_vec[i],"PPB", "CLintu_obsu_RH.png", sep = ""), width = 10, height = 10)
 # df <- na.omit(df)
 # print(paste(max(df),min(df)))
 # 
 # z15 <- (c(afe(df$x,df$y), aafe(df$x,df$y),slope))
 # 
 # 16
 P.O.5 <- read.csv("~/Big Dataset/p.o/P.O.5/P.O.5.csv")
 
 d <- P.O.5
 template <- template_all_geomean_predictions_fraction
 
 d <- arrange(d,d$Idorsia.No)
 tem <- arrange(template, template$Idorsia.No)
 id <- intersect(d$Idorsia.No,tem$Idorsia.No)
 
 id <- intersect(id,rango$Idorsia.No)
 rango <- arrange(rango,rango$Idorsia.No)
 rango <- rango[rango$Idorsia.No%in%id,]
 d <- d[d$Idorsia.No%in%id,]
 tem <- tem[tem$Idorsia.No%in%id,]
 rango <- arrange(rango,rango$Idorsia.No)
 d <- arrange(d,d$Idorsia.No)
 tem <- arrange(tem, tem$Idorsia.No)
 sum(tem$Idorsia.No==d$Idorsia.No)
 

 d <- arrange(d,d$Idorsia.No)
 tem <- arrange(tem, tem$Idorsia.No)
 sum(tem$Idorsia.No==d$Idorsia.No)
 
 x <- d$Peff_obs.10..4
 y <- template$S.Peff
 
 df <- data.frame(x,y)
 df$y <- ifelse(df$y==0,NA,df$y)
 df$x <- ifelse(df$x==0,NA,df$x)
 # df <- data.frame(x = tem$KineticIV.Clearance..mL.min.kg., y = as.numeric(d$CLp))
 df <- data.frame(x,y,PPB)
 df$PPB <- ifelse(df$PPB=="",NA,df$PPB)
 df$y <- ifelse(df$y==0,NA,df$y)
 df$x <- ifelse(df$x==0,NA,df$x)
 
 
 # Iterate over names and create plot for each name
 plot_list <- ggplot(data = df, mapping = aes(x = df$x,y = df$y))+
   geom_point(mapping = aes(y=df$y,x=df$x), colour = "#529EFF")+
   scale_x_log10(limits = c(0.1,82), labels = comma)+
   scale_y_log10(limits = c(0.1,82), labels = comma)+
   geom_abline(intercept = 0, slope = 1)+
   stat_function(mapping = aes(),fun = stat_function_01,geom = "line",inherit.aes = T, linetype = "dashed")+
   stat_function(mapping = aes(),fun = stat_function_02,geom = "line",inherit.aes = T, linetype = "dashed")+
   stat_function(mapping = aes(),fun = stat_function_03,geom = "line",inherit.aes = T, linetype = "dotted")+
   stat_function(mapping = aes(),fun = stat_function_04,geom = "line",inherit.aes = T, linetype = "dotted")+
   geom_smooth(method=lm)+
   geom_point(aes(colour=PPB, group=PPB))+
   
   theme_classic()+
   labs(
     title = paste("Peff"),
     subtitle = "",
     x = "observed Peff [10^-4 cm/s]",
     y = "predicted Peff [10^-4 cm/s]")
 
 
 # extract slope from ggplot
 # model <- lm(data = df, formula = y ~ x)
 # slope <- summary(model)$coefficients[2]
 
 # print slope
 # print(slope)
 ggsave(plot = plot_list, file = paste(names_vec[i],"PPB", "Peff.png", sep = ""), width = 10, height = 10)
 # df <- na.omit(df)
 # print(paste(max(df),min(df))) 
 # z16 <- (c(afe(df$x,df$y), aafe(df$x,df$y),slope))
 # 
 # fehler <- data.frame(rbind(z1,z2,z3,z4,z5,z6,z7,z8,z9,z10,z11,z12,z13,z14,z15,z16))
 # write.csv(fehler,"idiot.csv")
 # 