
setwd("~/Big Dataset/Simulations/ADMET Properties")
admed_physiol <-  read.csv("~/Big Dataset/Simulations/ADMET Properties/All_final_to_share_B2P%_7.4.csv")
solHCL <- read.csv("solHCL.csv")

setwd("~/Big Dataset/Simulations/fed properties fup,RBP,logD")
template <- read.csv("~/Big Dataset/p.o/template.csv")
template_all_geomean_predictions_fraction <- read.csv("~/Big Dataset/template_all_geomean_predictions_fraction.csv")

# library(dplyr)
# # install.packages("ggplot2")
# library(ggplot2)
# # install.packages("scales")
# library(scales)
# # install.packages("fun")
# library(fun)
# # install.packages("tidyverse")
# library(tidyverse)
# library(ggpubr)
# library(stargazer)
# aafe <- function(x,y){ #x observed, y predicted
#   df <- data.frame(x,y)
#   df <- na.omit(df)
#   res <- 10 ** ((1/length(df$x))*sum(abs(log10(df$y/df$x))))
#   
#   return(res)
# }
# afe <- function(x,y){ #x observed, y predicted
#   df <- data.frame(x,y)
#   df <- na.omit(df)
#   res <- 10 ** ((1/length(df$x))*sum((log10(df$y/df$x))))
#   return(res)
# }
# 
# # Plot 1 CLphep1 vs KineticIV.Clearance..mL.min.kg. MFE
# 
# stat_function_01 <- function(x){
#   2 * x
# }
# stat_function_02 <- function(x){
#   (1/2) * x
# }
# stat_function_03 <- function(x){
#   5 * x
# }
# stat_function_04 <- function(x){
#   (1/5) * x
# }
# setwd("~/Big Dataset/graphs and errors")



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

x <- as.numeric(tem$solHCl.solubility..ug.ml./1000)
y <- as.numeric(d$S.S_pH) 

df <- data.frame(x,y)
df$y <- ifelse(df$y==0,NA,df$y)
df$x <- ifelse(df$x==0,NA,df$x)
# df <- data.frame(x = tem$KineticIV.Clearance..mL.min.kg., y = as.numeric(d$CLp))

df <- data.frame(x,y,Syst.in.vivo.CLb)
df$Syst.in.vivo.CLb <- ifelse(df$Syst.in.vivo.CLb=="",NA,df$Syst.in.vivo.CLb)
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
  geom_point(aes(colour=Syst.in.vivo.CLb, group=Syst.in.vivo.CLb))+
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
ggsave(plot = plot_list, file = paste(names_vec[i],"Syst.in.vivo.CLb", "solHCl.png", sep = ""), width = 10, height = 10)
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
 df <- data.frame(x,y,Syst.in.vivo.CLb)
 df$Syst.in.vivo.CLb <- ifelse(df$Syst.in.vivo.CLb=="",NA,df$Syst.in.vivo.CLb)
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
   geom_point(aes(colour=Syst.in.vivo.CLb, group=Syst.in.vivo.CLb))+
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
 ggsave(plot = plot_list, file = paste(names_vec[i],"Syst.in.vivo.CLb", "solFeSSIF.png", sep = ""), width = 10, height = 10)
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
 df <- data.frame(x,y,Syst.in.vivo.CLb)
 df$Syst.in.vivo.CLb <- ifelse(df$Syst.in.vivo.CLb=="",NA,df$Syst.in.vivo.CLb)
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
   geom_point(aes(colour=Syst.in.vivo.CLb, group=Syst.in.vivo.CLb))+
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
 ggsave(plot = plot_list, file = paste(names_vec[i],"Syst.in.vivo.CLb", "solFaSSIF.png", sep = ""), width = 10, height = 10)
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
 df <- data.frame(x,y,Syst.in.vivo.CLb)
 df$Syst.in.vivo.CLb <- ifelse(df$Syst.in.vivo.CLb=="",NA,df$Syst.in.vivo.CLb)
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
   geom_point(aes(colour=Syst.in.vivo.CLb, group=Syst.in.vivo.CLb))+
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
 ggsave(plot = plot_list, file = paste(names_vec[i],"Syst.in.vivo.CLb", "solPBS.png", sep = ""), width = 10, height = 10)
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
 df <- data.frame(x,y,Syst.in.vivo.CLb)
 df$Syst.in.vivo.CLb <- ifelse(df$Syst.in.vivo.CLb=="",NA,df$Syst.in.vivo.CLb)
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
   geom_point(aes(colour=Syst.in.vivo.CLb, group=Syst.in.vivo.CLb))+
   theme_classic()+
   labs(
     title = paste("logD"),
     subtitle = "",
     x = "observed logD [mg/mL]",
     y = "predicted logD [mg/mL]")
 
 
 
 # # extract slope from ggplot
 # model <- lm(data = df, formula = y ~ x)
 # slope <- summary(model)$coefficients[2]
 # 
 # # print slope
 # print(slope)
 ggsave(plot = plot_list, file = paste(names_vec[i],"Syst.in.vivo.CLb", "logD.png", sep = ""), width = 10, height = 10)
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
 
 df <- data.frame(x,y,Syst.in.vivo.CLb)
 df$Syst.in.vivo.CLb <- ifelse(df$Syst.in.vivo.CLb=="",NA,df$Syst.in.vivo.CLb)
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
   geom_point(aes(colour=Syst.in.vivo.CLb, group=Syst.in.vivo.CLb))+
   theme_classic()+
   labs(
     title = paste("fuhep Austin"),
     subtitle = "",
     x = "observed fuhep Austin",
     y = "predicted fuhep")
 
 
 
 # # extract slope from ggplot
 # model <- lm(data = df, formula = y ~ x)
 # slope <- summary(model)$coefficients[2]
 # 
 # # print slope
 # print(slope)
 ggsave(plot = plot_list, file = paste(names_vec[i],"Syst.in.vivo.CLb", "fuhepAustin.png", sep = ""), width = 10, height = 10)
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
 df <- data.frame(x,y,Syst.in.vivo.CLb)
 df$Syst.in.vivo.CLb <- ifelse(df$Syst.in.vivo.CLb=="",NA,df$Syst.in.vivo.CLb)
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
   geom_point(aes(colour=Syst.in.vivo.CLb, group=Syst.in.vivo.CLb))+
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
 ggsave(plot = plot_list, file = paste(names_vec[i],"Syst.in.vivo.CLb", "fumic_half.png", sep = ""), width = 10, height = 10)
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
 df <- data.frame(x,y,Syst.in.vivo.CLb)
 df$Syst.in.vivo.CLb <- ifelse(df$Syst.in.vivo.CLb=="",NA,df$Syst.in.vivo.CLb)
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
   geom_point(aes(colour=Syst.in.vivo.CLb, group=Syst.in.vivo.CLb))+
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
 ggsave(plot = plot_list, file = paste(names_vec[i],"Syst.in.vivo.CLb", "rat_fup.png", sep = ""), width = 10, height = 10)
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
 df <- data.frame(x,y,Syst.in.vivo.CLb)
 df$Syst.in.vivo.CLb <- ifelse(df$Syst.in.vivo.CLb=="",NA,df$Syst.in.vivo.CLb)
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
   geom_point(aes(colour=Syst.in.vivo.CLb, group=Syst.in.vivo.CLb))+
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
 ggsave(plot = plot_list, file = paste(names_vec[i],"Syst.in.vivo.CLb", "RBP.png", sep = ""), width = 10, height = 10)
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
 
 x <- as.numeric(d$MetStabRatMicros.intrClearance..ul..min.mg..)*1.66/(as.numeric(d$Microsomal.binding.fu.mic....)/100)
 y <- as.numeric(d$CYP_RLM_CLint)
 
 df <- data.frame(x,y)
 df$y <- ifelse(df$y==0,NA,df$y)
 df$x <- ifelse(df$x==0,NA,df$x)
 # df <- data.frame(x = tem$KineticIV.Clearance..mL.min.kg., y = as.numeric(d$CLp))
 df <- data.frame(x,y,Syst.in.vivo.CLb)
 df$Syst.in.vivo.CLb <- ifelse(df$Syst.in.vivo.CLb=="",NA,df$Syst.in.vivo.CLb)
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
   geom_point(aes(colour=Syst.in.vivo.CLb, group=Syst.in.vivo.CLb))+
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
 ggsave(plot = plot_list, file = paste(names_vec[i], "Syst.in.vivo.CLb","CLintu_RLM.png", sep = ""), width = 10, height = 10)
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
 y <- as.numeric(d$HEP_rCLint)
 
 df <- data.frame(x,y)
 df$y <- ifelse(df$y==0,NA,df$y)
 df$x <- ifelse(df$x==0,NA,df$x)
 # df <- data.frame(x = tem$KineticIV.Clearance..mL.min.kg., y = as.numeric(d$CLp))
 df <- data.frame(x,y,Syst.in.vivo.CLb)
 df$Syst.in.vivo.CLb <- ifelse(df$Syst.in.vivo.CLb=="",NA,df$Syst.in.vivo.CLb)
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
   geom_point(aes(colour=Syst.in.vivo.CLb, group=Syst.in.vivo.CLb))+
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
 ggsave(plot = plot_list, file = paste(names_vec[i],"Syst.in.vivo.CLb", "CLintu_RH.png", sep = ""), width = 10, height = 10)
 # df <- na.omit(df)
 # print(paste(max(df),min(df)))
 # 
 # print(c(afe(df$x,df$y), aafe(df$x,df$y),slope))
 # 
 # 
 # 