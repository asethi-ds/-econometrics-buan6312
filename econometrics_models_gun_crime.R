# Set working Directory as needed

rm(list=ls())

# Loading libraries
library(foreign)
library(plm)
library(dplyr)
library(stargazer)
library(ggplot2)
library(corrplot)
library(car)
library(survey)
library(MASS)
library(vars) 
library(het.test)
library(xlsx)

# Reading data
data <- read.xlsx("C:/Users/Player1/Downloads/guns.xlsx", sheetIndex = 1, header=TRUE) #Change as needed

# Converting to Panel Data Frame
data_pan <- pdata.frame(data,index=c('State','year'))

# Fixing Missing Data
data_pan[is.na(data_pan)] <- 0

#Pooled OLS Model

ModelPool <- lm (log(vio)~shall+log(incarc_rate)+pb1064+pm1029+pop+avginc+log(density), data=data_pan)
summary(ModelPool)
stargazer(ModelPool, dep.var.caption="Table 1: Pooled OLS Model",type = 'text',align = TRUE)

bptest(ModelPool) # Testing for Heteroskedasticity

# Pooled OLS with Cluster Robust Standard Errors
ModelPoolRobSE <- lm(log(vio)~shall+log(incarc_rate)+pb1064+pm1029+pop+avginc+log(density), data=data_pan)
summary(ModelPoolRobSE,vcov=vcovHC(ModelPoolRobSE,method="arellano"))
stargazer(ModelPoolRobSE,type='text',align = TRUE)

# Checking the difference in Standard Errors
stargazer(ModelPool,ModelPoolRobSE, dep.var.caption = "Table 2: Comparison of Pooled OLS with and without Cluster Robust Standard Errors",   type='text',align = TRUE)


# Entity Fixed Effects Model
ModelFE <- plm(log(vio)~shall+log(incarc_rate)+pb1064+pm1029+pop+avginc+log(density)
               , model="within"
               , data = data_pan)
summary(ModelFE)

stargazer(ModelFE, dep.var.caption="Table 3: Entity Fixed Effects Model", type="text", align = TRUE)

# Entity and Time Fixed Effects Model Code

ModelTFE3 <- plm(log(vio)~factor(year)+shall+log(incarc_rate)+pb1064+pm1029+pop+avginc+log(density), model="within", data = data_pan)

summary(ModelTFE3)
stargazer(ModelTFE3, type="text",align = TRUE)

waldtest(ModelTFE3, update(ModelTFE3, .~.-factor(year)), vcov=vcovHC)
stargazer(waldtest(ModelTFE3, update(ModelTFE3, .~.-factor(year)), vcov=vcovHC), type = "text")

# Comparing the 3 models
stargazer(ModelPoolRobSE,ModelFE,ModelTFE3, column.labels=c("POOLED OLS","ENTITY FIXED","TIME AND ENTITY FIXED EFFECTS"),
          type="text",align = TRUE)
        
