#bullshit
rm(list=ls())
library(foreign)
library(readxl)
library(tidyverse)
library(haven)

index_5 <- read_dta("C:/Users/Stealth/Desktop/Group Assignment FSS/ESS_5_with_index.dta")
index_6 <- read_dta("C:/Users/Stealth/Desktop/Group Assignment FSS/ESS_6_with_index.dta")
index_7 <- read_dta("C:/Users/Stealth/Desktop/Group Assignment FSS/ESS_7_with_index.dta")
index_8 <- read_dta("C:/Users/Stealth/Desktop/Group Assignment FSS/ESS_08_with_index.dta")
index_9 <- read_dta("C:/Users/Stealth/Desktop/Group Assignment FSS/ESS_09_with_index.dta")
index_10 <- read_dta("C:/Users/Stealth/Desktop/Group Assignment FSS/ESS_10_with_index.dta")
index_11 <- read_dta("C:/Users/Stealth/Desktop/Group Assignment FSS/ESS_11_with_index.dta")





data_2018_control_finlandia <- subset(data_2018_control, data_2018_control$cntry =="FI" )
mean_2018_finlandia <- mean(data_2018_control_finlandia$imp_free)
mean_2018_finlandia

data_2018_control_hungary <- subset(data_2018_control, data_2018_control$cntry =="HU" )
mean_2018_hungary <- mean(data_2018_control_hungary$imp_free)
mean_2018_hungary

data_2018_control_switzerland <- subset(data_2018_control, data_2018_control$cntry =="CH" )
mean_2018_switzerland <- mean(data_2018_control_switzerland$imp_free)
mean_2018_switzerland

data_2018_control_slovenia <- subset(data_2018_control, data_2018_control$cntry =="SI" )
mean_2018_slovenia <- mean(data_2018_control_slovenia$imp_free)
mean_2018_slovenia

data_2018_control_croatia <- subset(data_2018_control, data_2018_control$cntry =="HR" )
mean_2018_croatia <- mean(data_2018_control_croatia$imp_free)
mean_2018_croatia


