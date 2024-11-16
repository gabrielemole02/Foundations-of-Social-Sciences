#Foundations Group Project
rm(list=ls())
library(foreign)
library(car)  
library(readxl)
library(readr)
library(lmtest)  
library(sandwich)
library(quantreg)
library(rdd)
library(ggplot2)

#Load datasets
data_covid <- read_csv("C:/Users/Stealth/Downloads/covid-19-stringency-index.csv")
data_covid <- subset(data_covid, data_covid$Entity == "Finland" | data_covid$Entity == "Hungary" | data_covid$Entity == "Germany" | data_covid$Entity == "Ireland" | data_covid$Entity == "United Kingdom" |data_covid$Entity == "Switzerland" |data_covid$Entity == "Netherlands" |data_covid$Entity == "Lithuania" |data_covid$Entity == "Slovenia" |data_covid$Entity == "Croatia" |data_covid$Entity == "Austria" |data_covid$Entity == "Slovakia" |data_covid$Entity == "Norway")

data <- read_csv("C:/Users/Stealth/Downloads/ESS1e06_7-ESS2e03_6-ESS3e03_7-ESS4e04_6-ESS5e03_5-ESS6e02_6-ESS7e02_3-ESS8e02_3-ESS9e03_2-ESS10-ESS10SC-ESS11-subset.csv")

#data processing  of main dataset
data$imp_free <- ifelse(is.na(data$impfree), data$impfreea, data$impfree)
data$imp_people <- ifelse(is.na(data$iphlppl), data$iphlppla, data$iphlppl)

#dropping missing values
data1 <- subset(data, !is.na(data$agea))
data1 <- subset(data1, !is.na(data1$imp_free))
data1 <- subset(data1, !is.na(data1$imp_people))
data1 <- subset(data1, imp_free != "7" & imp_free != "8" & imp_free != "9")
data1 <- subset(data1, imp_free != "66" & imp_free != "77" & imp_free != "88" & imp_free != "99")
data1 <- subset(data1, imp_people != "66" & imp_people != "77" & imp_people!= "88" & imp_people != "99")
data1 <- subset(data1, eduyrs != "66" &eduyrs != "77" & eduyrs!= "88" & eduyrs != "99")
data1 <- subset(data1, agea != "999")
data1 <- subset(data1, gndr != "9")

#rescaling
data1$imp_free <- ifelse(data1$imp_free == "6", 1, ifelse(data1$imp_free == "5", 2, ifelse(data1$imp_free == "4", 3, ifelse(data1$imp_free == "3", 4, ifelse(data1$imp_free == "2", 5, 6)))))
data1$imp_people <- ifelse(data1$imp_people == "6", 1, ifelse(data1$imp_people == "5", 2, ifelse(data1$imp_people == "4", 3, ifelse(data1$imp_people == "3", 4, ifelse(data1$imp_people == "2", 5, 6)))))

#yearly mean 2020-2023 on stringency data

data_2020 <- data_covid[grepl("2020", data_covid$Day),]
data_2021 <- data_covid[grepl("2021", data_covid$Day),]
data_2022 <- data_covid[grepl("2022", data_covid$Day),]

cntry <- c("FI" , "HU" , "DE" , "IR" , "UK", "CH", "NL", "SI", "HR", "AT", "SK","NO", "LI")

#2020

data_2020_Finland <- subset(data_2020, data_2020$Entity=="Finland")
means_2020_Finland <- mean(data_2020_Finland$`Stringency index (weighted average)`)

data_2020_Hungary <- subset(data_2020, data_2020$Entity=="Hungary")
means_2020_Hungary <- mean(data_2020_Hungary$`Stringency index (weighted average)`)

data_2020_Germany <- subset(data_2020, data_2020$Entity=="Germany")
means_2020_Germany <- mean(data_2020_Germany$`Stringency index (weighted average)`)

data_2020_Ireland <- subset(data_2020, data_2020$Entity=="Ireland")
means_2020_Ireland <- mean(data_2020_Ireland$`Stringency index (weighted average)`)

data_2020_uk <- subset(data_2020, data_2020$Entity=="United Kingdom")
means_2020_uk <- mean(data_2020_uk$`Stringency index (weighted average)`)

data_2020_Switzerland <- subset(data_2020, data_2020$Entity=="Switzerland")
means_2020_Switzerland <- mean(data_2020_Switzerland$`Stringency index (weighted average)`)

data_2020_Netherlands <- subset(data_2020, data_2020$Entity=="Netherlands")
means_2020_Netherlands <- mean(data_2020_Netherlands$`Stringency index (weighted average)`)

data_2020_Slovenia <- subset(data_2020, data_2020$Entity=="Slovenia")
means_2020_Slovenia <- mean(data_2020_Slovenia$`Stringency index (weighted average)`)

data_2020_Croatia <- subset(data_2020, data_2020$Entity=="Croatia")
means_2020_Croatia <- mean(data_2020_Croatia$`Stringency index (weighted average)`)

data_2020_Austria <- subset(data_2020, data_2020$Entity=="Austria")
means_2020_Austria <- mean(data_2020_Austria$`Stringency index (weighted average)`)

data_2020_Slovakia <- subset(data_2020, data_2020$Entity=="Slovakia")
means_2020_Slovakia <- mean(data_2020_Slovakia$`Stringency index (weighted average)`)

data_2020_Norway <- subset(data_2020, data_2020$Entity=="Norway")
means_2020_Norway <- mean(data_2020_Norway$`Stringency index (weighted average)`)

data_2020_Lithuania <- subset(data_2020, data_2020$Entity=="Lithuania")
means_2020_Lithuania <- mean(data_2020_Lithuania$`Stringency index (weighted average)`)

means_2020 <- c(means_2020_Finland, means_2020_Hungary , means_2020_Germany, means_2020_Ireland , means_2020_uk, means_2020_Switzerland, means_2020_Netherlands, means_2020_Slovenia, means_2020_Croatia, means_2020_Austria, means_2020_Slovakia, means_2020_Norway, means_2020_Lithuania)


#2021

data_2021_Finland <- subset(data_2021, data_2021$Entity=="Finland")
means_2021_Finland <- mean(data_2021_Finland$`Stringency index (weighted average)`)

data_2021_Hungary <- subset(data_2021, data_2021$Entity=="Hungary")
means_2021_Hungary <- mean(data_2021_Hungary$`Stringency index (weighted average)`)

data_2021_Germany <- subset(data_2021, data_2021$Entity=="Germany")
means_2021_Germany <- mean(data_2021_Germany$`Stringency index (weighted average)`)

data_2021_Ireland <- subset(data_2021, data_2021$Entity=="Ireland")
means_2021_Ireland <- mean(data_2021_Ireland$`Stringency index (weighted average)`)

data_2021_uk <- subset(data_2021, data_2021$Entity=="United Kingdom")
means_2021_uk <- mean(data_2021_uk$`Stringency index (weighted average)`)

data_2021_Switzerland <- subset(data_2021, data_2021$Entity=="Switzerland")
means_2021_Switzerland <- mean(data_2021_Switzerland$`Stringency index (weighted average)`)

data_2021_Netherlands <- subset(data_2021, data_2021$Entity=="Netherlands")
means_2021_Netherlands <- mean(data_2021_Netherlands$`Stringency index (weighted average)`)

data_2021_Slovenia <- subset(data_2021, data_2021$Entity=="Slovenia")
means_2021_Slovenia <- mean(data_2021_Slovenia$`Stringency index (weighted average)`)

data_2021_Croatia <- subset(data_2021, data_2021$Entity=="Croatia")
means_2021_Croatia <- mean(data_2021_Croatia$`Stringency index (weighted average)`)

data_2021_Austria <- subset(data_2021, data_2021$Entity=="Austria")
means_2021_Austria <- mean(data_2021_Austria$`Stringency index (weighted average)`)

data_2021_Slovakia <- subset(data_2021, data_2021$Entity=="Slovakia")
means_2021_Slovakia <- mean(data_2021_Slovakia$`Stringency index (weighted average)`)

data_2021_Norway <- subset(data_2021, data_2021$Entity=="Norway")
means_2021_Norway <- mean(data_2021_Norway$`Stringency index (weighted average)`)

data_2021_Lithuania <- subset(data_2021, data_2021$Entity=="Lithuania")
means_2021_Lithuania <- mean(data_2021_Lithuania$`Stringency index (weighted average)`)

means_2021 <- c(means_2021_Finland, means_2021_Hungary , means_2021_Germany, means_2021_Ireland , means_2021_uk, means_2021_Switzerland, means_2021_Netherlands, means_2021_Slovenia, means_2021_Croatia, means_2021_Austria, means_2021_Slovakia, means_2021_Norway, means_2021_Lithuania)

#2022

data_2022_Finland <- subset(data_2022, data_2022$Entity=="Finland")
means_2022_Finland <- mean(data_2022_Finland$`Stringency index (weighted average)`)

data_2022_Hungary <- subset(data_2022, data_2022$Entity=="Hungary")
means_2022_Hungary <- mean(data_2022_Hungary$`Stringency index (weighted average)`)

data_2022_Germany <- subset(data_2022, data_2022$Entity=="Germany")
means_2022_Germany <- mean(data_2022_Germany$`Stringency index (weighted average)`)

data_2022_Ireland <- subset(data_2022, data_2022$Entity=="Ireland")
means_2022_Ireland <- mean(data_2022_Ireland$`Stringency index (weighted average)`)

data_2022_uk <- subset(data_2022, data_2022$Entity=="United Kingdom")
means_2022_uk <- mean(data_2022_uk$`Stringency index (weighted average)`)

data_2022_Switzerland <- subset(data_2022, data_2022$Entity=="Switzerland")
means_2022_Switzerland <- mean(data_2022_Switzerland$`Stringency index (weighted average)`)

data_2022_Netherlands <- subset(data_2022, data_2022$Entity=="Netherlands")
means_2022_Netherlands <- mean(data_2022_Netherlands$`Stringency index (weighted average)`)

data_2022_Slovenia <- subset(data_2022, data_2022$Entity=="Slovenia")
means_2022_Slovenia <- mean(data_2022_Slovenia$`Stringency index (weighted average)`)

data_2022_Croatia <- subset(data_2022, data_2022$Entity=="Croatia")
means_2022_Croatia <- mean(data_2022_Croatia$`Stringency index (weighted average)`)

data_2022_Austria <- subset(data_2022, data_2022$Entity=="Austria")
means_2022_Austria <- mean(data_2022_Austria$`Stringency index (weighted average)`)

data_2022_Slovakia <- subset(data_2022, data_2022$Entity=="Slovakia")
means_2022_Slovakia <- mean(data_2022_Slovakia$`Stringency index (weighted average)`)

data_2022_Norway <- subset(data_2022, data_2022$Entity=="Norway")
means_2022_Norway <- mean(data_2022_Norway$`Stringency index (weighted average)`)

data_2022_Lithuania <- subset(data_2022, data_2022$Entity=="Lithuania")
means_2022_Lithuania <- mean(data_2022_Lithuania$`Stringency index (weighted average)`)

means_2022 <- c(means_2022_Finland, means_2022_Hungary , means_2022_Germany, means_2022_Ireland , means_2022_uk, means_2022_Switzerland, means_2022_Netherlands, means_2022_Slovenia, means_2022_Croatia, means_2022_Austria, means_2022_Slovakia, means_2022_Norway, means_2022_Lithuania)

xValue <- 1:13
matplot(xValue, cbind(means_2020, means_2021, means_2022), type = "l", lty = 1, 
        col = c("red", "blue", "green"), xlab = "X", 
        ylab = "Y", main = "Comparison trends in stringency") + abline(v=9, col="black")

A <- cbind(cntry, means_2020, means_2021, means_2022)
data_stringency <- as.data.frame(A)
data_stringency$means_2020 <- as.numeric(data_stringency$means_2020)
data_stringency$means_2021 <- as.numeric(data_stringency$means_2021)
data_stringency$means_2022 <- as.numeric(data_stringency$means_2022)
data_stringency$means <- (as.numeric(data_stringency$means_2020) + as.numeric(data_stringency$means_2021) +as.numeric(data_stringency$means_2022) )/3
data_stringency$treat <- ifelse(data_stringency$means >= median(data_stringency$means), 1,0)

#merging datasets
data_did_str <- merge(data_stringency, data1, by = "cntry")
data_did_str$post <- ifelse(data_did_str$essround <= 9, 0,1)
summary(data_did_str$post)
summary(data_did_str$treat)

#diff-in-diff

reg1 <- lm(imp_free ~ treat + post + treat*post, data = data_did_str)
coeftest(reg1, vcov = vcovHC(reg1))

#controls
reg2 <- lm(imp_free ~ treat + post + treat*post +agea +gndr + eduyrs, data = data_did_str)
coeftest(reg2, vcov = vcovHC(reg2))

#time and fixed effects
reg2b <- lm(imp_free ~ treat + post + treat*post +agea +gndr + eduyrs +factor(essround) + factor(cntry), data = data_did_str)
coeftest(reg2b, vcov = vcovHC(reg2b))


#####
#PTA#
#####

#placebo test
reg3 <- lm(imp_free ~ treat + post + treat*post +agea +gndr + eduyrs + ppltrst + rlgdgr + gincdif +factor(essround)+factor(cntry) + factor(essround)*treat, data = data_did_str)
coeftest(reg3, vcov = vcovHC(reg3))

#parallel trends
data_control <- subset(data_did_str, data_did_str$treat == 0)
data_treat <- subset(data_did_str, data_did_str$treat == 1)

#control
data_2002_control <- subset(data_control, data_control$essround==1)
means_2002_control <- mean(data_2002_control$imp_free)

data_2004_control <- subset(data_control, data_control$essround==2)
means_2004_control <- mean(data_2004_control$imp_free)

data_2006_control <- subset(data_control, data_control$essround==3)
means_2006_control <- mean(data_2006_control$imp_free)

data_2008_control <- subset(data_control, data_control$essround==4)
means_2008_control <- mean(data_2008_control$imp_free)

data_2010_control <- subset(data_control, data_control$essround==5)
means_2010_control <- mean(data_2010_control$imp_free)

data_2012_control <- subset(data_control, data_control$essround==6)
means_2012_control <- mean(data_2012_control$imp_free)

data_2014_control <- subset(data_control, data_control$essround==7)
means_2014_control <- mean(data_2014_control$imp_free)

data_2016_control <- subset(data_control, data_control$essround==8)
means_2016_control <- mean(data_2016_control$imp_free)

data_2018_control <- subset(data_control, data_control$essround==9)
means_2018_control <- mean(data_2018_control$imp_free)

data_2020_control <- subset(data_control, data_control$essround==10)
means_2020_control <- mean(data_2020_control$imp_free)

data_2023_control <- subset(data_control, data_control$essround==11)
means_2023_control <- mean(data_2023_control$imp_free)

means_control <- c(means_2002_control, means_2004_control, means_2006_control, means_2008_control, means_2010_control, means_2012_control, means_2014_control, means_2016_control, means_2018_control, means_2020_control ,means_2023_control)

#treatment
data_2002_treat <- subset(data_treat, data_treat$essround==1)
means_2002_treat <- mean(data_2002_treat$imp_free)

data_2004_treat <- subset(data_treat, data_treat$essround==2)
means_2004_treat <- mean(data_2004_treat$imp_free)

data_2006_treat <- subset(data_treat, data_treat$essround==3)
means_2006_treat <- mean(data_2006_treat$imp_free)

data_2008_treat <- subset(data_treat, data_treat$essround==4)
means_2008_treat <- mean(data_2008_treat$imp_free)

data_2010_treat <- subset(data_treat, data_treat$essround==5)
means_2010_treat <- mean(data_2010_treat$imp_free)

data_2012_treat <- subset(data_treat, data_treat$essround==6)
means_2012_treat <- mean(data_2012_treat$imp_free)

data_2014_treat <- subset(data_treat, data_treat$essround==7)
means_2014_treat <- mean(data_2014_treat$imp_free)

data_2016_treat <- subset(data_treat, data_treat$essround==8)
means_2016_treat <- mean(data_2016_treat$imp_free)

data_2018_treat <- subset(data_treat, data_treat$essround==9)
means_2018_treat <- mean(data_2018_treat$imp_free)

data_2020_treat <- subset(data_treat, data_treat$essround==10)
means_2020_treat <- mean(data_2020_treat$imp_free)

data_2023_treat <- subset(data_treat, data_treat$essround==11)
means_2023_treat <- mean(data_2023_treat$imp_free)

means_treat <- c(means_2002_treat, means_2004_treat, means_2006_treat, means_2008_treat, means_2010_treat, means_2012_treat, means_2014_treat, means_2016_treat, means_2018_treat, means_2020_treat ,means_2023_treat)

xValue <- 1:11
matplot(xValue, cbind(means_control, means_treat), type = "l", lty = 1, 
        col = c("red", "blue"), xlab = "X", 
        ylab = "Y", main = "Comparison trends in Individualism") + abline(v=9.5, col="black")

################
#subpopulations#
################

#agegroups
data_young <- subset(data_did_str, data_did_str$agea <= 35)
data_adult <- subset(data_did_str, data_did_str$agea > 35 | data_did_str$agea < 35 )
data_old <- subset(data_did_str, data_did_str$agea >= 65)

#young
reg4 <- lm(imp_free ~ treat + post + treat*post, data = data_young)
coeftest(reg4, vcov = vcovHC(reg4))
#adults
reg5 <- lm(imp_free ~ treat + post + treat*post, data = data_adult)
coeftest(reg5, vcov = vcovHC(reg5))
#old
reg6 <- lm(imp_free ~ treat + post + treat*post, data = data_old)
coeftest(reg6, vcov = vcovHC(reg6))

#education level
data_high_ed <- subset(data_did_str, data_did_str$eduyrs >= 13)
data_low_ed <- subset(data_did_str, data_did_str$eduyrs < 13)

reg7 <- lm(imp_free ~ treat + post + treat*post, data = data_high_ed)
coeftest(reg7, vcov = vcovHC(reg7))

reg8 <- lm(imp_free ~ treat + post + treat*post, data = data_low_ed)
coeftest(reg8, vcov = vcovHC(reg8))

#gender
data_male <- subset(data_did_str, data_did_str$gndr == 1)
data_female <- subset(data_did_str, data_did_str$gndr == 2)

reg9 <- lm(imp_free ~ treat + post + treat*post, data = data_male)
coeftest(reg9, vcov = vcovHC(reg9))

reg10 <- lm(imp_free ~ treat + post + treat*post, data = data_female)
coeftest(reg10, vcov = vcovHC(reg10))

############
#robustness#
############

#check if the opposite value diminshed 
reg11 <- lm(imp_people ~ treat + post + treat*post, data = data_did_str)
coeftest(reg11, vcov = vcovHC(reg11))

#controls
reg12 <- lm(imp_people ~ treat + post + treat*post +agea +gndr + eduyrs, data = data_did_str)
coeftest(reg12, vcov = vcovHC(reg12))

reg13 <- lm(imp_people ~ treat + post + treat*post +agea +gndr + eduyrs + ppltrst + rlgdgr + gincdif +factor(essround)+factor(cntry) + factor(essround)*treat, data = data_did_str)
coeftest(reg13, vcov = vcovHC(reg13))

#alternative measure of individualism through a composite index



