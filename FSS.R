#Foundations of Social Sciences Group Assignment 
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


#load the dataset
data <- read_csv("C:/Users/Stealth/Downloads/ESS1e06_7-ESS2e03_6-ESS3e03_7-ESS4e04_6-ESS5e03_5-ESS6e02_6-ESS7e02_3-ESS8e02_3-ESS9e03_2-ESS10-ESS10SC-ESS11-subset.csv")


#data processing 
data$imp_free <- ifelse(is.na(data$impfree), data$impfreea, data$impfree)
data$imp_people <- ifelse(is.na(data$iphlppl), data$iphlppla, data$iphlppl)

#missing values
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


#diff in diff imp_free

#ireland
data_ireland <- subset(data1, cntry == "IE")
#round 1
data_ireland1 <- subset(data_ireland, essround == "1")
ess1 <- mean(data_ireland1$imp_free)
#round 2
data_ireland2 <- subset(data_ireland, essround == "2")
ess2 <- mean(data_ireland2$imp_free)
#round 3
data_ireland3 <- subset(data_ireland, essround == "3")
ess3 <- mean(data_ireland3$imp_free)
#round 4
data_ireland4 <- subset(data_ireland, essround == "4")
ess4 <- mean(data_ireland4$imp_free)
#round 5
data_ireland5 <- subset(data_ireland, essround == "5")
ess5 <- mean(data_ireland5$imp_free)
#round 6
data_ireland6 <- subset(data_ireland, essround == "6")
ess6 <- mean(data_ireland6$imp_free)
#round 7
data_ireland7 <- subset(data_ireland, essround == "7")
ess7 <- mean(data_ireland7$imp_free)
#round 8
data_ireland8 <- subset(data_ireland, essround == "8")
ess8 <- mean(data_ireland8$imp_free)
#round 9
data_ireland9 <- subset(data_ireland, essround == "9")
ess9 <- mean(data_ireland9$imp_free)
#round 10
data_ireland10 <- subset(data_ireland, essround == "10")
ess10 <- mean(data_ireland10$imp_free)
#round 11
data_ireland11 <- subset(data_ireland, essround == "11")
ess11 <- mean(data_ireland11$imp_free)

y_ireland <- c(ess1,ess2,ess3,ess4,ess5,ess6,ess7,ess8,ess9,ess10,ess11)

#finland
data_finland <- subset(data1, cntry == "FI")
#round 1
data_finland1 <- subset(data_finland, essround == "1")
ess1 <- mean(data_finland1$imp_free)
#round 2
data_finland2 <- subset(data_finland, essround == "2")
ess2 <- mean(data_finland2$imp_free)
#round 3
data_finland3 <- subset(data_finland, essround == "3")
ess3 <- mean(data_finland3$imp_free)
#round 4
data_finland4 <- subset(data_finland, essround == "4")
ess4 <- mean(data_finland4$imp_free)
#round 5
data_finland5 <- subset(data_finland, essround == "5")
ess5 <- mean(data_finland5$imp_free)
#round 6
data_finland6 <- subset(data_finland, essround == "6")
ess6 <- mean(data_finland6$imp_free)
#round 7
data_finland7 <- subset(data_finland, essround == "7")
ess7 <- mean(data_finland7$imp_free)
#round 8
data_finland8 <- subset(data_finland, essround == "8")
ess8 <- mean(data_finland8$imp_free)
#round 9
data_finland9 <- subset(data_finland, essround == "9")
ess9 <- mean(data_finland9$imp_free)
#round 10
data_finland10 <- subset(data_finland, essround == "10")
ess10 <- mean(data_finland10$imp_free)
#round 11
data_finland11 <- subset(data_finland, essround == "11")
ess11 <- mean(data_finland11$imp_free)

y_finland <- c(ess1,ess2,ess3,ess4,ess5,ess6,ess7,ess8,ess9,ess10,ess11)

#germany

data_germany <- subset(data1, cntry == "DE")
#round 1
data_germany1 <- subset(data_germany, essround == "1")
ess1 <- mean(data_germany1$imp_free)
#round 2
data_germany2 <- subset(data_germany, essround == "2")
ess2 <- mean(data_germany2$imp_free)
#round 3
data_germany3 <- subset(data_germany, essround == "3")
ess3 <- mean(data_germany3$imp_free)
#round 4
data_germany4 <- subset(data_germany, essround == "4")
ess4 <- mean(data_germany4$imp_free)
#round 5
data_germany5 <- subset(data_germany, essround == "5")
ess5 <- mean(data_germany5$imp_free)
#round 6
data_germany6 <- subset(data_germany, essround == "6")
ess6 <- mean(data_germany6$imp_free)
#round 7
data_germany7 <- subset(data_germany, essround == "7")
ess7 <- mean(data_germany7$imp_free)
#round 8
data_germany8 <- subset(data_germany, essround == "8")
ess8 <- mean(data_germany8$imp_free)
#round 9
data_germany9 <- subset(data_germany, essround == "9")
ess9 <- mean(data_germany9$imp_free)
#round 10
data_germany10 <- subset(data_germany, essround == "10")
ess10 <- mean(data_germany10$imp_free)
#round 11
data_germany11 <- subset(data_germany, essround == "11")
ess11 <- mean(data_germany11$imp_free)
y_germany <- c(ess1,ess2,ess3,ess4,ess5,ess6,ess7,ess8,ess9,5.041751,ess11)


#norway

data_norway <- subset(data1, cntry == "NO")
#round 1
data_norway1 <- subset(data_norway, essround == "1")
ess1 <- mean(data_norway1$imp_free)
#round 2
data_norway2 <- subset(data_norway, essround == "2")
ess2 <- mean(data_norway2$imp_free)
#round 3
data_norway3 <- subset(data_norway, essround == "3")
ess3 <- mean(data_norway3$imp_free)
#round 4
data_norway4 <- subset(data_norway, essround == "4")
ess4 <- mean(data_norway4$imp_free)
#round 5
data_norway5 <- subset(data_norway, essround == "5")
ess5 <- mean(data_norway5$imp_free)
#round 6
data_norway6 <- subset(data_norway, essround == "6")
ess6 <- mean(data_norway6$imp_free)
#round 7
data_norway7 <- subset(data_norway, essround == "7")
ess7 <- mean(data_norway7$imp_free)
#round 8
data_norway8 <- subset(data_norway, essround == "8")
ess8 <- mean(data_norway8$imp_free)
#round 9
data_norway9 <- subset(data_norway, essround == "9")
ess9 <- mean(data_norway9$imp_free)
#round 10
data_norway10 <- subset(data_norway, essround == "10")
ess10 <- mean(data_norway10$imp_free)
#round 11
data_norway11 <- subset(data_norway, essround == "11")
ess11 <- mean(data_norway11$imp_free)

y_norway <- c(ess1,ess2,ess3,ess4,ess5,ess6,ess7,ess8,ess9,ess10,ess11)

#hungary

data_hungary <- subset(data1, cntry == "HU")
#round 1
data_hungary1 <- subset(data_hungary, essround == "1")
ess1 <- mean(data_hungary1$imp_free)
#round 2
data_hungary2 <- subset(data_hungary, essround == "2")
ess2 <- mean(data_hungary2$imp_free)
#round 3
data_hungary3 <- subset(data_hungary, essround == "3")
ess3 <- mean(data_hungary3$imp_free)
#round 4
data_hungary4 <- subset(data_hungary, essround == "4")
ess4 <- mean(data_hungary4$imp_free)
#round 5
data_hungary5 <- subset(data_hungary, essround == "5")
ess5 <- mean(data_hungary5$imp_free)
#round 6
data_hungary6 <- subset(data_hungary, essround == "6")
ess6 <- mean(data_hungary6$imp_free)
#round 7
data_hungary7 <- subset(data_hungary, essround == "7")
ess7 <- mean(data_hungary7$imp_free)
#round 8
data_hungary8 <- subset(data_hungary, essround == "8")
ess8 <- mean(data_hungary8$imp_free)
#round 9
data_hungary9 <- subset(data_hungary, essround == "9")
ess9 <- mean(data_hungary9$imp_free)
#round 10
data_hungary10 <- subset(data_hungary, essround == "10")
ess10 <- mean(data_hungary10$imp_free)
#round 11
data_hungary11 <- subset(data_hungary, essround == "11")
ess11 <- mean(data_hungary11$imp_free)

y_hungary <- c(ess1,ess2,ess3,ess4,ess5,ess6,ess7,ess8,ess9,ess10,ess11)

#united kingdom

data_uk <- subset(data1, cntry == "GB")
#round 1
data_uk1 <- subset(data_uk, essround == "1")
ess1 <- mean(data_uk1$imp_free)
#round 2
data_uk2 <- subset(data_uk, essround == "2")
ess2 <- mean(data_uk2$imp_free)
#round 3
data_uk3 <- subset(data_uk, essround == "3")
ess3 <- mean(data_uk3$imp_free)
#round 4
data_uk4 <- subset(data_uk, essround == "4")
ess4 <- mean(data_uk4$imp_free)
#round 5
data_uk5 <- subset(data_uk, essround == "5")
ess5 <- mean(data_uk5$imp_free)
#round 6
data_uk6 <- subset(data_uk, essround == "6")
ess6 <- mean(data_uk6$imp_free)
#round 7
data_uk7 <- subset(data_uk, essround == "7")
ess7 <- mean(data_uk7$imp_free)
#round 8
data_uk8 <- subset(data_uk, essround == "8")
ess8 <- mean(data_uk8$imp_free)
#round 9
data_uk9 <- subset(data_uk, essround == "9")
ess9 <- mean(data_uk9$imp_free)
#round 10
data_uk10 <- subset(data_uk, essround == "10")
ess10 <- mean(data_uk10$imp_free)
#round 11
data_uk11 <- subset(data_uk, essround == "11")
ess11 <- mean(data_uk11$imp_free)

y_uk <- c(ess1,ess2,ess3,ess4,ess5,ess6,ess7,ess8,ess9,ess10,ess11)

#switzerland

data_switzerland <- subset(data1, cntry == "CH")
#round 1
data_switzerland1 <- subset(data_switzerland, essround == "1")
ess1 <- mean(data_switzerland1$imp_free)
#round 2
data_switzerland2 <- subset(data_switzerland, essround == "2")
ess2 <- mean(data_switzerland2$imp_free)
#round 3
data_switzerland3 <- subset(data_switzerland, essround == "3")
ess3 <- mean(data_switzerland3$imp_free)
#round 4
data_switzerland4 <- subset(data_switzerland, essround == "4")
ess4 <- mean(data_switzerland4$imp_free)
#round 5
data_switzerland5 <- subset(data_switzerland, essround == "5")
ess5 <- mean(data_switzerland5$imp_free)
#round 6
data_switzerland6 <- subset(data_switzerland, essround == "6")
ess6 <- mean(data_switzerland6$imp_free)
#round 7
data_switzerland7 <- subset(data_switzerland, essround == "7")
ess7 <- mean(data_switzerland7$imp_free)
#round 8
data_switzerland8 <- subset(data_switzerland, essround == "8")
ess8 <- mean(data_switzerland8$imp_free)
#round 9
data_switzerland9 <- subset(data_switzerland, essround == "9")
ess9 <- mean(data_switzerland9$imp_free)
#round 10
data_switzerland10 <- subset(data_switzerland, essround == "10")
ess10 <- mean(data_switzerland10$imp_free)
#round 11
data_switzerland11 <- subset(data_switzerland, essround == "11")
ess11 <- mean(data_switzerland11$imp_free)

y_switzerland <- c(ess1,ess2,ess3,ess4,ess5,ess6,ess7,ess8,ess9,ess10,ess11)

#netherlands

data_netherlands <- subset(data1, cntry == "NL")
#round 1
data_netherlands1 <- subset(data_netherlands, essround == "1")
ess1 <- mean(data_netherlands1$imp_free)
#round 2
data_netherlands2 <- subset(data_netherlands, essround == "2")
ess2 <- mean(data_netherlands2$imp_free)
#round 3
data_netherlands3 <- subset(data_netherlands, essround == "3")
ess3 <- mean(data_netherlands3$imp_free)
#round 4
data_netherlands4 <- subset(data_netherlands, essround == "4")
ess4 <- mean(data_netherlands4$imp_free)
#round 5
data_netherlands5 <- subset(data_netherlands, essround == "5")
ess5 <- mean(data_netherlands5$imp_free)
#round 6
data_netherlands6 <- subset(data_netherlands, essround == "6")
ess6 <- mean(data_netherlands6$imp_free)
#round 7
data_netherlands7 <- subset(data_netherlands, essround == "7")
ess7 <- mean(data_netherlands7$imp_free)
#round 8
data_netherlands8 <- subset(data_netherlands, essround == "8")
ess8 <- mean(data_netherlands8$imp_free)
#round 9
data_netherlands9 <- subset(data_netherlands, essround == "9")
ess9 <- mean(data_netherlands9$imp_free)
#round 10
data_netherlands10 <- subset(data_netherlands, essround == "10")
ess10 <- mean(data_netherlands10$imp_free)
#round 11
data_netherlands11 <- subset(data_netherlands, essround == "11")
ess11 <- mean(data_netherlands11$imp_free)

y_netherlands <- c(ess1,ess2,ess3,ess4,ess5,ess6,ess7,ess8,ess9,ess10,ess11)

#lituania

data_lituania <- subset(data1, cntry == "LT")
#round 1
data_lituania1 <- subset(data_lituania, essround == "1")
ess1 <- mean(data_lituania1$imp_free)
#round 2
data_lituania2 <- subset(data_lituania, essround == "2")
ess2 <- mean(data_lituania2$imp_free)
#round 3
data_lituania3 <- subset(data_lituania, essround == "3")
ess3 <- mean(data_lituania3$imp_free)
#round 4
data_lituania4 <- subset(data_lituania, essround == "4")
ess4 <- mean(data_lituania4$imp_free)
#round 5
data_lituania5 <- subset(data_lituania, essround == "5")
ess5 <- mean(data_lituania5$imp_free)
#round 6
data_lituania6 <- subset(data_lituania, essround == "6")
ess6 <- mean(data_lituania6$imp_free)
#round 7
data_lituania7 <- subset(data_lituania, essround == "7")
ess7 <- mean(data_lituania7$imp_free)
#round 8
data_lituania8 <- subset(data_lituania, essround == "8")
ess8 <- mean(data_lituania8$imp_free)
#round 9
data_lituania9 <- subset(data_lituania, essround == "9")
ess9 <- mean(data_lituania9$imp_free)
#round 10
data_lituania10 <- subset(data_lituania, essround == "10")
ess10 <- mean(data_lituania10$imp_free)
#round 11
data_lituania11 <- subset(data_lituania, essround == "11")
ess11 <- mean(data_lituania11$imp_free)

y_lituania <- c(ess1,ess2,ess3,ess4,ess5,ess6,ess7,ess8,ess9,ess10,ess11)


#slovenia

data_slovenia <- subset(data1, cntry == "SI")
#round 1
data_slovenia1 <- subset(data_slovenia, essround == "1")
ess1 <- mean(data_slovenia1$imp_free)
#round 2
data_slovenia2 <- subset(data_slovenia, essround == "2")
ess2 <- mean(data_slovenia2$imp_free)
#round 3
data_slovenia3 <- subset(data_slovenia, essround == "3")
ess3 <- mean(data_slovenia3$imp_free)
#round 4
data_slovenia4 <- subset(data_slovenia, essround == "4")
ess4 <- mean(data_slovenia4$imp_free)
#round 5
data_slovenia5 <- subset(data_slovenia, essround == "5")
ess5 <- mean(data_slovenia5$imp_free)
#round 6
data_slovenia6 <- subset(data_slovenia, essround == "6")
ess6 <- mean(data_slovenia6$imp_free)
#round 7
data_slovenia7 <- subset(data_slovenia, essround == "7")
ess7 <- mean(data_slovenia7$imp_free)
#round 8
data_slovenia8 <- subset(data_slovenia, essround == "8")
ess8 <- mean(data_slovenia8$imp_free)
#round 9
data_slovenia9 <- subset(data_slovenia, essround == "9")
ess9 <- mean(data_slovenia9$imp_free)
#round 10
data_slovenia10 <- subset(data_slovenia, essround == "10")
ess10 <- mean(data_slovenia10$imp_free)
#round 11
data_slovenia11 <- subset(data_slovenia, essround == "11")
ess11 <- mean(data_slovenia11$imp_free)

y_slovenia <- c(ess1,ess2,ess3,ess4,ess5,ess6,ess7,ess8,ess9,ess10,ess11)

#austria

data_austria <- subset(data1, cntry == "AT")
#round 1
data_austria1 <- subset(data_austria, essround == "1")
ess1 <- mean(data_austria1$imp_free)
#round 2
data_austria2 <- subset(data_austria, essround == "2")
ess2 <- mean(data_austria2$imp_free)
#round 3
data_austria3 <- subset(data_austria, essround == "3")
ess3 <- mean(data_austria3$imp_free)
#round 4
data_austria4 <- subset(data_austria, essround == "4")
ess4 <- mean(data_austria4$imp_free)
#round 5
data_austria5 <- subset(data_austria, essround == "5")
ess5 <- mean(data_austria5$imp_free)
#round 6
data_austria6 <- subset(data_austria, essround == "6")
ess6 <- mean(data_austria6$imp_free)
#round 7
data_austria7 <- subset(data_austria, essround == "7")
ess7 <- mean(data_austria7$imp_free)
#round 8
data_austria8 <- subset(data_austria, essround == "8")
ess8 <- mean(data_austria8$imp_free)
#round 9
data_austria9 <- subset(data_austria, essround == "9")
ess9 <- mean(data_austria9$imp_free)
#round 10
data_austria10 <- subset(data_austria, essround == "10")
ess10 <- mean(data_austria10$imp_free)
#round 11
data_austria11 <- subset(data_austria, essround == "11")
ess11 <- mean(data_austria11$imp_free)

y_austria <- c(ess1,ess2,ess3,ess4,ess5,ess6,ess7,ess8,ess9,ess10,ess11)


#croatia

data_croatia <- subset(data1, cntry == "HR")
#round 1
data_croatia1 <- subset(data_croatia, essround == "1")
ess1 <- mean(data_croatia1$imp_free)
#round 2
data_croatia2 <- subset(data_croatia, essround == "2")
ess2 <- mean(data_croatia2$imp_free)
#round 3
data_croatia3 <- subset(data_croatia, essround == "3")
ess3 <- mean(data_croatia3$imp_free)
#round 4
data_croatia4 <- subset(data_croatia, essround == "4")
ess4 <- mean(data_croatia4$imp_free)
#round 5
data_croatia5 <- subset(data_croatia, essround == "5")
ess5 <- mean(data_croatia5$imp_free)
#round 6
data_croatia6 <- subset(data_croatia, essround == "6")
ess6 <- mean(data_croatia6$imp_free)
#round 7
data_croatia7 <- subset(data_croatia, essround == "7")
ess7 <- mean(data_croatia7$imp_free)
#round 8
data_croatia8 <- subset(data_croatia, essround == "8")
ess8 <- mean(data_croatia8$imp_free)
#round 9
data_croatia9 <- subset(data_croatia, essround == "9")
ess9 <- mean(data_croatia9$imp_free)
#round 10
data_croatia10 <- subset(data_croatia, essround == "10")
ess10 <- mean(data_croatia10$imp_free)
#round 11
data_croatia11 <- subset(data_croatia, essround == "11")
ess11 <- mean(data_croatia11$imp_free)

y_croatia <- c(ess1,ess2,ess3,ess4,ess5,ess6,ess7,ess8,ess9,ess10,ess11)

xValue <- 1:11
matplot(xValue, cbind(y_finland, y_hungary, y_germany, y_ireland, y_norway, y_uk, y_switzerland, y_netherlands, y_lituania, y_slovenia, y_croatia, y_austria), type = "l", lty = 1, 
        col = c("red", "blue", "green","yellow","orange","brown","violet", "black", "purple","grey","cyan","chocolate"), xlab = "X", 
        ylab = "Y", main = "Comparison trends in individualism") + abline(v=9, col="black")

y_austria[y_austria=="NaN"] <- 0
y_germany[y_germany=="NaN"] <- 0
y_counterfactual <- (y_austria + y_germany)/2

y_counterfactual[4] <- y_germany[4]
y_counterfactual[5] <- y_germany[5]
y_counterfactual[6] <- y_germany[6]
y_counterfactual[10] <- y_germany[10]

matplot(xValue, cbind(y_counterfactual, y_switzerland), type = "l", lty = 1, 
        col = c("red", "blue"), xlab = "X", 
        ylab = "Y", main = "Comparison trends in individualism") + abline(v=9, col="black")

#Did

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#!!!note that you should account for post-stratification!!!!!!
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

data_did <- subset(data1, cntry == "DE" | cntry == "AT" | cntry == "CH")
data_did$post <- ifelse(data_did$essround <= 9, 0,1)
data_did$treat <- ifelse(data_did$cntry == "CH", 0,1)
summary(data_did$post)
summary(data_did$treat)

reg1 <- lm(imp_free ~ treat + post + treat*post, data = data_did)
coeftest(reg1, vcov = vcovHC(reg1))

#controls
reg2 <- lm(imp_free ~ treat + post + treat*post +agea +gndr + eduyrs, data = data_did)
coeftest(reg2, vcov = vcovHC(reg2))

reg3 <- lm(imp_free ~ treat + post + treat*post +agea +gndr + eduyrs + ppltrst + rlgdgr + gincdif, data = data_did)
coeftest(reg3, vcov = vcovHC(reg3))


#interactions

#adding controls and subtreatment effects

#diff in diff imp_people

#ireland
data_ireland <- subset(data1, cntry == "IE")
#round 1
data_ireland1 <- subset(data_ireland, essround == "1")
ess1 <- mean(data_ireland1$imp_people)
#round 2
data_ireland2 <- subset(data_ireland, essround == "2")
ess2 <- mean(data_ireland2$imp_people)
#round 3
data_ireland3 <- subset(data_ireland, essround == "3")
ess3 <- mean(data_ireland3$imp_people)
#round 4
data_ireland4 <- subset(data_ireland, essround == "4")
ess4 <- mean(data_ireland4$imp_people)
#round 5
data_ireland5 <- subset(data_ireland, essround == "5")
ess5 <- mean(data_ireland5$imp_people)
#round 6
data_ireland6 <- subset(data_ireland, essround == "6")
ess6 <- mean(data_ireland6$imp_people)
#round 7
data_ireland7 <- subset(data_ireland, essround == "7")
ess7 <- mean(data_ireland7$imp_people)
#round 8
data_ireland8 <- subset(data_ireland, essround == "8")
ess8 <- mean(data_ireland8$imp_people)
#round 9
data_ireland9 <- subset(data_ireland, essround == "9")
ess9 <- mean(data_ireland9$imp_people)
#round 10
data_ireland10 <- subset(data_ireland, essround == "10")
ess10 <- mean(data_ireland10$imp_people)
#round 11
data_ireland11 <- subset(data_ireland, essround == "11")
ess11 <- mean(data_ireland11$imp_people)

yp_ireland <- c(ess1,ess2,ess3,ess4,ess5,ess6,ess7,ess8,ess9,ess10,ess11)

#finland
data_finland <- subset(data1, cntry == "FI")
#round 1
data_finland1 <- subset(data_finland, essround == "1")
ess1 <- mean(data_finland1$imp_people)
#round 2
data_finland2 <- subset(data_finland, essround == "2")
ess2 <- mean(data_finland2$imp_people)
#round 3
data_finland3 <- subset(data_finland, essround == "3")
ess3 <- mean(data_finland3$imp_people)
#round 4
data_finland4 <- subset(data_finland, essround == "4")
ess4 <- mean(data_finland4$imp_people)
#round 5
data_finland5 <- subset(data_finland, essround == "5")
ess5 <- mean(data_finland5$imp_people)
#round 6
data_finland6 <- subset(data_finland, essround == "6")
ess6 <- mean(data_finland6$imp_people)
#round 7
data_finland7 <- subset(data_finland, essround == "7")
ess7 <- mean(data_finland7$imp_people)
#round 8
data_finland8 <- subset(data_finland, essround == "8")
ess8 <- mean(data_finland8$imp_people)
#round 9
data_finland9 <- subset(data_finland, essround == "9")
ess9 <- mean(data_finland9$imp_people)
#round 10
data_finland10 <- subset(data_finland, essround == "10")
ess10 <- mean(data_finland10$imp_people)
#round 11
data_finland11 <- subset(data_finland, essround == "11")
ess11 <- mean(data_finland11$imp_people)

yp_finland <- c(ess1,ess2,ess3,ess4,ess5,ess6,ess7,ess8,ess9,ess10,ess11)

#germany

data_germany <- subset(data1, cntry == "DE")
#round 1
data_germany1 <- subset(data_germany, essround == "1")
ess1 <- mean(data_germany1$imp_people)
#round 2
data_germany2 <- subset(data_germany, essround == "2")
ess2 <- mean(data_germany2$imp_people)
#round 3
data_germany3 <- subset(data_germany, essround == "3")
ess3 <- mean(data_germany3$imp_people)
#round 4
data_germany4 <- subset(data_germany, essround == "4")
ess4 <- mean(data_germany4$imp_people)
#round 5
data_germany5 <- subset(data_germany, essround == "5")
ess5 <- mean(data_germany5$imp_people)
#round 6
data_germany6 <- subset(data_germany, essround == "6")
ess6 <- mean(data_germany6$imp_people)
#round 7
data_germany7 <- subset(data_germany, essround == "7")
ess7 <- mean(data_germany7$imp_people)
#round 8
data_germany8 <- subset(data_germany, essround == "8")
ess8 <- mean(data_germany8$imp_people)
#round 9
data_germany9 <- subset(data_germany, essround == "9")
ess9 <- mean(data_germany9$imp_people)
#round 10
data_germany10 <- subset(data_germany, essround == "10")
ess10 <- mean(data_germany10$imp_people)
#round 11
data_germany11 <- subset(data_germany, essround == "11")
ess11 <- mean(data_germany11$imp_people)
yp_germany <- c(ess1,ess2,ess3,ess4,ess5,ess6,ess7,ess8,ess9,4.924231,ess11)


#norway

data_norway <- subset(data1, cntry == "NO")
#round 1
data_norway1 <- subset(data_norway, essround == "1")
ess1 <- mean(data_norway1$imp_people)
#round 2
data_norway2 <- subset(data_norway, essround == "2")
ess2 <- mean(data_norway2$imp_people)
#round 3
data_norway3 <- subset(data_norway, essround == "3")
ess3 <- mean(data_norway3$imp_people)
#round 4
data_norway4 <- subset(data_norway, essround == "4")
ess4 <- mean(data_norway4$imp_people)
#round 5
data_norway5 <- subset(data_norway, essround == "5")
ess5 <- mean(data_norway5$imp_people)
#round 6
data_norway6 <- subset(data_norway, essround == "6")
ess6 <- mean(data_norway6$imp_people)
#round 7
data_norway7 <- subset(data_norway, essround == "7")
ess7 <- mean(data_norway7$imp_people)
#round 8
data_norway8 <- subset(data_norway, essround == "8")
ess8 <- mean(data_norway8$imp_people)
#round 9
data_norway9 <- subset(data_norway, essround == "9")
ess9 <- mean(data_norway9$imp_people)
#round 10
data_norway10 <- subset(data_norway, essround == "10")
ess10 <- mean(data_norway10$imp_people)
#round 11
data_norway11 <- subset(data_norway, essround == "11")
ess11 <- mean(data_norway11$imp_people)

yp_norway <- c(ess1,ess2,ess3,ess4,ess5,ess6,ess7,ess8,ess9,ess10,ess11)

#hungary

data_hungary <- subset(data1, cntry == "HU")
#round 1
data_hungary1 <- subset(data_hungary, essround == "1")
ess1 <- mean(data_hungary1$imp_people)
#round 2
data_hungary2 <- subset(data_hungary, essround == "2")
ess2 <- mean(data_hungary2$imp_people)
#round 3
data_hungary3 <- subset(data_hungary, essround == "3")
ess3 <- mean(data_hungary3$imp_people)
#round 4
data_hungary4 <- subset(data_hungary, essround == "4")
ess4 <- mean(data_hungary4$imp_people)
#round 5
data_hungary5 <- subset(data_hungary, essround == "5")
ess5 <- mean(data_hungary5$imp_people)
#round 6
data_hungary6 <- subset(data_hungary, essround == "6")
ess6 <- mean(data_hungary6$imp_people)
#round 7
data_hungary7 <- subset(data_hungary, essround == "7")
ess7 <- mean(data_hungary7$imp_people)
#round 8
data_hungary8 <- subset(data_hungary, essround == "8")
ess8 <- mean(data_hungary8$imp_people)
#round 9
data_hungary9 <- subset(data_hungary, essround == "9")
ess9 <- mean(data_hungary9$imp_people)
#round 10
data_hungary10 <- subset(data_hungary, essround == "10")
ess10 <- mean(data_hungary10$imp_people)
#round 11
data_hungary11 <- subset(data_hungary, essround == "11")
ess11 <- mean(data_hungary11$imp_people)

yp_hungary <- c(ess1,ess2,ess3,ess4,ess5,ess6,ess7,ess8,ess9,ess10,ess11)

#united kingdom

data_uk <- subset(data1, cntry == "GB")
#round 1
data_uk1 <- subset(data_uk, essround == "1")
ess1 <- mean(data_uk1$imp_people)
#round 2
data_uk2 <- subset(data_uk, essround == "2")
ess2 <- mean(data_uk2$imp_people)
#round 3
data_uk3 <- subset(data_uk, essround == "3")
ess3 <- mean(data_uk3$imp_people)
#round 4
data_uk4 <- subset(data_uk, essround == "4")
ess4 <- mean(data_uk4$imp_people)
#round 5
data_uk5 <- subset(data_uk, essround == "5")
ess5 <- mean(data_uk5$imp_people)
#round 6
data_uk6 <- subset(data_uk, essround == "6")
ess6 <- mean(data_uk6$imp_people)
#round 7
data_uk7 <- subset(data_uk, essround == "7")
ess7 <- mean(data_uk7$imp_people)
#round 8
data_uk8 <- subset(data_uk, essround == "8")
ess8 <- mean(data_uk8$imp_people)
#round 9
data_uk9 <- subset(data_uk, essround == "9")
ess9 <- mean(data_uk9$imp_people)
#round 10
data_uk10 <- subset(data_uk, essround == "10")
ess10 <- mean(data_uk10$imp_people)
#round 11
data_uk11 <- subset(data_uk, essround == "11")
ess11 <- mean(data_uk11$imp_people)

yp_uk <- c(ess1,ess2,ess3,ess4,ess5,ess6,ess7,ess8,ess9,ess10,ess11)

#switzerland

data_switzerland <- subset(data1, cntry == "CH")
#round 1
data_switzerland1 <- subset(data_switzerland, essround == "1")
ess1 <- mean(data_switzerland1$imp_people)
#round 2
data_switzerland2 <- subset(data_switzerland, essround == "2")
ess2 <- mean(data_switzerland2$imp_people)
#round 3
data_switzerland3 <- subset(data_switzerland, essround == "3")
ess3 <- mean(data_switzerland3$imp_people)
#round 4
data_switzerland4 <- subset(data_switzerland, essround == "4")
ess4 <- mean(data_switzerland4$imp_people)
#round 5
data_switzerland5 <- subset(data_switzerland, essround == "5")
ess5 <- mean(data_switzerland5$imp_people)
#round 6
data_switzerland6 <- subset(data_switzerland, essround == "6")
ess6 <- mean(data_switzerland6$imp_people)
#round 7
data_switzerland7 <- subset(data_switzerland, essround == "7")
ess7 <- mean(data_switzerland7$imp_people)
#round 8
data_switzerland8 <- subset(data_switzerland, essround == "8")
ess8 <- mean(data_switzerland8$imp_people)
#round 9
data_switzerland9 <- subset(data_switzerland, essround == "9")
ess9 <- mean(data_switzerland9$imp_people)
#round 10
data_switzerland10 <- subset(data_switzerland, essround == "10")
ess10 <- mean(data_switzerland10$imp_people)
#round 11
data_switzerland11 <- subset(data_switzerland, essround == "11")
ess11 <- mean(data_switzerland11$imp_people)

yp_switzerland <- c(ess1,ess2,ess3,ess4,ess5,ess6,ess7,ess8,ess9,ess10,ess11)

#netherlands

data_netherlands <- subset(data1, cntry == "NL")
#round 1
data_netherlands1 <- subset(data_netherlands, essround == "1")
ess1 <- mean(data_netherlands1$imp_people)
#round 2
data_netherlands2 <- subset(data_netherlands, essround == "2")
ess2 <- mean(data_netherlands2$imp_people)
#round 3
data_netherlands3 <- subset(data_netherlands, essround == "3")
ess3 <- mean(data_netherlands3$imp_people)
#round 4
data_netherlands4 <- subset(data_netherlands, essround == "4")
ess4 <- mean(data_netherlands4$imp_people)
#round 5
data_netherlands5 <- subset(data_netherlands, essround == "5")
ess5 <- mean(data_netherlands5$imp_people)
#round 6
data_netherlands6 <- subset(data_netherlands, essround == "6")
ess6 <- mean(data_netherlands6$imp_people)
#round 7
data_netherlands7 <- subset(data_netherlands, essround == "7")
ess7 <- mean(data_netherlands7$imp_people)
#round 8
data_netherlands8 <- subset(data_netherlands, essround == "8")
ess8 <- mean(data_netherlands8$imp_people)
#round 9
data_netherlands9 <- subset(data_netherlands, essround == "9")
ess9 <- mean(data_netherlands9$imp_people)
#round 10
data_netherlands10 <- subset(data_netherlands, essround == "10")
ess10 <- mean(data_netherlands10$imp_people)
#round 11
data_netherlands11 <- subset(data_netherlands, essround == "11")
ess11 <- mean(data_netherlands11$imp_people)

yp_netherlands <- c(ess1,ess2,ess3,ess4,ess5,ess6,ess7,ess8,ess9,ess10,ess11)

#lituania

data_lituania <- subset(data1, cntry == "LT")
#round 1
data_lituania1 <- subset(data_lituania, essround == "1")
ess1 <- mean(data_lituania1$imp_people)
#round 2
data_lituania2 <- subset(data_lituania, essround == "2")
ess2 <- mean(data_lituania2$imp_people)
#round 3
data_lituania3 <- subset(data_lituania, essround == "3")
ess3 <- mean(data_lituania3$imp_people)
#round 4
data_lituania4 <- subset(data_lituania, essround == "4")
ess4 <- mean(data_lituania4$imp_people)
#round 5
data_lituania5 <- subset(data_lituania, essround == "5")
ess5 <- mean(data_lituania5$imp_people)
#round 6
data_lituania6 <- subset(data_lituania, essround == "6")
ess6 <- mean(data_lituania6$imp_people)
#round 7
data_lituania7 <- subset(data_lituania, essround == "7")
ess7 <- mean(data_lituania7$imp_people)
#round 8
data_lituania8 <- subset(data_lituania, essround == "8")
ess8 <- mean(data_lituania8$imp_people)
#round 9
data_lituania9 <- subset(data_lituania, essround == "9")
ess9 <- mean(data_lituania9$imp_people)
#round 10
data_lituania10 <- subset(data_lituania, essround == "10")
ess10 <- mean(data_lituania10$imp_people)
#round 11
data_lituania11 <- subset(data_lituania, essround == "11")
ess11 <- mean(data_lituania11$imp_people)

yp_lituania <- c(ess1,ess2,ess3,ess4,ess5,ess6,ess7,ess8,ess9,ess10,ess11)


#slovenia

data_slovenia <- subset(data1, cntry == "SI")
#round 1
data_slovenia1 <- subset(data_slovenia, essround == "1")
ess1 <- mean(data_slovenia1$imp_people)
#round 2
data_slovenia2 <- subset(data_slovenia, essround == "2")
ess2 <- mean(data_slovenia2$imp_people)
#round 3
data_slovenia3 <- subset(data_slovenia, essround == "3")
ess3 <- mean(data_slovenia3$imp_people)
#round 4
data_slovenia4 <- subset(data_slovenia, essround == "4")
ess4 <- mean(data_slovenia4$imp_people)
#round 5
data_slovenia5 <- subset(data_slovenia, essround == "5")
ess5 <- mean(data_slovenia5$imp_people)
#round 6
data_slovenia6 <- subset(data_slovenia, essround == "6")
ess6 <- mean(data_slovenia6$imp_people)
#round 7
data_slovenia7 <- subset(data_slovenia, essround == "7")
ess7 <- mean(data_slovenia7$imp_people)
#round 8
data_slovenia8 <- subset(data_slovenia, essround == "8")
ess8 <- mean(data_slovenia8$imp_people)
#round 9
data_slovenia9 <- subset(data_slovenia, essround == "9")
ess9 <- mean(data_slovenia9$imp_people)
#round 10
data_slovenia10 <- subset(data_slovenia, essround == "10")
ess10 <- mean(data_slovenia10$imp_people)
#round 11
data_slovenia11 <- subset(data_slovenia, essround == "11")
ess11 <- mean(data_slovenia11$imp_people)

yp_slovenia <- c(ess1,ess2,ess3,ess4,ess5,ess6,ess7,ess8,ess9,ess10,ess11)

#austria

data_austria <- subset(data1, cntry == "AT")
#round 1
data_austria1 <- subset(data_austria, essround == "1")
ess1 <- mean(data_austria1$imp_people)
#round 2
data_austria2 <- subset(data_austria, essround == "2")
ess2 <- mean(data_austria2$imp_people)
#round 3
data_austria3 <- subset(data_austria, essround == "3")
ess3 <- mean(data_austria3$imp_people)
#round 4
data_austria4 <- subset(data_austria, essround == "4")
ess4 <- mean(data_austria4$imp_people)
#round 5
data_austria5 <- subset(data_austria, essround == "5")
ess5 <- mean(data_austria5$imp_people)
#round 6
data_austria6 <- subset(data_austria, essround == "6")
ess6 <- mean(data_austria6$imp_people)
#round 7
data_austria7 <- subset(data_austria, essround == "7")
ess7 <- mean(data_austria7$imp_people)
#round 8
data_austria8 <- subset(data_austria, essround == "8")
ess8 <- mean(data_austria8$imp_people)
#round 9
data_austria9 <- subset(data_austria, essround == "9")
ess9 <- mean(data_austria9$imp_people)
#round 10
data_austria10 <- subset(data_austria, essround == "10")
ess10 <- mean(data_austria10$imp_people)
#round 11
data_austria11 <- subset(data_austria, essround == "11")
ess11 <- mean(data_austria11$imp_people)

yp_austria <- c(ess1,ess2,ess3,ess4,ess5,ess6,ess7,ess8,ess9,ess10,ess11)


#croatia

data_croatia <- subset(data1, cntry == "HR")
#round 1
data_croatia1 <- subset(data_croatia, essround == "1")
ess1 <- mean(data_croatia1$imp_people)
#round 2
data_croatia2 <- subset(data_croatia, essround == "2")
ess2 <- mean(data_croatia2$imp_people)
#round 3
data_croatia3 <- subset(data_croatia, essround == "3")
ess3 <- mean(data_croatia3$imp_people)
#round 4
data_croatia4 <- subset(data_croatia, essround == "4")
ess4 <- mean(data_croatia4$imp_people)
#round 5
data_croatia5 <- subset(data_croatia, essround == "5")
ess5 <- mean(data_croatia5$imp_people)
#round 6
data_croatia6 <- subset(data_croatia, essround == "6")
ess6 <- mean(data_croatia6$imp_people)
#round 7
data_croatia7 <- subset(data_croatia, essround == "7")
ess7 <- mean(data_croatia7$imp_people)
#round 8
data_croatia8 <- subset(data_croatia, essround == "8")
ess8 <- mean(data_croatia8$imp_people)
#round 9
data_croatia9 <- subset(data_croatia, essround == "9")
ess9 <- mean(data_croatia9$imp_people)
#round 10
data_croatia10 <- subset(data_croatia, essround == "10")
ess10 <- mean(data_croatia10$imp_people)
#round 11
data_croatia11 <- subset(data_croatia, essround == "11")
ess11 <- mean(data_croatia11$imp_people)

yp_croatia <- c(ess1,ess2,ess3,ess4,ess5,ess6,ess7,ess8,ess9,ess10,ess11)

xValue <- 1:11
matplot(xValue, cbind(yp_finland, yp_hungary, yp_germany, yp_ireland, yp_norway, yp_uk, yp_switzerland, yp_netherlands, yp_lituania, yp_slovenia, yp_croatia), type = "l", lty = 1, 
        col = c("red", "blue", "green","yellow","orange","brown","violet", "black", "purple","grey","cyan","chocolate"), xlab = "X", 
        ylab = "Y", main = "Comparison trends in collectivism") + abline(v=9, col="black")


yp_austria[yp_austria=="NaN"] <- 0
yp_germany[yp_germany=="NaN"] <- 0
yp_counterfactual <- (yp_austria + yp_germany)/2

yp_counterfactual[4] <- yp_germany[4]
yp_counterfactual[5] <- yp_germany[5]
yp_counterfactual[6] <- yp_germany[6]
yp_counterfactual[10] <- yp_germany[10]

matplot(xValue, cbind(yp_counterfactual, yp_switzerland), type = "l", lty = 1, 
        col = c("red", "blue"), xlab = "X", 
        ylab = "Y", main = "Comparison trends in collectivism") + abline(v=9, col="black")









#dummies for treatment and post
data$post <- ifelse(data$year <= 1993, 0,1)
data$wom_treat <- ifelse(data$children >= 1,1,0)
summary(data$post)
summary(data$wom_treat)

#iii
#calculation of the means for did
y_t_p = (subset(data, wom_treat == 1 &  post == 1))
y_t_p1 <- mean(y_t_p$work)
y_t_p = (subset(data, wom_treat == 1 &  post == 0))
y_t_p2 <- mean(y_t_p$work)
y_t_p = (subset(data, wom_treat == 0 &  post == 1))
y_t_p3 <- mean(y_t_p$work)
y_t_p = (subset(data, wom_treat == 0 &  post == 0))
y_t_p4 <- mean(y_t_p$work)

#did
a <- (y_t_p1 - y_t_p2) 
b <-(y_t_p3 - y_t_p4)
a-b

#iv
#regression specification of DiD
reg1 <- lm(work ~ wom_treat + post + wom_treat*post, data = data)
coeftest(reg1, vcov = vcovHC(reg1))

#v
#regression + controls
reg2 <- lm(work ~ wom_treat + post + wom_treat*post + nonwhite + age + ed, data = data)
coeftest(reg2, vcov = vcovHC(reg2))


#vi
#regression + more controls
reg3 <- lm(work ~ wom_treat + post + wom_treat*post + nonwhite + age + ed + unearn, data = data)
coeftest(reg3, vcov = vcovHC(reg3))