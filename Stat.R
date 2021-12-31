library(readr)
library(tidyverse)
library(ggplot2)
library(nnet)
library(MASS)

main_data<- read_csv("291221_Desc_Data.csv")
SSP_imaging <- subset(main_data, main_data$SummaryGroup == "2" | main_data$SummaryGroup == "5" | main_data$SummaryGroup == "9")

#cleaning of variables
SSP_imaging$First_imaging <- as.factor(SSP_imaging$First_imaging)
SSP_imaging$age_TK <- as.numeric(SSP_imaging$Age)
SSP_imaging$gender_TK <- as.factor(SSP_imaging$Sex)
SSP_imaging$prevStone_TK <-as.factor(SSP_imaging$PreviousStoneHistory)
SSP_imaging$hydronephrosis_TK <-as.factor(SSP_imaging$Hydronephrosis)
SSP_imaging$hydroureter_TK <-as.factor(SSP_imaging$Hydroureter)
SSP_imaging$perinephricstranding_TK <- as.factor(SSP_imaging$Perinephricstranding)
SSP_imaging$NSAIDGiven_TK <- as.factor(SSP_imaging$NSAIDGiven)
SSP_imaging$metGiven_TK <- as.factor(SSP_imaging$MedicallyExpulsiveTherapyMETGiven)
SSP_imaging <- SSP_imaging %>% 
      mutate(CRP_TK = if_else(SSP_imaging$CRP.numeric < 5, "0-4.9",
                                  if_else(SSP_imaging$CRP.numeric >= 10, ">=10", "5.0-9.9"))) %>% 
      mutate(stonesize_TK = if_else(SSP_imaging$StoneSizemm <= 5, "0-5",
                          if_else(SSP_imaging$CRP.numeric >7, ">7", ">5-7"))) 
       
SSP_imaging$side_TK <- as.factor(SSP_imaging$Side)
SSP_imaging$stoneposition_TK <- as.factor(SSP_imaging$StonePosition)
SSP_imaging$stonesize_TK <- as.factor(SSP_imaging$stonesize_TK)

#multinommial logistic regression
SSP_imaging$First_imaging<- relevel(SSP_imaging$First_imaging, ref = "CT")
SSP_imaging$stonesize_TK<- relevel(SSP_imaging$stonesize_TK, ref = "0-5")

mylogit <- multinom(First_imaging ~ age_TK + gender_TK + prevStone_TK + hydronephrosis_TK + hydroureter_TK +
                 perinephricstranding_TK + NSAIDGiven_TK + metGiven_TK + CRP_TK + stonesize_TK +
                 side_TK + stoneposition_TK + WBC109L
                 , data = SSP_imaging)
summary(mylogit)
exp(coef(mylogit))

z <- summary(mylogit)$coefficients/summary(mylogit)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

#calculation of 95CI
exp(confint(mylogit))
