## Joon Chung
## Contact: see README
## 1/17/2022

library(survival)
library(survminer)
library(tableone)

## SHS Q3 scores
favstats(shs_df$pc1_scaled)
favstats(shs_df$shs)

shs_df$shs_di <- ifelse(shs_df$pc1_scaled > 0.4842288, 1, 0)
table(shs_df$shs_di, shs_df$death)
table(shs_df$shs_di)

shs_df$shs_di_raw <- ifelse(shs_df$shs >6, 1, 0)
table(shs_df$shs_di_raw, shs_df$death)
table(shs_df$shs_di_raw)

## For sensitivity, code copd, cancer as self-report?

shs_df <- merge(shs_df, cancer_df, by = "idno")

## SHS Model 0 ####
## Sleep Health Score regression (continuous)
shs_model_0 <- coxph(Surv(dthtt-slpexam1, death) ~ pc1_scaled*female +  age5c + race + 
                      income_tertile + 
                       degree_attain + married, data = shs_df)

summary(shs_model_0)
ShowRegTable(shs_model_0)

write.csv(ShowRegTable(shs_model_0), file = "pc1 x female.csv")
## SHS Model 1 ####
shs_model_1 <- coxph(Surv(dthtt-slpexam1, death) ~ pc1_scaled + age5c + female + race + 
                       modvig_pa + smoke + income_tertile + 
                       degree_attain + married + aehi_10, data = shs_df)

summary(shs_model_1)
ShowRegTable(shs_model_1)

# 
# di_model_1 <- coxph(Surv(dthtt-slpexam1, death) ~ shs_di + age5c + female + race + 
#                        modvig_pa + smoke + income_tertile + 
#                        degree_attain + married + aehi_10, data = shs_df)
# 
# summary(di_model_1)
# ShowRegTable(di_model_1)

## Sleep Health Score regressions BMI + prevalent disease adjusted
shs_model_2 <- coxph(Surv(dthtt-slpexam1, death) ~ pc1_scaled + age5c + female + race + 
                       modvig_pa + smoke + income_tertile + 
                       degree_attain + married + aehi_10 + bmi + prev_cvd , data = shs_df)

summary(shs_model_2)
ShowRegTable(shs_model_2)


# di_model_2 <- coxph(Surv(dthtt-slpexam1, death) ~ shs_di + age5c + female + race + 
#                        modvig_pa + smoke + income_tertile + 
#                        degree_attain + married + aehi_10 + bmi + prev_cvd + emphys_f + cancer, data = shs_df)
# 
# summary(di_model_2)
# ShowRegTable(di_model_2)

## Assumption test:
shs_test <- cox.zph(shs_model_2)
# di_test <- cox.zph(di_model_2)

# shs_test
# chisq df     p
# pc1_scaled      0.000425  1 0.984
# age5c           0.033606  1 0.855
# female          0.057931  1 0.810
# race            5.488118  3 0.139
# modvig_pa       0.930097  1 0.335
# smoke           3.331285  4 0.504
# income_tertile  2.139730  2 0.343
# degree_attain   5.129512  3 0.163
# married         0.479250  1 0.489
# aehi_10         0.276671  1 0.599
# bmi             1.974185  1 0.160
# prev_cvd        3.394562  1 0.065
# emphys_f        2.361178  1 0.124
# cancer          0.111983  1 0.738
# GLOBAL         25.530534 22 0.272


## Sensitivity models: 
# Exclude deaths w/in one year
shs_df$death_lag <- shs_df$dthtt/365 - shs_df$slpexam1/365


sensitivity_df <- shs_df %>% dplyr::filter(death_lag > 1)
table(sensitivity_df$death) # Excluding w/in 1 year of sleep exam

table(shs_df$death) # Total sample
## SHS regression
sens_shs_1yr <- coxph(Surv(dthtt-slpexam1, death) ~ pc1_scaled + age5c + female + race + 
                           modvig_pa + smoke + income_tertile + 
                           degree_attain + aehi_10, data = sensitivity_df)
summary(sens_shs_1yr)
ShowRegTable(sens_shs_1yr)

ShowRegTable(coxph(Surv(dthtt-slpexam1, death) ~ pc1_scaled + age5c + female + race + # Base
                     income_tertile + 
                     degree_attain + married, data = sensitivity_df))

ShowRegTable( coxph(Surv(dthtt-slpexam1, death) ~ pc1_scaled + age5c + female + race + # Medical comorbidity
                      modvig_pa + smoke + income_tertile + 
                      degree_attain + married + aehi_10 + prev_cvd + prev_cancer, data = sensitivity_df))






# PC score regression
pc1_model_1_spline <- coxph(Surv(dthtt-slpexam1, death) ~ pspline(pc1_scaled, df = 3) + 
                              age5c + female + race + 
                              modvig_pa + smoke + income_tertile + 
                              degree_attain + married + aehi_10, data = shs_df)

summary(pc1_model_1_spline)  # 0.55 Non-linear
termplot(pc1_model_1_spline, term=1, se=TRUE, col.term=1, col.se=1, rug = T, 
         xlab = "Sleep score (PC1)")

shs_df$pc1_sq <- (shs_df$pc1_scaled)^2

pc1_model_1_squared <- coxph(Surv(dthtt-slpexam1, death) ~ pc1_scaled + pc1_sq  + age5c + female + race + 
                              modvig_pa + smoke + income_tertile + 
                              degree_attain + married + aehi_10, data = shs_df)

pc1_model_1_squared
termplot(shs_model_1_spline, term=1, se=TRUE, col.term=1, col.se=1, rug = T, xlab = "Sleep score (summary)")

shs_model_1 <- coxph(Surv(dthtt-slpexam1, death) ~ pspline(shs, df = 3) + age5c + female + race + 
                       modvig_pa + smoke + income_tertile + 
                       degree_attain + married + aehi_10, data = shs_df)

shs_model_1
termplot(shs_model_1, term=1, se=TRUE, col.term=1, col.se=1, rug = T, xlab="Sleep score (summary; raw)")

## PC Scores

# PC score regression


table(sensitivity_df$death)
shs_df$death_lag <- shs_df$dthtt/365 - shs_df$slpexam1/365

sensitivity_df <- shs_df %>% dplyr::mutate(death_excl = ifelse(death_lag < 1 & death == 1, 1, 0)) %>%
  mutate(death_1 = ifelse(death_excl==0 & death == 1, 1, 0))

table(sensitivity_df$death) 

table(sensitivity_df$death_1) # Recoded 18 deaths as alive w/in 1 year of sleep exam
# 0     1 
# 1573  153 ; n=1,726, deaths=153

table(shs_df$death) # Total sample
