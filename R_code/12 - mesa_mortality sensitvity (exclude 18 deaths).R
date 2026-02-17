## Sensitivity analyses: Exclude deaths within 1 year of exam

table(sensitivity_df$death)
shs_df$death_lag <- shs_df$dthtt/365 - shs_df$slpexam1/365

sensitivity_df <- shs_df %>% dplyr::mutate(death_excl = ifelse(death_lag < 1 & death == 1, 1, 0)) %>%
  mutate(death_1 = ifelse(death_excl==0 & death == 1, 1, 0)) %>%
  filter(death_lag > 1)


## Sleep Health Score regressions
shs_model_0_sens <- coxph(Surv(dthtt-slpexam1, death_1) ~ shs_scaled + age5c + female + race + 
                            income_tertile + 
                            degree_attain + married, data = sensitivity_df)

summary(shs_model_0_sens)

# # PC score regression
# pc1_model_0_sens <- coxph(Surv(dthtt-slpexam1 +365, death) ~ pc1_scaled + age5c + female + race + 
#                             income_tertile + 
#                             degree_attain + married, data = sensitivity_df)
# 
# summary(pc1_model_0_sens)

# ## Sleep Cluster regression
# cluster_model_0 <- coxph(Surv(dthtt-slpexam1, death) ~ cluster_f + age5c + female + race + 
#                            income_tertile + 
#                            degree_attain + married, data = shs_df)
# summary(cluster_model_0)


## Sleep Health Score regressions
shs_model_1_sens <- coxph(Surv(dthtt-slpexam1, death) ~ shs_scaled + age5c + female + race + 
                       modvig_pa + smoke + income_tertile + 
                       degree_attain + married + aehi_10, data = sensitivity_df)

summary(shs_model_1_sens)

# # PC score regression
# pc1_model_1_sens <- coxph(Surv(dthtt-slpexam1, death) ~ pc1_scaled + age5c + female + race + 
#                        modvig_pa + smoke + income_tertile + 
#                        degree_attain + married + aehi_10, data = sensitivity_df)
# 
# summary(pc1_model_1_sens)

# ## Sleep Cluster regression
# cluster_model_1 <- coxph(Surv(dthtt-slpexam1, death) ~ cluster_f + age5c + female + race + 
#                        modvig_pa + smoke + income_tertile + 
#                        degree_attain + married + aehi_10, data = shs_df)
# summary(cluster_model_1)

## Sleep Health Score regressions BMI + prevalent disease adjusted
shs_model_2_sens <- coxph(Surv(dthtt-slpexam1, death) ~ shs_scaled + age5c + female + race + 
                       modvig_pa + smoke + income_tertile + 
                       degree_attain + married + aehi_10 + bmi + prev_cvd + prev_copd + prev_cancer, data = sensitivity_df)

summary(shs_model_2_sens)


# pc1_model_2_sens <- coxph(Surv(dthtt-slpexam1, death) ~ pc1_scaled + age5c + female + race + 
#                        modvig_pa + smoke + income_tertile + 
#                        degree_attain + married + aehi_10 + bmi + prev_cvd + prev_copd + prev_cancer, data = sensitivity_df)
# 
# summary(pc1_model_2_sens)


## Dichotomous PC1 and SHS for KM curves

## Sleep Health Score regressions
shs_di_model_0_sens <- coxph(Surv(dthtt-slpexam1, death) ~ shs_di + age5c + female + race + 
                          income_tertile + 
                          degree_attain + married, data = sensitivity_df)

summary(shs_di_model_0_sens)

# # PC score regression
# pc1_di_model_0_sens <- coxph(Surv(dthtt-slpexam1, death) ~ pc1_di + age5c + female + race + 
#                           income_tertile + 
#                           degree_attain + married, data = sensitivity_df)
# 
# summary(pc1_di_model_0_sens)

## Sleep Health Score regressions
shs_di_model_1_sens <- coxph(Surv(dthtt-slpexam1, death) ~ shs_di + age5c + female + race + 
                            modvig_pa + smoke + income_tertile + 
                            degree_attain + married + aehi_10, data = sensitivity_df)

summary(shs_di_model_1_sens)

# # PC score regression
# pc1_di_model_1_sens <- coxph(Surv(dthtt-slpexam1, death) ~ pc1_di + age5c + female + race + 
#                             modvig_pa + smoke + income_tertile + 
#                             degree_attain + married + aehi_10, data = sensitivity_df)
# 
# summary(pc1_di_model_1_sens)

## Sleep Health Score regressions BMI + prevalent disease adjusted
shs_di_model_2_sens <- coxph(Surv(dthtt-slpexam1, death) ~ shs_di + age5c + female + race + 
                            modvig_pa + smoke + income_tertile + 
                            degree_attain + married + aehi_10 + bmi + prev_cvd + prev_copd + prev_cancer, data = sensitivity_df)

summary(shs_di_model_2_sens)


# pc1_di_model_2_sens <- coxph(Surv(dthtt-slpexam1, death) ~ pc1_di + age5c + female + race + 
#                             modvig_pa + smoke + income_tertile + 
#                             degree_attain + married + aehi_10 + bmi + prev_cvd + prev_copd + prev_cancer, data = sensitivity_df)
# 
# summary(pc1_di_model_2_sens)

## ShowRegTable
library(tableone)
ShowRegTable(shs_model_0_sens)[1,][1] # Sociodem - SHS
# ShowRegTable(pc1_model_0_sens)[1,][1] # Sociodem - PC1

ShowRegTable(shs_model_1_sens)[1,][1] # + lifestyle - SHS
# ShowRegTable(pc1_model_1_sens)[1,][1] # + lifestyle - PC1

ShowRegTable(shs_model_2_sens)[1,][1] # + morbidity - SHS
# ShowRegTable(pc1_model_2_sens)[1,][1] # + morbidity - PC1

ShowRegTable(shs_di_model_0_sens)[1,][1] # Sociodem - SHS_di
# ShowRegTable(pc1_di_model_0_sens)[1,][1] # Sociodem - PC1_di

ShowRegTable(shs_di_model_1_sens)[1,][1] # + lifestyle - SHS_di
# ShowRegTable(pc1_di_model_1_sens)[1,][1] # + lifestyle - PC1_di

ShowRegTable(shs_di_model_2_sens)[1,][1] # + morbidity - SHS_di
# ShowRegTable(pc1_di_model_2_sens)[1,][1] # + morbidity - PC1_di
