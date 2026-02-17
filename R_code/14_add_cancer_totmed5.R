cancer_df <- mesa_1 %>% dplyr::select(cancer1, idno)

# shs_df <- shs_df %>% dplyr::select(-cancer1.x, -cancer1.y)

sens_df <- merge(shs_df, cancer_df, by = "idno")

totmed_df <- mesa_df %>% dplyr::select(totmed5, idno)
sens_df <- na.omit(merge(sens_df, totmed_df, by = "idno"))

dim(sens_df)

## Sleep Health Score regressions BMI + prevalent disease adjusted
shs_model_2 <- coxph(Surv(dthtt-slpexam1, death) ~ pc1_scaled + age5c + female + race + 
                       modvig_pa + smoke + income_tertile + totmed5+ 
                       degree_attain + married + aehi_10 + bmi + prev_cvd +prev_cancer, data = sens_df)

summary(shs_model_2)
ShowRegTable(shs_model_2)


di_model_2 <- coxph(Surv(dthtt-slpexam1, death) ~ pc1_scaled + age5c + female + race + 
                      modvig_pa + smoke + income_tertile + totmed5 +
                      degree_attain + married + aehi_10 + bmi + prev_cvd + emphys_f + prev_cancer, data = sens_df)

summary(di_model_2)
ShowRegTable(di_model_2)

## Sensitivity: Exclude CVD, CANCER, EMPHYS_F, prev_COPD
table(shs_df$emphys_f, shs_df$prev_copd)
table(sens_df$cancer1, sens_df$prev_cancer)

sens2_df <- sens_df %>% dplyr::filter(prev_cvd == 0,
                                      prev_cancer == 0,
                                      prev_copd == 0,
                                      emphys_f == 0,
                                      cancer1 == "0: NO") %>%
  na.omit()

## Sleep Health Score regressions BMI + prevalent disease adjusted
shs_sens_1 <- coxph(Surv(dthtt-slpexam1, death) ~ pc1_scaled + age5c + female + race + 
                       income_tertile + totmed5+ 
                       degree_attain + married, 
                     data = sens2_df)

summary(shs_sens_1)
ShowRegTable(shs_sens_1) # 

shs_sens_2 <- coxph(Surv(dthtt-slpexam1, death) ~ pc1_scaled + age5c + female + race + 
                       modvig_pa + smoke + income_tertile + totmed5+ 
                       degree_attain + married + aehi_10 , 
                     data = sens2_df)

summary(shs_sens_2)
ShowRegTable(shs_sens_2) # 

shs_sens_3 <- coxph(Surv(dthtt-slpexam1, death) ~ pc1_scaled + age5c + female + race + 
                      modvig_pa + smoke + income_tertile + totmed5+ 
                      degree_attain + married + aehi_10 + bmi, 
                    data = sens2_df)

summary(shs_sens_3)
ShowRegTable(shs_sens_3) # 
