library(survMisc)

shs_df$time_yr = with(shs_df, dthtt-slpexam1)

pc_mod_0 <- coxph(Surv(time_yr, death) ~ pc1_scaled,  
                  data = shs_df,
                  x = TRUE)



pc_noage_model <- coxph(Surv(time_yr, death) ~ pc1_scaled + female + race + bmi,  
                        data = shs_df,
                        x = TRUE)


base_mod_0 <- coxph(Surv(time_yr, death) ~ 
                      
                      age5c + female + race + 
                      modvig_pa + smoke + income_tertile + 
                      degree_attain + married + aehi_10 + bmi,  
                    data = shs_df,
                    x = TRUE)

base_mod_1 <- coxph(Surv(time_yr, death) ~ 
                      age5c + female + race + 
                      modvig_pa + smoke + income_tertile + 
                      degree_attain + married + aehi_10 + bmi + pc1_scaled,  
                    data = shs_df,
                    x = TRUE)

anova(base_mod_0, base_mod_1, test="LRT")

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
shs_model_2 <- coxph(Surv(dthtt-slpexam1, death) ~  age5c + female + race + 
                       modvig_pa + smoke + income_tertile + 
                       degree_attain + married + aehi_10 , data = shs_df)

summary(shs_model_2)
ShowRegTable(shs_model_2)

anova(shs_model_2, shs_model_1, test="LRT")


rsq(pc_noage_model)
rsq(base_mod_0)
rsq(base_mod_1)



pc_r2 <- rsq(pc_mod_1)
shs_r2 <- rsq(shs_mod_1)
tst_r2 <- rsq(tst_mod_1)

sme_r2 <- rsq(sme_mod_1)
frag_r2 <- rsq(frag_mod_1)
waso_r2 <- rsq(waso_mod_1)

ahi_r2 <- rsq(ahi_mod_1)
sws_r2 <- rsq(n3_mod_1)
rem_r2 <- rsq(rem_mod_1)

mpsd_r2 <- rsq(mpsd_mod_1)
sdtst_r2 <- rsq(sdtst_mod_1)
quality_r2 <- rsq(quality_mod_1)

ess_r2 <- rsq(ess_mod_1)
sol_r2 <- rsq(sol_mod_1)
timing_r2 <- rsq(timing_mod_1)

## dichotomous regs
ahi_di_r2 <- rsq(ahi_mod_1_di)
sws_di_r2 <- rsq(n3_mod_1_di)
rem_di_r2 <- rsq(rem_mod_1_di)
mpsd_di_r2 <- rsq(mpsd_mod_1_di)
sdtst_di_r2 <- rsq(sdtst_mod_1_di)
ess_di_r2 <- rsq(ess_mod_1_di)
waso_di_r2 <- rsq(waso_mod_1_di)
quality_di_r2 <- rsq(quality_mod_1_di)

mev_df <- as.data.frame(t(cbind(pc_r2, shs_r2, tst_r2,
                                sme_r2, frag_r2, waso_r2,
                                ahi_r2, sws_r2, rem_r2,
                                mpsd_r2, sdtst_r2, quality_r2,
                                ess_r2, timing_r2,sol_r2
                                # ahi_di_r2, sws_di_r2,
                                # rem_di_r2, mpsd_di_r2, sdtst_di_r2, ess_di_r2,
                                # waso_di_r2, quality_di_r2
)))
mev_df$mev <- as.numeric(mev_df$mev)     
mev_df$mer <- as.numeric(mev_df$mer)     
mev_df$cod <- as.numeric(mev_df$cod)     

mesa_mortality_r2 <- mev_df %>% dplyr::arrange(desc(mev))              
