## Code HTN and DM

## Adjust for HTN and DM

# incident hypertension defined as systolic blood pressure ≥130 mm Hg, 
#  diastolic blood pressure ≥80 mm,
#  or the use of antihypertensive medication

table(mesa_5$htn5c, mesa_5$htnmed5c)

mesa_5$htn_5 <- ifelse(mesa_5$htn5c == "1: YES" & mesa_5$htnmed5c == "1: YES",
                       1, 0)

table(mesa_5$htn_5)

#  or fasting glucose ≥ 126 mg/dL, or taking insulin or taking 
#  oral hypoglycemic medications as previously used in MESA.

table(mesa_5$dm035c, mesa_5$diabins5)

mesa_5$no_dm_5 <- ifelse(mesa_5$dm035c == "NORMAL" & mesa_5$diabins5 == "0: NO",
                      1, 0)

table(mesa_5$no_dm_5, mesa_5$dm035c)
table(mesa_5$no_dm_5, mesa_5$diabins5)

htn_dm_df <- mesa_5 %>% dplyr::select(htn_5, no_dm_5, idno)

# mesa_sens_1 <- na.omit(merge(shs_df, htn_dm_df, by = "idno"))
# dim(na.omit(mesa_sens_1))
# 
# mesa_sens_1 <- merge(mesa_sens_1, totmed_df, by = "idno")
# mesa_sens_1 <- na.omit(mesa_sens_1)

dim(mesa_sens_1)
table(mesa_sens_1$death)

base_no_htn_dm <- coxph(Surv(time_yr, death) ~ 
                      age5c + female + race + 
                      income_tertile + 
                      degree_attain + married + 
                         totmed5 + pc1_scaled,  
                    data = mesa_sens_1,
                    x = TRUE)

ShowRegTable(base_no_htn_dm)


base_no_htn_dm1 <- coxph(Surv(time_yr, death) ~ 
                          age5c + female + race + 
                          modvig_pa + smoke + income_tertile + 
                          degree_attain + married + aehi_10 + totmed5 +  pc1_scaled,  
                        data = mesa_sens_1,
                        x = TRUE)

ShowRegTable(base_no_htn_dm1)

base_no_htn_dm2 <- coxph(Surv(time_yr, death) ~ 
                          age5c + female + race + 
                          modvig_pa + smoke + income_tertile + 
                          degree_attain + married + aehi_10 + htn_5+ 
                           no_dm_5 + pc1_scaled,  
                        data = mesa_sens_1,
                        x = TRUE)

ShowRegTable(base_no_htn_dm2)

base_no_htn_dm3 <- coxph(Surv(time_yr, death) ~ 
                          age5c + female + race + 
                          modvig_pa + smoke + income_tertile + 
                          degree_attain + married + aehi_10 + 
                          htn_5 + no_dm_5 + pc1_scaled,  
                        data = mesa_sens_1,
                        x = TRUE)

ShowRegTable(base_no_htn_dm3)

base_no_htn_dm4 <- coxph(Surv(time_yr, death) ~ 
                           age5c + female + race + 
                           modvig_pa + smoke + income_tertile + 
                           degree_attain + married + aehi_10 + 
                           htn_5 + no_dm_5 + emphys_f +  pc1_scaled,  
                         data = mesa_sens_1,
                         x = TRUE)

ShowRegTable(base_no_htn_dm4)
##############################################
mesa_sens_2 <- na.omit(merge(mesa_sens_1, emphys_df, by = "idno"))
table(mesa_sens_2$emphys_f.x)

