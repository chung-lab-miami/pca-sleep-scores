## Sleep health score, dichotomous at Q3
favstats(shs_df$shs_scaled)$Q3
shs_df$shs_scaled_di <- ifelse(shs_df$shs_scaled > favstats(shs_df$shs_scaled)$Q3, 1, 0)
shs_df$shs_raw_di <- ifelse(shs_df$shs <7, 1, 0)

shs_model_1 <- coxph(Surv(dthtt-slpexam1, death) ~ shs_raw_di + age5c + female + race + 
                          modvig_pa + smoke + income_tertile + 
                          degree_attain + married + aehi_10, data = shs_df,
                     x = TRUE)

summary(shs_model_1)
ShowRegTable(shs_model_1)

## Appendix D - domain scores (psg, actigraphy, self-report)

## Actigraphy domain score:
shs_df$actigraphy_score <- scale(with(shs_df, sdtst_di + mpsd_di + tst_di + 
                                  sme_di + timing_di + frag_di))

shs_act_model_2 <- coxph(Surv(dthtt-slpexam1, death) ~ actigraphy_score + age5c + female + race + 
                          modvig_pa + smoke + income_tertile + 
                          degree_attain + married + aehi_10, data = shs_df)

summary(shs_act_model_2)
ShowRegTable(shs_act_model_2)

## Self-report domain score:
shs_df$self_reported_score <- scale(with(shs_df, sol_di + quality_di + ess_di))

shs_sr_model_2 <- coxph(Surv(dthtt-slpexam1, death) ~ self_reported_score + age5c + female + race + 
                           modvig_pa + smoke + income_tertile + 
                           degree_attain + married + aehi_10, data = shs_df)

summary(shs_sr_model_2)
ShowRegTable(shs_sr_model_2)

## Polysomnography domain score:
shs_df$psg_score <- scale(with(shs_df, remper_di + swsper_di + ahi_di + waso_di))
  
shs_psg_model_2 <- coxph(Surv(dthtt-slpexam1, death) ~ psg_score + age5c + female + race + 
                           modvig_pa + smoke + income_tertile + 
                           degree_attain + married + aehi_10, data = shs_df)

summary(shs_psg_model_2)
ShowRegTable(shs_psg_model_2)

## Operationalize Ru SATED

shs_df$ru_sated <- as.numeric(scale(with(shs_df, sdtst_di + tst_di + quality_di + frag_di + timing_di + ess_di)))

rusated_model_1 <- coxph(Surv(dthtt-slpexam1, death) ~ ru_sated + age5c + female + race + 
                          income_tertile + 
                           degree_attain + married , data = shs_df)

summary(rusated_model_1)

rusated_model_2 <- coxph(Surv(dthtt-slpexam1, death) ~ ru_sated + age5c + female + race + 
                               modvig_pa + smoke + income_tertile + 
                               degree_attain + married + aehi_10, data = shs_df)

summary(rusated_model_2)


rusated_model_3 <- coxph(Surv(dthtt-slpexam1, death) ~ ru_sated + age5c + female + race + 
                           modvig_pa + smoke + income_tertile + 
                           degree_attain + married + aehi_10 + bmi + prev_cancer + prev_copd + prev_cvd, data = shs_df)

summary(rusated_model_3)
# 
# shs_df$rusated_di <- ifelse(shs_df$ru_sated > 1.08, 1, 0)
# 
# rusated_di_model_1 <- coxph(Surv(dthtt-slpexam1, death) ~ rusated_di + age5c + female + race + 
#                               income_tertile + 
#                               degree_attain + married, data = shs_df)
# 
# summary(rusated_di_model_1)
# 
# rusated_di_model_2 <- coxph(Surv(dthtt-slpexam1, death) ~ rusated_di + age5c + female + race + 
#                            modvig_pa + smoke + income_tertile + 
#                            degree_attain + married + aehi_10, data = shs_df)
# 
# summary(rusated_di_model_2)
# 
# rusated_di_model_3 <- coxph(Surv(dthtt-slpexam1, death) ~ rusated_di + age5c + female + race + 
#                               modvig_pa + smoke + income_tertile + 
#                               degree_attain + married + aehi_10 + prev_cvd + prev_cancer + prev_copd, data = shs_df)
# 
# summary(rusated_di_model_3)
# 
# 
# shs_di_model_1 <- coxph(Surv(dthtt-slpexam1, death) ~ shs_di + age5c + female + race + 
#                           income_tertile + 
#                           degree_attain + married, data = shs_df)
# 
# summary(shs_di_model_1)
# 
# shs_di_model_2 <- coxph(Surv(dthtt-slpexam1, death) ~ shs_di + age5c + female + race + 
#                           modvig_pa + smoke + income_tertile + 
#                           degree_attain + married + aehi_10, data = shs_df)
# 
# summary(shs_di_model_2)
# 
# shs_di_model_3 <- coxph(Surv(dthtt-slpexam1, death) ~ shs_di + age5c + female + race + 
#                           modvig_pa + smoke + income_tertile + 
#                           degree_attain + married + aehi_10 + prev_cancer + 
#                           prev_copd + prev_cvd + bmi, data = shs_df)
# 
# summary(shs_di_model_3)

## Show Reg Table
library(tableone)

# ShowRegTable(shs_di_model_1)
# ShowRegTable(shs_di_model_2) # 0.58 [0.37, 0.90]
# ShowRegTable(shs_di_model_3)

ShowRegTable(shs_model_0)
ShowRegTable(shs_model_1)
ShowRegTable(shs_model_2)


ShowRegTable(rusated_model_1)
ShowRegTable(rusated_model_2)
ShowRegTable(rusated_model_3)

ShowRegTable(rusated_di_model_1)
ShowRegTable(rusated_di_model_2)
ShowRegTable(rusated_di_model_3)

## 
shsfit <- survfit(Surv((dthtt/365-slpexam1/365), death) ~ shs_di, data = shs_df)
shsfit

shs_survplot <- ggsurvplot(shsfit, data = shs_df,
                           legend.title = "Sleep score dichotomous",
                           legend.labs = c("Lowest 3 quartiles", 
                                           "Highest quartile"),
                           xlab = "Years",
                           ylim = c(0.8, 1),
                           conf.int = T,
                           ggtheme = theme_light(base_size= 25))

shs_survplot

ggpubr::ggexport(shs_survplot, 
                 filename = "shs_survplot.png",
                 width = 1000,
                 height = 1000,
                 pointsize = 24) 

rusatedfit <- survfit(Surv((dthtt/365-slpexam1/365), death) ~ rusated_di, data = shs_df)
rusatedfit

rusated_survplot <- ggsurvplot(rusatedfit, data = shs_df,
                           legend.title = "Sleep score dichotomous",
                           legend.labs = c("Lowest 3 quartiles", 
                                           "Highest quartile"),
                           xlab = "Years",
                           ylim = c(0.8, 1),
                           conf.int = T,
                           ggtheme = theme_light(base_size= 25))

rusated_survplot

ggpubr::ggexport(rusated_survplot, 
                 filename = "rusated_survplot.png",
                 width = 1000,
                 height = 1000,
                 pointsize = 24) 
