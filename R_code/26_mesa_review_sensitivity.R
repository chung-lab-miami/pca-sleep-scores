## MESA sensitivity analyses

## Medications (sleep altering, total)
## Depressive symptoms
## Health insurance
## Pack-years, time since quitting
## Second hand smoke

library(tidyverse)
library(survival)
library(survminer)
library(mosaic)


qplot(mesa_df$cesd_nosleep) + xlab("CESD scores, no sleep") 
qplot(mesa_df$totmed5) + xlab("Total medication use") 

qplot(mesa_df$pkyrs5c) + xlab("Pack years") 
qplot(mesa_df$avcinh5) + xlab("Smoke inhale") 

cesd_totmed_pkyrs_df <- mesa_df %>% 
  dplyr::select(cesd_nosleep, totmed5, pkyrs5c, idno)

## Exam 4
qplot(mesa_4$shndsmk4) + xlab("Second-hand smoke, hrs/wk")

secondhand_df <- mesa_4 %>% 
  dplyr::select(shndsmk4, idno)

## Exam 3
qplot(mesa_3$hinone3) + xlab("Insurance (Exam 3)")
noinsurance_df <- mesa_3 %>%
  dplyr::select(hinone3, idno)

cesd_totmed_pkyrs_df
secondhand_df
noinsurance_df

exam5_to_merge <- merge(cesd_totmed_pkyrs_df, secondhand_df, by = "idno")
exam4_to_merge <- merge(exam5_to_merge, noinsurance_df, by = "idno")

names(exam4_to_merge)

final_df <- na.omit(merge(shs_df, exam4_to_merge, by = "idno"))
dim(na.omit(final_df))

## Descriptive statistics
table(final_df$death)


sens_model_1 <- coxph(Surv(dthtt-slpexam1, death) ~ pc1_scaled + age5c + female + race + 
                       modvig_pa + smoke + income_tertile + 
                       degree_attain + married + aehi_10, data = final_df)

summary(sens_model_1)
ShowRegTable(sens_model_1)

sens_model_2 <- coxph(Surv(dthtt-slpexam1, death) ~ pc1_scaled + age5c + female + race + 
                        modvig_pa + smoke + income_tertile + 
                        degree_attain + married + aehi_10 + bmi + prev_cvd , data = final_df)

summary(sens_model_2)
ShowRegTable(sens_model_2)

sens_model_3 <- coxph(Surv(dthtt-slpexam1, death) ~ pc1_scaled + age5c + female + race + 
                        modvig_pa + smoke + income_tertile + 
                        degree_attain + married + aehi_10 + bmi + prev_cvd +
                        cesd_nosleep, data = final_df)

summary(sens_model_3)
ShowRegTable(sens_model_3)

sens_model_4 <- coxph(Surv(dthtt-slpexam1, death) ~ pc1_scaled + age5c + female + race + 
                        modvig_pa + smoke + income_tertile + 
                        degree_attain + aehi_10 + bmi + prev_cvd +
                        cesd_nosleep + totmed5 + pkyrs5c + shndsmk4 + 
                        hinone3, data = final_df)

summary(sens_model_4)
ShowRegTable(sens_model_4)


