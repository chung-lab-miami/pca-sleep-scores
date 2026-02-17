## mesa mortality: table one and Cox model result tables
library(tableone)

shs_df <- shs_df %>% 
  dplyr::mutate(pc1_scaled = as.numeric(pc1_scaled)) %>%
  dplyr::mutate(quality_f_temp = factor(quality, labels = c("Very sound and restful",
                                                       "Sound and restful",
                                                       "Average quality",
                                                       "Restless",
                                                       "Very restless")),
                dis_f_temp = factor(sol_sub, labels = c("No, not in the past 4 weeks",
                                                   "Yes, less than once a week",
                                                   "Yes, 1-2 times a week",
                                                   "Yes, 3-4 times a week",
                                                   "Yes, 5 or more times a week")),
                death_years_slpexam = (dthtt/365 - slpexam1/365)) %>%
  dplyr::mutate(quality_f = case_when(quality_f_temp == "Very sound and restful" |
                                      quality_f_temp == "Sound and restful" ~ 
                                        "Very sound and restful or Sound and restful",
                                      quality_f_temp == "Restless" |
                                        quality_f_temp == "Average quality" |
                                        quality_f_temp == "Very restless" ~ "Average -  Very restless")) %>%
  
  dplyr::mutate(dis_f = case_when(dis_f_temp == "No, not in the past 4 weeks"|
                                    dis_f_temp == "Yes, less than once a week" ~ " Less than 1/week",
                                  dis_f_temp == "Yes, 1-2 times a week" | 
                                    dis_f_temp == "Yes, 3-4 times a week" |
                                    dis_f_temp == "Yes, 5 or more times a week" ~ "1/week or more")) %>%
  dplyr::mutate(shs_scaled = as.numeric(scale(shs)))
                                  
                                  

# skewed vars IQR 
skewed_vars <- c("mpsd", "sdtst", "frag",
                 "waso", "sol", "sws_per", "rem_per",
                 "ahi", "death_years_slpexam")

              # Outcome
soc_vars <- c("death", "death_years_slpexam",
              
              # Socio-demographics
              "age5c", "female", "race", "degree_attain", "income_tertile", 
              
              # Lifestyle
              "smoke", "aehi_10", "modvig_pa", 
              
              # Health status indicators
              "bmi", "prev_cvd", "copd_both", "cancer_any", 
              
              # Sleep variables: composites
              "shs", "pc1_scaled", "shs_scaled", "pc1_di",
              
              # Sleep variables: individual components by actigraphy
              "tst_hr", "mpsd", "sdtst", "frag", "sme", "waso", "sol", "log_timing",
              
              # Sleep variables: individual components by PSG
              "sws_per", "rem_per", "ahi",
              
              # Sleep variables: self-report
              "quality_f", "dis_f", "ess")

cat_vars <- c("female", "race", "degree_attain", "income_tertile",
              "prev_cvd", "copd_both", "cancer_any", "death",
              "quality_f", "dis_f")           

mesa_mortality_tableone_overall <- CreateTableOne(vars = soc_vars, data = shs_df, factorVars = cat_vars)
mesa_mortality_tableone_stratified <- CreateTableOne(vars = soc_vars, 
                                                     data = shs_df, 
                                                     factorVars = cat_vars,
                                                     strata = "death")

mesa_mortality_tableone_stratified_sleep <- CreateTableOne(vars = soc_vars, 
                                                     data = shs_df, 
                                                     factorVars = cat_vars,
                                                     strata = "pc1_di")

tableone_overall <- print(mesa_mortality_tableone_overall, nonnormal = skewed_vars, showAllLevels = TRUE)
tableone_stratified <- print(mesa_mortality_tableone_stratified, nonnormal = skewed_vars, showAllLevels = TRUE)
tableone_stratified_sleep <- print(mesa_mortality_tableone_stratified_sleep, nonnormal = skewed_vars, showAllLevels = TRUE)

write.csv(tableone_overall, file = "mesa_mortality_tableone_overall.csv")
write.csv(tableone_stratified, file = "mesa_mortality_tableone_stratified.csv")
write.csv(tableone_stratified_sleep, file = "mesa_mortality_tableone_stratified_sleep.csv")

## 


