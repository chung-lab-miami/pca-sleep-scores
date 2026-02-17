## Joon Chung
## Predictions

library(survival)
library(survminer)
library(tidyverse)
library(risksetROC)

## Concatenate all sleep variables
sleep_vars_di <- c("tst_di", "mpsd_di", "sdtst_di",
                   "sme_di", "frag_di", "waso_di",
                   "ahi_di", "swsper_di", "remper_di",
                   "quality_di", "sol_di", "ess_di",
                   "timing_di")

sleep_vars <- c("tst", "log_mpsd", "sdtst",
                "sme", "frag", "waso",
                "ahi", "sws_per", "rem_per",
                "quality", "sol", "ess",
                "log_timing", "pc1_scaled", "reduced_pc1",
                "reduced_pc1_nompsd"
                )

sleep_vars_all <- c(sleep_vars_di, sleep_vars)


## Survival times, vital status
survival.time <- shs_df$time
survival.status <- shs_df$death

# U times
utimes <- unique( survival.time[ survival.status == 1 ] )
utimes <- utimes[ order(utimes) ]

# Estimated survival probabilities at unique failure times
surv.prob <- unique(survfit(Surv(survival.time,survival.status)~1)$surv)

# Empty list for results
auc_list <- list()
auc_df <- data.frame(iAUC = NA, sleep_var = NA, iteration = seq(1,29, by=1))

## Loop 
for(i in 1:length(sleep_vars_all)){
  
  adjusted_sleep_formula_di <- paste0("Surv(time, death) ~ ", 
                                      sleep_vars_all[i], 
                                      " + age5c + female + race + bmi")
  
  fit0 <- coxph(as.formula(adjusted_sleep_formula_di), data = shs_df)
  eta <- fit0$linear.predictor
  model.score <- eta

  
  ## find AUC at unique failure times
  AUC <- rep( NA, length(utimes) )
  for( j in 1:length(utimes) ) {
    out <- CoxWeights( eta, survival.time, survival.status,utimes[j])
    AUC[j] <- out$AUC
  }
  
  ## integrated AUC to get concordance measure
  iAUC <- IntegrateAUC( AUC, utimes, surv.prob, tmax=365 )
  

  auc_df$iAUC[i] <- round(iAUC, 3)
  auc_df$sleep_var[i] <- sleep_vars_all[i]

  
  print(paste0("iteration ", i))
  
}


auc_df_sorted <- auc_df %>% arrange(desc(iAUC))
# auc_di_df <- auc_df[1:13,] %>% arrange(desc(iAUC))
# auc_cont_df <- auc_df[14:28, ]%>% arrange(desc(iAUC))

# 
# ## Write to file
# write.csv(auc_df, file = "auc_df.csv")
# write.csv(auc_di_df, file = "auc_di_df.csv")
# write.csv(auc_cont_df, file = "auc_cont_df.csv")