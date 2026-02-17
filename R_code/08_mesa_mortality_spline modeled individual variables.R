# Kitchen sink model

## names needed:
## 1) variable name within pspline(tst_hr, df = 3)
## 2) model name eg. tst_model_splines
## 3) Xlab name eg Total sleep time and mortality
## 4) Ylab name eg Partial for spline-modeled Total sleep time (units), df = 3


## 1) variable name within pspline(tst_hr, df = 3)
sleep_vars <- c("tst", "log_mpsd", "sdtst", "sme", "frag", "waso", "ahi", "sws_per",
                "rem_per", "quality", "sol", "ess", "log_timing")
# [1] "tst_hr"     "log_mpsd"   "sdtst"      "sme"        "frag"       "waso"       "ahi"        "sws_per"    "rem_per"   
# [10] "quality"    "sol"        "ess"        "log_timing"

## 2) model name eg. tst_model_splines (run for loop)
sleep_vars_model_names <- c()
for(i in 1:length(sleep_vars)){
sleep_vars_model_names[i] <- paste0(sleep_vars[i], "_model_splines") 
}

sleep_vars_model_names

## 3) Xlab name eg Total sleep time and mortality
# Generate full names of sleep variables
sleep_names <- c("Total sleep time (min)", "Midpoint irregularity (sd, minutes; log)", "Total sleep time irregularity (sd, minutes)",
                 "Sleep maintenance efficiency (%)", "Fragmentation Index", "Wake after sleep onset (min)", 
                 "The Apnea-Hypopnea Index (events/hr)", "% N3 (slow wave) sleep", "% R (rapid eye movement) sleep",
                 "Quality", "Onset latency (min)", "Epworth Sleepiness Scale", "Timing (log)")

sleep_vars_xlab <- c()

for(i in 1:length(sleep_names)){
  sleep_vars_xlab[i] <- paste0(sleep_names[i], " and log Hazard")
}

## 4) Ylab name eg Partial for spline-modeled Total sleep time (units), df = 3
sleep_vars_ylab <- c()
for(i in 1:length(sleep_names)){
  
  sleep_vars_ylab[i] <- paste0("Partial for spline-modeled ", sleep_names[i]) 
  
}

## Code general loop for spline models:
#  model_res_list: listed results of coxph regressions
temp_results <- c()
model_res_list <- list()

for(i in 1:length(sleep_names)){

  adjusted_sleep_formula_scaled <- paste0("Surv(dthtt-slpexam1, death) ~ pspline(", 
                                          sleep_vars[i], 
                                          ", df = 5) + age5c + female + race + 
                                          modvig_pa + smoke + income_tertile + 
                                          degree_attain + married + aehi_10")
  
  
  temp_results <- coxph(formula = as.formula(adjusted_sleep_formula_scaled),
                        data    = shs_df , x = T, y=T)


  ragg::agg_tiff(paste0(sleep_vars[i], ".tiff"), width = 5, height = 5, units = "in", res = 300)
  termplot(temp_results, term=1, se=TRUE, col.term=1, col.se=1, rug = T, xlab= sleep_vars_xlab[i],
           ylab = sleep_vars_ylab[i])
  dev.off()
  model_res_list[[i]] <- temp_results
  print(model_res_list[[i]])
}

# End script
