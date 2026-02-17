## MESA 

## Cox PH
library(survival)
library(survminer)

sleep_vars_di <- c("tst_di", "mpsd_di", "sdtst_di",
                   "sme_di", "frag_di", "waso_di",
                   "ahi_di", "swsper_di", "remper_di",
                   "quality_di", "sol_di", "ess_di",
                   "timing_di")



primary_report_table_di <- c()
primary_report_model_list <- list()

for(i in 1:length(sleep_vars_di)){
  
  adjusted_sleep_formula_di <- paste0("Surv(dthtt-slpexam1, death) ~ ", 
                                      sleep_vars_di[i], 
                                      " + age5c + female + race + 
                       modvig_pa + smoke + income_tertile + 
                       degree_attain + married + aehi_10")
  
  
  ## UNADJUSTED REGRESSIONS
  
  death_mod_di <- coxph(formula = as.formula(adjusted_sleep_formula_di),
                        data    = shs_df , x = T, y=T)
  
  summary(death_mod_di)
  primary_report_model_list[[i]] <- death_mod_di
  
  # For coefficients
  HR_coefs <- exp(death_mod_di$coefficients)
  
  # For confidence interval
  cc <- as.data.frame(coef(summary(death_mod_di)))
  names(cc) <- c("coef", "hazard_ratio", "se_coef", "z", "p")
  
  cc$LB <- summary(death_mod_di)$conf.int[1,3]
  cc$UB <- summary(death_mod_di)$conf.int[1,4]
  cc$var <- sleep_vars_di[i]
  
  print(i)
  
  # For p-values
  # pvals <- coef(summary(death_mod_di))[, 5]
  # stars <- c()
  # for(i in 1:length(pvals)){
  #   if(pvals[i] > 0.05) stars[i] <- ""
  #   if(pvals[i] < 0.05 & pvals[i] >=0.01) stars[i] <- "*"
  #   if(pvals[i] <= 0.01 & pvals[i] >=0.001) stars[i] <- "**" 
  #   if(pvals[i] < 0.001) stars[i] <- "***"
  # }
  # 
  # Unite coefs with ci_vector, ci_vector on new line
  
  
  primary_report_table_di <- rbind(primary_report_table_di, cc[1,])
  
  # write.csv(report_df, file = paste0(sleep_vars[j], "-RR.csv"))
  
}

primary_report_table_di
primary_report_model_list

## For standardized variables (not dichotomous)

sleep_vars <- c("tst_hr", "log_mpsd", "sdtst",
                "sme", "frag", "waso",
                "ahi", "sws_per", "rem_per",
                "quality", "sol", "ess",
                "log_timing", "shs_scaled", "pc1_scaled")


standard_report_model_list <- list()

scaled_sleep_vars <- shs_df %>% dplyr::select(tst_hr, log_mpsd, sdtst,
                                              sme, frag, waso,
                                              ahi, sws_per, rem_per,
                                              quality, sol, ess,
                                              log_timing, shs_scaled,
                                              pc1_scaled) %>% scale()

scaled_sleep_vars <- rename_with(as_tibble(scaled_sleep_vars), ~ paste0(., "_scaled"), fixed = TRUE)
head(scaled_sleep_vars)

dput(names(scaled_sleep_vars))
sleep_vars_scaled <- c("tst_hr_scaled", "log_mpsd_scaled", "sdtst_scaled", "sme_scaled", 
                       "frag_scaled", "waso_scaled", "ahi_scaled", "sws_per_scaled", 
                       "rem_per_scaled", "quality_scaled", "sol_scaled", "ess_scaled", 
                       "log_timing_scaled", "pc1_scaled_scaled")

shs_df_scaled <- as_tibble(cbind(shs_df, scaled_sleep_vars))

shs_df_scaled <- shs_df_scaled %>% 
  dplyr::mutate(log_mpsd_scaled = log_mpsd_scaled*-1,
                sdtst_scaled = sdtst_scaled*-1,
                frag_scaled = frag_scaled*-1,
                waso_scaled = waso_scaled*-1,
                ahi_scaled = ahi_scaled*-1,
                quality_scaled = quality_scaled*-1,
                ess_scaled = ess_scaled*-1)

primary_report_table_standard <- c()

for(i in 1:length(sleep_vars_scaled)){
  
  adjusted_sleep_formula_scaled <- paste0("Surv(dthtt-slpexam1, death) ~ ", 
                                      sleep_vars_scaled[i], 
                                      " + age5c + female + race + 
                       modvig_pa + smoke + income_tertile + 
                       degree_attain + married + aehi_10")
  
  
  ## UNADJUSTED REGRESSIONS
  
  death_mod_scaled <- coxph(formula = as.formula(adjusted_sleep_formula_scaled),
                        data    = shs_df_scaled, x = T, y=T)
  
  summary(death_mod_scaled)
 standard_report_model_list[[i]] <- death_mod_scaled
  
  # For coefficients
  HR_coefs <- exp(death_mod_di$coefficients)
  
  # For confidence interval
  cc <- as.data.frame(coef(summary(death_mod_scaled)))
  names(cc) <- c("coef", "hazard_ratio", "se_coef", "z", "p")
  
  cc$LB <- summary(death_mod_scaled)$conf.int[1,3]
  cc$UB <- summary(death_mod_scaled)$conf.int[1,4]
  cc$var <- sleep_vars_scaled[i]
  
  print(i)

  primary_report_table_standard <- rbind(primary_report_table_standard, cc[1,])
  
  # write.csv(report_df, file = paste0(sleep_vars[j], "-RR.csv"))
  
}

primary_report_table_standard

## Label
class(primary_report_table_di$var) # character

primary_report_table_di$group <- factor(primary_report_table_di$var,
                                     levels = c("tst_di", "sme_di", "mpsd_di",
                                                "ess_di", "quality_di", "frag_di",
                                                "swsper_di", "remper_di", "ahi_di",
                                                "waso_di", "sol_di", "timing_di",
                                                "sdtst_di", "waso_act_di"))

primary_report_table_di$group <- factor(primary_report_table_di$group,
                                     labels = c("Duration 6-8 hours", 
                                                "Sleep maintenance efficiency >90%", 
                                                "Midpoint irregularity <30 minutes",
                                                "Epworth Sleepiness Scale score \u226410", 
                                                "Quality = very sound or \n sound/restful", 
                                                "Fragmentation \u226415",
                                                "N3 = 16%\u201420%", 
                                                "Rapid eye movement = 21%\u201430%", 
                                                "The Apnea-Hypopnea Index \u226415 events/hour",
                                                "Wake after sleep onset <60 minutes", 
                                                "Difficulties initiating sleep \u22641/week", 
                                                "Timing = 02:00\u201404:00",
                                                "Duration irregularity <60 minutes"))


primary_report_table_standard$group <- factor(primary_report_table_standard$var)

primary_report_table_standard$group <- factor(primary_report_table_standard$var, levels = sleep_vars_scaled)
                                       
primary_report_table_standard$group <- factor(primary_report_table_standard$group, labels = c("Total sleep time",
           "Midpoint sd (log; reverse)",
           "Duration sd (reverse)",
           "Maintenance efficiency",
           "Fragmentation (reverse)",
           "Wake after sleep onset (reverse)",
           "The Apnea-Hypopnea Index (reverse)",
           "% N3", "% R", "Quality (reverse)",
           "Onset latency", "Epworth Sleepiness Scale (reverse)",
           "Timing (log)",
           "Sleep score (PC1)"))                                       
                    
mortality_primary_plot <- ggplot(primary_report_table_di, 
                                 aes(x = reorder(group, -hazard_ratio), 
                                     y = hazard_ratio)) +
  geom_point() + 
  geom_errorbar(aes(ymin = LB, ymax = UB), width = 0.2, size = 0.8) + 
  coord_flip() + 
  theme_classic(base_size = 12) +
  geom_hline(yintercept = 1, linetype = "dashed") + 
  ylab("\n ") +
  xlab("")+
  theme(legend.position = "none") +
  # ggtitle(expression(underline(Hazard~Ratio~'(95% CI)'))) +
  theme(plot.title = element_text(angle = 0, hjust=0.5, vjust=-1, size = 10))

mortality_primary_plot

mortality_primary_plot_standard <- ggplot(primary_report_table_standard, 
                                 aes(x = reorder(group, -hazard_ratio), 
                                     y = hazard_ratio)) +
  scale_fill_discrete(guide="none") +
  geom_point() + 
  geom_errorbar(aes(ymin = LB, ymax = UB), width = 0.2, size = 0.8) + 
  coord_flip() + 
  theme_classic(base_size = 12) +
  geom_hline(yintercept = 1, linetype = "dashed") + 
  ylab("\n ") +
  xlab("")+
  theme(legend.position = "none") +
  # ggtitle(expression(underline(Hazard~Ratio~'(95% CI)'))) +
  theme(plot.title = element_text(angle = 0, hjust=0.5, vjust=-1, size = 10)) +
  ylab("Hazard ratio") + 
  xlab("Sleep metric \n") +
  theme(text = element_text(size = 12, colour = "black"),
        axis.text.y = element_text(hjust = 0)) + 
  scale_y_continuous(limits=c(0, 2))

mortality_primary_plot_standard

head(primary_report_table_di)
ggsave(plot = mortality_primary_plot,
       filename = "mortality_primary.png",
       width = 120,
       height = 140,
       units = "mm",
       dpi=1200)

ggsave(plot = mortality_primary_plot_standard,
       filename = "mortality_primary_standard.png",
       width = 140,
       height = 140,
       units = "mm",
       dpi=900)


primary_report_table_standard$hr <- sprintf("%.2f", 
                                            primary_report_table_standard$hazard_ratio, digits = 2)


primary_report_table_standard$lb <- sprintf("%.2f", 
                                            primary_report_table_standard$LB, digits = 2)


primary_report_table_standard$ub <- sprintf("%.2f", 
                                            primary_report_table_standard$UB, digits = 2)

primary_report_table_standard$hr_ci <- with(primary_report_table_standard$hr_ci,
                                            paste0(primary_report_table_standard$hr, 
                                                   " [", primary_report_table_standard$lb, 
                                                   ",", primary_report_table_standard$ub, "]"))
  
test_df <- primary_report_table_standard %>% dplyr::arrange(hr)


mortality_rrs <- ggplot(test_df, aes(x = reorder(group, -hazard_ratio), y = hr)) +
  coord_flip() + 
  theme_classic(base_size = 2) +
  scale_y_continuous(limits = c(0.25, 0.75)) + 
  
  ylab("\n ") +
  xlab("")+
  theme(legend.position = "none") +
  scale_fill_discrete(guide="none") + 
  # facet_grid(~param_ci) + 
  geom_text(data = primary_report_table_standard,
            aes(label = as.character(hr_ci)), y = 0.5, size = 4, 
            parse = F)+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())  +
  theme(strip.background = element_rect(
    color= NA, size=0, linetype="solid"
  )
  ) +
  theme(axis.line = element_line(colour = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+ labs(tag = " ") +
  
  ggtitle(expression(underline(Hazard~Ratio~'(95% CI)'))) +
  theme(plot.title = element_text(angle = 0, hjust=0.5, vjust=-1, size = 12))

grid.arrange(mortality_primary_plot_standard, mortality_rrs,
             nrow = 1)

ggsave(plot = mortality_rrs,
       filename = "mortalityrrs.png",
       width = 100,
       height = 140,
       units = "mm",
       dpi=1200)

# primary_report_table: combine RR and 95%CI
primary_report_table_standard$report_rr <- paste0(primary_report_table_standard$hr, " [", sprintf("%.2f", primary_report_table_standard$LB), ", ", 
                                                  sprintf("%.2f", primary_report_table_standard$UB), "]")
reported_rrs_cis <- primary_report_table_standard %>% dplyr::select(group, report_rr)

write.csv(reported_rrs_cis, file = "mortality_report_rr_ci.csv")              


primary_report_table_di <- primary_report_table_di %>% dplyr::arrange(hr)

primary_report_table_di$report_rr <- paste0(primary_report_table_di$hr, " [", sprintf("%.2f", primary_report_table_di$LB), ", ", 
                                                  sprintf("%.2f", primary_report_table_di$UB), "]")
write.csv(primary_report_table_di, file = "primary_report_table_di.csv")

