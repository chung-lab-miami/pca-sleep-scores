## Hmisc (Harrell)
## Univariate models
options(scipen=999)
set.seed(8675309)

shs_df$time <- shs_df$dthtt - shs_df$slpexam1


somers_rank <- rcorrcens(Surv(time, death) ~ pc1_scaled + shs_scaled + 
                tst_hr + sme + quality + waso + frag + sws_per + rem_per + 
                ahi + mpsd + sdtst + ess + log_timing, data = shs_df)

somers_rank

#           C    Dxy  aDxy    SD    Z      P    n
pc1_scaled 0.617  0.234 0.234 0.043 5.40 0.0000 1726
shs_scaled 0.586  0.172 0.172 0.041 4.16 0.0000 1726
tst_hr     0.567  0.135 0.135 0.050 2.69 0.0072 1726
sme        0.494 -0.012 0.012 0.046 0.25 0.7996 1726
quality    0.493 -0.015 0.015 0.043 0.34 0.7366 1726
waso       0.373 -0.254 0.254 0.045 5.69 0.0000 1726
frag       0.428 -0.143 0.143 0.045 3.21 0.0013 1726
sws_per    0.587  0.174 0.174 0.045 3.90 0.0001 1726
rem_per    0.587  0.173 0.173 0.045 3.83 0.0001 1726
ahi        0.417 -0.167 0.167 0.044 3.82 0.0001 1726
mpsd       0.435 -0.130 0.130 0.045 2.88 0.0040 1726
sdtst      0.439 -0.122 0.122 0.044 2.76 0.0058 1726
ess        0.520  0.040 0.040 0.045 0.89 0.3718 1726
log_timing 0.470 -0.059 0.059 0.048 1.25 0.2115 1726

somers_rank_di <- rcorrcens(Surv(time, death) ~ sdtst_di + mpsd_di + swsper_di + 
                            ahi_di + tst_di + waso_di + remper_di + timing_di +
                              frag_di + quality_di + sol_di + sme_di + ess_di + 
                              pc1_di + shs_di, data = shs_df)

somers_rank_di

#Somers Rank Correlation for Censored Data    Response variable:Surv(time, death)
               C    Dxy  aDxy    SD    Z      P    n
sdtst_di   0.548  0.096 0.096 0.036 2.68 0.0074 1726
mpsd_di    0.529  0.058 0.058 0.032 1.83 0.0668 1726
swsper_di  0.519  0.038 0.038 0.019 1.96 0.0495 1726
ahi_di     0.568  0.136 0.136 0.035 3.87 0.0001 1726
tst_di     0.568  0.135 0.135 0.038 3.51 0.0004 1726
waso_di    0.573  0.146 0.146 0.033 4.44 0.0000 1726
remper_di  0.540  0.079 0.079 0.033 2.42 0.0154 1726
timing_di  0.518  0.037 0.037 0.038 0.97 0.3320 1726
frag_di    0.531  0.062 0.062 0.030 2.09 0.0364 1726
quality_di 0.498 -0.005 0.005 0.038 0.13 0.8960 1726
sol_di     0.505  0.011 0.011 0.037 0.29 0.7703 1726
sme_di     0.492 -0.017 0.017 0.035 0.48 0.6301 1726
ess_di     0.488 -0.025 0.025 0.024 1.02 0.3083 1726

# # Import cstat csv
# c_statistic. <- read.csv("C:/Users/jj261/Dropbox (Partners HealthCare)/2021_mesa_mortality/c_statistic .csv")
# cstat_df <- c_statistic.
# 
# cstat_df_cont <- cstat_df %>% dplyr::filter(Type=="continuous")
# 
# cstat_df_di <- cstat_df %>% dplyr::filter(Type=="dichotomous")
# 
# cstat_df_cont <- cstat_df_cont %>% dplyr::arrange(desc(C_stat))
# write.csv(cstat_df_cont, file = "cstat_continuous.csv")
# 
# cstat_df_di <- cstat_df_di %>% dplyrshs_mod_0 <- coxph(Surv(dthtt-slpexam1, death) ~ age5c + female + race,  
                   # data = shs_df,
                   # x = TRUE)
shs_mod_1 <- coxph(Surv(dthtt-slpexam1, death) ~ age5c + female + race + shs_scaled,  
                   data = shs_df,
                   x = TRUE)
shs_mods <- list(shs_mod_0, shs_mod_1)


IPA(shs_mods, times= c(500, 1000, 1500, 2000, 2500, 3000))

pc1_model_1 <- coxph(Surv(dthtt-slpexam1, death) ~ pc1_scaled + age5c + female + race + 
                       modvig_pa + smoke + income_tertile + 
                       degree_attain + married + aehi_10, data = shs_df,
                     x = TRUE)

summary(pc1_model_1)
ShowRegTable(pc1_model_1)

IPA(pc1_model_1, times= c(500, 1000, 1500, 2000, 2500, 3000))::arrange(desc(C_stat))
# write.csv(cstat_df_di, file = "cstat_df_di.csv")

## Other importance measures



mpsd_model_1 <- coxph(Surv(dthtt-slpexam1, death) ~ log_mpsd + age5c + female + race + 
                       modvig_pa + smoke + income_tertile + 
                       degree_attain + married + aehi_10, data = shs_df,
                     x = TRUE)

summary(mpsd_model_1)
ShowRegTable(mpsd_model_1)

IPA(mpsd_model_1, times= c(500, 1000, 1500, 2000, 2500, 3000))














rcorr.cens(shs_df$pc1_scaled, Surv(shs_df$time, shs_df$death)) # 0.61727217 
rcorr.cens(shs_df$shs_scaled, Surv(shs_df$time, shs_df$death)) # 0.58623399

rcorr.cens(shs_df$tst_hr, Surv(shs_df$time, shs_df$death))   # 0.56734563
rcorr.cens(shs_df$log_mpsd, Surv(shs_df$time, shs_df$death)) # 0.43486394
rcorr.cens(shs_df$sdtst, Surv(shs_df$time, shs_df$death))    # 0.43916949

rcorr.cens(shs_df$pc1_di, Surv(shs_df$time, shs_df$death))   # 
rcorr.cens(shs_df$shs_di, Surv(shs_df$time, shs_df$death))   # 0.5584543
rcorr.cens(shs_df$tst_di, Surv(shs_df$time, shs_df$death))   # 0.56755390
rcorr.cens(shs_df$mpsd_di, Surv(shs_df$time, shs_df$death))  # 0.52898765
rcorr.cens(shs_df$sdtst_di, Surv(shs_df$time, shs_df$death)) # 0.54814865

shs_model_1 <- coxph(Surv(dthtt-slpexam1, death) ~ shs_scaled + age5c + female + race +
                       modvig_pa + smoke + income_tertile +
                       degree_attain + married + aehi_10 + bmi,
                     data = Train, x = T, y=T, method="breslow")

## Multivariate
rcorrcens(Surv(time, death) ~ pc1_scaled + shs_scaled, data = shs_df)
# C = 

rcorrcens(Surv(shs_df$time, shs_df$death) ~ shs_df$pc1_scaled + shs_df$age5c + shs_df$female + shs_df$race +
            shs_df$modvig_pa + shs_df$smoke + shs_df$income_tertile +
            shs_df$degree_attain + shs_df$married + shs_df$aehi_10)
# C = 0.617

rcorrcens(Surv(shs_df$time, shs_df$death) ~ shs_df$shs_scaled + shs_df$age5c + shs_df$female + shs_df$race +
            shs_df$modvig_pa + shs_df$smoke + shs_df$income_tertile +
            shs_df$degree_attain + shs_df$married + shs_df$aehi_10)
# C = 0.586

rcorrcens(Surv(shs_df$time, shs_df$death) ~ shs_df$tst + shs_df$age5c + shs_df$female + shs_df$race +
            shs_df$modvig_pa + shs_df$smoke + shs_df$income_tertile +
            shs_df$degree_attain + shs_df$married + shs_df$aehi_10)
# C = 0.567

rcorrcens(Surv(shs_df$time, shs_df$death) ~ shs_df$log_mpsd + shs_df$age5c + shs_df$female + shs_df$race +
            shs_df$modvig_pa + shs_df$smoke + shs_df$income_tertile +
            shs_df$degree_attain + shs_df$married + shs_df$aehi_10)

# C = 0.435

rcorrcens(Surv(shs_df$time, shs_df$death) ~ shs_df$sdtst + shs_df$age5c + shs_df$female + shs_df$race +
            shs_df$modvig_pa + shs_df$smoke + shs_df$income_tertile +
            shs_df$degree_attain + shs_df$married + shs_df$aehi_10)

# C = 0.439

rcorrcens(Surv(shs_df$time, shs_df$death) ~ shs_df$rem_per + shs_df$age5c + shs_df$female + shs_df$race +
            shs_df$modvig_pa + shs_df$smoke + shs_df$income_tertile +
            shs_df$degree_attain + shs_df$married + shs_df$aehi_10)

# C = 0.587

rcorrcens(Surv(shs_df$time, shs_df$death) ~ shs_df$sws_per + shs_df$age5c + shs_df$female + shs_df$race +
            shs_df$modvig_pa + shs_df$smoke + shs_df$income_tertile +
            shs_df$degree_attain + shs_df$married + shs_df$aehi_10)

# C = 0.587

rcorrcens(Surv(shs_df$time, shs_df$death) ~ shs_df$ahi + shs_df$age5c + shs_df$female + shs_df$race +
            shs_df$modvig_pa + shs_df$smoke + shs_df$income_tertile +
            shs_df$degree_attain + shs_df$married + shs_df$aehi_10)

# C = 0.417

rcorrcens(Surv(shs_df$time, shs_df$death) ~ shs_df$ess + shs_df$age5c + shs_df$female + shs_df$race +
            shs_df$modvig_pa + shs_df$smoke + shs_df$income_tertile +
            shs_df$degree_attain + shs_df$married + shs_df$aehi_10)

# C = 0.520 

rcorrcens(Surv(shs_df$time, shs_df$death) ~ shs_df$quality + shs_df$age5c + shs_df$female + shs_df$race +
            shs_df$modvig_pa + shs_df$smoke + shs_df$income_tertile +
            shs_df$degree_attain + shs_df$married + shs_df$aehi_10)

# C = 0.493

rcorrcens(Surv(shs_df$time, shs_df$death) ~ shs_df$log_timing + shs_df$age5c + shs_df$female + shs_df$race +
            shs_df$modvig_pa + shs_df$smoke + shs_df$income_tertile +
            shs_df$degree_attain + shs_df$married + shs_df$aehi_10)

# C = 0.470






## Dichotomous

plot(shs_df$shs_scaled, shs_df$sdtst)
rcorrcens(Surv(shs_df$time, shs_df$death) ~ shs_df$pc1_di + shs_df$age5c + shs_df$female + shs_df$race +
            shs_df$modvig_pa + shs_df$smoke + shs_df$income_tertile +
            shs_df$degree_attain + shs_df$married + shs_df$aehi_10)
# C = 0.582

rcorrcens(Surv(shs_df$time, shs_df$death) ~ shs_df$shs_di + shs_df$age5c + shs_df$female + shs_df$race +
            shs_df$modvig_pa + shs_df$smoke + shs_df$income_tertile +
            shs_df$degree_attain + shs_df$married + shs_df$aehi_10)
# C = 0.558

rcorrcens(Surv(shs_df$time, shs_df$death) ~ shs_df$tst_di + shs_df$age5c + shs_df$female + shs_df$race +
            shs_df$modvig_pa + shs_df$smoke + shs_df$income_tertile +
            shs_df$degree_attain + shs_df$married + shs_df$aehi_10)
# C = 0.568

rcorrcens(Surv(shs_df$time, shs_df$death) ~ shs_df$mpsd_di + shs_df$age5c + shs_df$female + shs_df$race +
            shs_df$modvig_pa + shs_df$smoke + shs_df$income_tertile +
            shs_df$degree_attain + shs_df$married + shs_df$aehi_10)

# C = 0.529

rcorrcens(Surv(shs_df$time, shs_df$death) ~ shs_df$sdtst_di + shs_df$age5c + shs_df$female + shs_df$race +
            shs_df$modvig_pa + shs_df$smoke + shs_df$income_tertile +
            shs_df$degree_attain + shs_df$married + shs_df$aehi_10)

# C = 0.548






plot(shs_df$shs_scaled, shs_df$tst)
plot(shs_df$shs_scaled, shs_df$log_mpsd)

plot(shs_df$shs_scaled, shs_df$ahi)
plot(shs_df$shs_scaled, shs_df$log_timing)



ggplot(shs_df, aes(pc1_scaled, y=sdtst)) + 
  geom_point() +
  theme_minimal() +
  stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm', formula= y~x) + 
  scale_x_continuous(breaks=-4:4) + 
  xlab("Sleep health score (Principal Component 1)") + 
  ylab("Duration irregularity (min, sd) \n (higher is worse)")


ggplot(shs_df, aes(pc1_scaled, y=tst)) + 
  geom_point() +
  theme_minimal() +
  stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm', formula= y~x) + 
  scale_x_continuous(breaks=-4:4)

ggplot(shs_df, aes(pc1_scaled, y=log_mpsd)) + 
  geom_point() +
  theme_minimal() +
  geom_smooth(method='lm') + 
  scale_x_continuous(breaks=-4:4)

ggplot(shs_df, aes(pc1_scaled, y=quality)) + 
  geom_point() +
  theme_minimal() +
  geom_smooth(method='lm') + 
  geom_jitter(width = 0, height = 1) +
  scale_x_continuous(breaks=-4:4)

ggplot(shs_df, aes(pc1_scaled, y=ess)) + 
  geom_point() +
  theme_minimal() +
  geom_smooth(method='lm') + 
  geom_jitter(width = 0, height = 1) +
  scale_x_continuous(breaks=-4:4)

ggplot(shs_df, aes(pc1_scaled, y=log_timing)) + 
  geom_point() +
  theme_minimal() +
  geom_smooth(method='lm') + 
  scale_x_continuous(breaks=-4:4)

ggplot(shs_df, aes(pc1_scaled, y=sme)) + 
  geom_point() +
  theme_minimal() +
  geom_smooth(method='lm') + 
  scale_x_continuous(breaks=-4:4)

ggplot(shs_df, aes(pc1_scaled, y=frag)) + 
  geom_point() +
  theme_minimal() +
  geom_smooth(method='lm') + 
  scale_x_continuous(breaks=-4:4)

ggplot(shs_df, aes(pc1_scaled, y=waso)) + 
  geom_point() +
  theme_minimal() +
  geom_smooth(method='lm') + 
  scale_x_continuous(breaks=-4:4)

ggplot(shs_df, aes(pc1_scaled, y=sws_per)) + 
  geom_point() +
  theme_minimal() +
  geom_smooth(method='lm') + 
  scale_x_continuous(breaks=-4:4)

ggplot(shs_df, aes(pc1_scaled, y=rem_per)) + 
  geom_point() +
  theme_minimal() +
  geom_smooth(method='lm') + 
  scale_x_continuous(breaks=-4:4)

ggplot(shs_df, aes(pc1_scaled, y=ahi)) + 
  geom_point() +
  theme_minimal() +
  geom_smooth(method='lm') + 
  scale_x_continuous(breaks=-4:4)


## DOT SHAPE = DATA COLLECTION MODE?
## SMALLER DOTS
## TRANSPARENT DOTS
ahi_plot <- ggplot(shs_df, aes(shs_scaled, y=ahi)) + 
  geom_point() +
  geom_jitter() + 
  theme_minimal() +
  geom_smooth(method='lm') + 
  scale_x_continuous(breaks=-4:4) + 
  xlab("") + 
  ylab("Apnea-Hypopnea Index \n (events/hr)")

tst_plot <- ggplot(shs_df, aes(shs_scaled, y=tst_hr)) + 
  geom_point() +
  geom_jitter() + 
  theme_minimal() +
  geom_smooth(method='lm') + 
  scale_x_continuous(breaks=-4:4) + 
  xlab("") + 
  ylab("Total sleep time \n (hrs)")

mpsd_plot <- ggplot(shs_df, aes(shs_scaled, y=log_mpsd)) + 
  geom_point() +
  geom_jitter() + 
  theme_minimal() +
  geom_smooth(method='lm') + 
  scale_x_continuous(breaks=-4:4) + 
  xlab("") + 
  ylab("Midpoint sd \n (log, min)")

sdtst_plot <- ggplot(shs_df, aes(shs_scaled, y=sdtst)) + 
  geom_point() +
  geom_jitter() + 
  theme_minimal() +
  geom_smooth(method='lm') + 
  scale_x_continuous(breaks=-4:4) + 
  xlab("") + 
  ylab("Duration sd \n (min)")

waso_plot <- ggplot(shs_df, aes(shs_scaled, y=waso)) + 
  geom_point() +
  geom_jitter() + 
  theme_minimal() +
  geom_smooth(method='lm') + 
  scale_x_continuous(breaks=-4:4) + 
  xlab("Sleep health score (scaled)") + 
  ylab("Wake after sleep onset \n (min)")

sme_plot <- ggplot(shs_df, aes(shs_scaled, y=sme)) + 
  geom_point() +
  geom_jitter() + 
  theme_minimal() +
  geom_smooth(method='lm') + 
  scale_x_continuous(breaks=-4:4) + 
  xlab("") + 
  ylab("Maintenance efficiency \n (%)")

frag_plot <- ggplot(shs_df, aes(shs_scaled, y=frag)) + 
  geom_point() +
  geom_jitter() + 
  theme_minimal() +
  geom_smooth(method='lm') + 
  scale_x_continuous(breaks=-4:4) + 
  xlab("") + 
  ylab("Fragmentation")

timing_plot <- ggplot(shs_df, aes(shs_scaled, y=log_timing)) + 
  geom_point() +
  geom_jitter() + 
  # geom_violin() +
  theme_minimal() +
  geom_smooth(method='lm') + 
  scale_x_continuous(breaks=-4:4) + 
  xlab("") + 
  ylab("Timing \n (log, min)")

sws_plot <- ggplot(shs_df, aes(shs_scaled, y=sws_per)) + 
  geom_point(alpha=0.5) +
  geom_jitter() + 
  theme_minimal() +
  geom_smooth(method='lm') + 
  scale_x_continuous(breaks=-4:4) + 
  xlab("") + 
  ylab("N3 \n (%)")

rem_plot <- ggplot(shs_df, aes(shs_scaled, y=rem_per)) + 
  geom_point(alpha=0.5) +
  geom_jitter() + 
  theme_minimal() +
  geom_smooth(method='lm') + 
  scale_x_continuous(breaks=-4:4) + 
  xlab("") + 
  ylab("R \n (%)")

quality_plot <- ggplot(shs_df, aes(shs_scaled, y=quality)) + 
  geom_point() +
  geom_jitter() + 
  theme_minimal() +
  geom_smooth(method='lm') + 
  scale_x_continuous(breaks=-4:4) + 
  xlab("") + 
  ylab("Quality \n (reverse coded)")

ess_plot <- ggplot(shs_df, aes(shs_scaled, y=ess)) + 
  geom_point( ) +
  geom_jitter() + 
  theme_minimal() +
  geom_smooth(method='lm') + 
  scale_x_continuous(breaks=-4:4)+ 
  xlab("Sleep health score (scaled)") +
  ylab("Epworth Sleepiness Scale \n")


sol_plot <- ggplot(shs_df, aes(shs_scaled, y=sol_sub)) + 
  geom_point( ) +
  geom_jitter() + 
  theme_minimal() +
  geom_smooth(method='lm') + 
  scale_x_continuous(breaks=-4:4) + 
  xlab("Sleep health score (scaled)") + 
  ylab("Difficulties initiating sleep")


library(gridExtra)
grid.arrange(tst_plot, mpsd_plot, sdtst_plot, sme_plot, frag_plot,
             ahi_plot, timing_plot, sws_plot, rem_plot, quality_plot,
             ess_plot, waso_plot, sol_plot,
             ncol = 3
             )

## PC1 score
ahi_plot_pc <- ggplot(shs_df, aes(pc1_scaled, y=ahi)) + 
  geom_point() +
  geom_jitter() + 
  theme_minimal() +
  geom_smooth(method='lm') + 
  scale_x_continuous(breaks=-4:4) + 
  xlab("") + 
  ylab("Apnea-Hypopnea Index \n (events/hr)")

tst_plot_pc <- ggplot(shs_df, aes(pc1_scaled, y=tst_hr)) + 
  geom_point() +
  geom_jitter() + 
  theme_minimal() +
  geom_smooth(method='lm') + 
  scale_x_continuous(breaks=-4:4) + 
  xlab("") + 
  ylab("Total sleep time \n (hrs)")

mpsd_plot_pc <- ggplot(shs_df, aes(pc1_scaled, y=log_mpsd)) + 
  geom_point() +
  geom_jitter() + 
  theme_minimal() +
  geom_smooth(method='lm') + 
  scale_x_continuous(breaks=-4:4) + 
  xlab("") + 
  ylab("Midpoint sd \n (log, min)")

sdtst_plot_pc <- ggplot(shs_df, aes(pc1_scaled, y=sdtst)) + 
  geom_point() +
  geom_jitter() + 
  theme_minimal() +
  geom_smooth(method='lm') + 
  scale_x_continuous(breaks=-4:4) + 
  xlab("") + 
  ylab("Duration sd \n (min)")

waso_plot_pc <- ggplot(shs_df, aes(pc1_scaled, y=waso)) + 
  geom_point() +
  geom_jitter() + 
  theme_minimal() +
  geom_smooth(method='lm') + 
  scale_x_continuous(breaks=-4:4) + 
  xlab("Sleep health score (PC1)") + 
  ylab("Wake after sleep onset \n (min)")

sme_plot_pc <- ggplot(shs_df, aes(pc1_scaled, y=sme)) + 
  geom_point() +
  geom_jitter() + 
  theme_minimal() +
  geom_smooth(method='lm') + 
  scale_x_continuous(breaks=-4:4) + 
  xlab("") + 
  ylab("Maintenance efficiency \n (%)")

frag_plot_pc <- ggplot(shs_df, aes(pc1_scaled, y=frag)) + 
  geom_point() +
  geom_jitter() + 
  theme_minimal() +
  geom_smooth(method='lm') + 
  scale_x_continuous(breaks=-4:4) + 
  xlab("") + 
  ylab("Fragmentation")

timing_plot_pc <- ggplot(shs_df, aes(pc1_scaled, y=log_timing)) + 
  geom_point() +
  geom_jitter() + 
  # geom_violin() +
  theme_minimal() +
  geom_smooth(method='lm') + 
  scale_x_continuous(breaks=-4:4) + 
  xlab("") + 
  ylab("Timing \n (log, min)")

sws_plot_pc <- ggplot(shs_df, aes(pc1_scaled, y=sws_per)) + 
  geom_point(alpha=0.5) +
  geom_jitter() + 
  theme_minimal() +
  geom_smooth(method='lm') + 
  scale_x_continuous(breaks=-4:4) + 
  xlab("") + 
  ylab("N3 \n (%)")

rem_plot_pc <- ggplot(shs_df, aes(pc1_scaled, y=rem_per)) + 
  geom_point(alpha=0.5) +
  geom_jitter() + 
  theme_minimal() +
  geom_smooth(method='lm') + 
  scale_x_continuous(breaks=-4:4) + 
  xlab("") + 
  ylab("R \n (%)")

quality_plot_pc <- ggplot(shs_df, aes(pc1_scaled, y=quality)) + 
  geom_point() +
  geom_jitter() + 
  theme_minimal() +
  geom_smooth(method='lm') + 
  scale_x_continuous(breaks=-4:4) + 
  xlab("") + 
  ylab("Quality \n (reverse coded)")

ess_plot_pc <- ggplot(shs_df, aes(pc1_scaled, y=ess)) + 
  geom_point( ) +
  geom_jitter() + 
  theme_minimal() +
  geom_smooth(method='lm') + 
  scale_x_continuous(breaks=-4:4)+ 
  xlab("Sleep health score (PC1)") +
  ylab("Epworth Sleepiness Scale \n")


sol_plot_pc <- ggplot(shs_df, aes(pc1_scaled, y=sol_sub)) + 
  geom_point( ) +
  geom_jitter() + 
  theme_minimal() +
  geom_smooth(method='lm') + 
  scale_x_continuous(breaks=-4:4) + 
  xlab("Sleep health score (PC1)") + 
  ylab("Difficulties initiating sleep")


library(gridExtra)
grid.arrange(tst_plot_pc, mpsd_plot_pc, sdtst_plot_pc, sme_plot_pc, frag_plot_pc,
             ahi_plot_pc, timing_plot_pc, sws_plot_pc, rem_plot_pc, quality_plot_pc,
             ess_plot_pc, waso_plot_pc, sol_plot_pc,
             ncol = 3
)