# Score Cox regression models
## Not run: library(survival)
library(rms)
library(prodlim)
library(riskRegression)

set.seed(8675309)

shs_df$time_yr <- with(shs_df, dthtt-slpexam1)/365

base_mod_0 <- coxph(Surv(time_yr, death) ~ age5c + female + race + bmi,  
                   data = shs_df,
                   x = TRUE)

pc_mod_1 <- coxph(Surv(time_yr, death) ~ age5c + female + race + bmi +
                     pc1_scaled,  
                   data = shs_df,
                   x = TRUE)

shs_mod_1 <- coxph(Surv(time_yr, death) ~ age5c + female + race + bmi +
                    shs_scaled,  
                  data = shs_df,
                  x = TRUE)

cluster_mod_1 <- coxph(Surv(time_yr, death) ~ age5c + female + race + bmi +
                     cluster_f,  
                   data = shs_df,
                   x = TRUE)

tst_mod_1 <- coxph(Surv(time_yr, death) ~ age5c + female + race + bmi +
                    tst_hr,  
                  data = shs_df,
                  x = TRUE)

sme_mod_1 <- coxph(Surv(time_yr, death) ~ age5c + female + race + bmi +
                    sme,  
                  data = shs_df,
                  x = TRUE)

frag_mod_1 <- coxph(Surv(time_yr, death) ~ age5c + female + race + bmi +
                    frag,  
                  data = shs_df,
                  x = TRUE)

ahi_mod_1 <- coxph(Surv(time_yr, death) ~ age5c + female + race + bmi +
                    ahi,  
                  data = shs_df,
                  x = TRUE)

n3_mod_1 <- coxph(Surv(time_yr, death) ~ age5c + female + race + bmi +
                    sws_per,  
                  data = shs_df,
                  x = TRUE)

rem_mod_1 <- coxph(Surv(time_yr, death) ~ age5c + female + race + bmi +
                    rem_per,  
                  data = shs_df,
                  x = TRUE)

mpsd_mod_1 <- coxph(Surv(time_yr, death) ~ age5c + female + race + bmi +
                    log_mpsd,  
                  data = shs_df,
                  x = TRUE)

sdtst_mod_1 <- coxph(Surv(time_yr, death) ~ age5c + female + race + bmi +
                    sdtst,  
                  data = shs_df,
                  x = TRUE)

ess_mod_1 <- coxph(Surv(time_yr, death) ~ age5c + female + race + bmi +
                       ess,  
                     data = shs_df,
                     x = TRUE)

waso_mod_1 <- coxph(Surv(time_yr, death) ~ age5c + female + race + bmi +
                     waso,  
                   data = shs_df,
                   x = TRUE)

quality_mod_1 <- coxph(Surv(time_yr, death) ~ age5c + female + race + bmi +
                      quality,  
                    data = shs_df,
                    x = TRUE)

sol_mod_1 <- coxph(Surv(time_yr, death) ~ age5c + female + race + bmi +
                         sol,  
                       data = shs_df,
                       x = TRUE)

timing_mod_1 <- coxph(Surv(time_yr, death) ~ age5c + female + race + bmi +
                         log_timing,  
                       data = shs_df,
                       x = TRUE)

mods_toscore <- list(base_mod = base_mod_0, pc_mod = pc_mod_1, 
                     tst_mod = tst_mod_1, 
                    waso_mod = waso_mod_1,
                     rem_mod = rem_mod_1, sws_mod = n3_mod_1,
                     mpsd_mod = mpsd_mod_1, sdtst_mod = sdtst_mod_1)

x_s <- Score(mods_toscore,
             formula = Surv(time_yr, death) ~ 1, data = shs_df, 
             times = seq(0, 8, by=1),
             metrics = "AUC",
             plots=c("calibration","ROC"))


auc_temp_df <- x_s$AUC

auc_df <- auc_temp_df$score

auc_df$sleep_model <- factor(auc_df$model, labels = c("Base", 
                          "PC1", "SHS", "TST",
                          "WASO",
                          "REM", "N3", "MPSD",
                          "SDTST"))

ggplot(auc_df, aes(x = times, y = AUC, colour = sleep_model)) + 
  geom_line(size = 1) + 
  theme_bw() +
  xlab("Time (years since MESA-Sleep)") + 
  labs(colour = "Sleep metric")

########### Dichotomous
base_mod_0 <- coxph(Surv(time_yr, death) ~ age5c + female + race + bmi,  
                    data = shs_df,
                    x = TRUE)

tst_mod_1_di <- coxph(Surv(time_yr, death) ~ age5c + female + race + bmi +
                     tst_di,  
                   data = shs_df,
                   x = TRUE)

sme_mod_1_di <- coxph(Surv(time_yr, death) ~ age5c + female + race + bmi +
                     sme_di,  
                   data = shs_df,
                   x = TRUE)

frag_mod_1_di <- coxph(Surv(time_yr, death) ~ age5c + female + race + bmi +
                      frag_di,  
                    data = shs_df,
                    x = TRUE)

ahi_mod_1_di <- coxph(Surv(time_yr, death) ~ age5c + female + race + bmi +
                     ahi_di,  
                   data = shs_df,
                   x = TRUE)

n3_mod_1_di <- coxph(Surv(time_yr, death) ~ age5c + female + race + bmi +
                    swsper_di,  
                  data = shs_df,
                  x = TRUE)

rem_mod_1_di <- coxph(Surv(time_yr, death) ~ age5c + female + race + bmi +
                     remper_di,  
                   data = shs_df,
                   x = TRUE)

mpsd_mod_1_di <- coxph(Surv(time_yr, death) ~ age5c + female + race + bmi +
                      mpsd_di,  
                    data = shs_df,
                    x = TRUE)

sdtst_mod_1_di <- coxph(Surv(time_yr, death) ~ age5c + female + race + bmi +
                       sdtst_di,  
                     data = shs_df,
                     x = TRUE)

ess_mod_1_di <- coxph(Surv(time_yr, death) ~ age5c + female + race + bmi +
                     ess_di,  
                   data = shs_df,
                   x = TRUE)

waso_mod_1_di <- coxph(Surv(time_yr, death) ~ age5c + female + race + bmi +
                      waso_di,  
                    data = shs_df,
                    x = TRUE)

quality_mod_1_di <- coxph(Surv(time_yr, death) ~ age5c + female + race + bmi +
                         quality_di,  
                       data = shs_df,
                       x = TRUE)

mods_toscore_di <- list(base_mod = base_mod_0,
                     tst_mod = tst_mod_1_di, 
                     waso_mod = waso_mod_1_di,
                     rem_mod = rem_mod_1_di, sws_mod = n3_mod_1_di,
                     mpsd_mod = mpsd_mod_1_di, sdtst_mod = sdtst_mod_1_di)

di_mods <- Score(mods_toscore_di,
             formula = Surv(time_yr, death) ~ 1, data = shs_df, 
             times = seq(1, 8, by=1),
             metrics = "AUC", 
             plots=c("calibration","ROC"))

di_mods

auc_temp_df_di <- di_mods$AUC

auc_df_di <- auc_temp_df_di$score

auc_df_di$sleep_model <- factor(auc_df_di$model, labels = c("Base", 
                                                      "TST",
                                                      "WASO",
                                                      "REM", "N3", "MPSD",
                                                      "SDTST"))

ggplot(auc_df_di, aes(x = times, y = AUC, colour = sleep_model)) + 
  geom_line(size = 1) + 
  theme_bw() +
  xlab("Time (years since MESA-Sleep)") + 
  labs(colour = "Sleep metric ")

## Metric by metric comparisons
tst_toscore <- list(base_mod = base_mod_0,
                    pc1 = pc_mod_1,
                        tst_mod_di = tst_mod_1_di, 
                        tst_mod = tst_mod_1)

tst_mods <- Score(tst_toscore,
                 formula = Surv(time_yr, death) ~ 1, data = shs_df, 
                 times = seq(1, 8, by=1),
                 metrics = "AUC",
                 plots=c("calibration","ROC"))

tst_mods

tst_mods_di <- tst_mods$AUC
tst_auc_df <- tst_mods_di$score

tst_auc_plot <- ggplot(tst_auc_df, aes(x = times, y = AUC, colour = model)) + 
  geom_line(size = 1) + 
  theme_bw() +
  xlab("Time (years since MESA-Sleep)") + 
  labs(colour = "Sleep metric ")




rem_toscore <- list(base_mod = base_mod_0,
                    pc1 = pc_mod_1,
                    rem_mod_di = rem_mod_1_di, 
                    rem_mod = rem_mod_1)

rem_mods <- Score(rem_toscore,
                  formula = Surv(time_yr, death) ~ 1, data = shs_df, 
                  times = seq(1, 8, by=1),
                  metrics = "AUC",
                  plots=c("calibration","ROC"))

rem_mods

rem_mods_di <- rem_mods$AUC
rem_auc_df <- rem_mods_di$score

rem_auc_plot <- ggplot(rem_auc_df, aes(x = times, y = AUC, colour = model)) + 
  geom_line(size = 1) + 
  theme_bw() +
  xlab("Time (years since MESA-Sleep)") + 
  labs(colour = "Sleep metric ")


## 

mpsd_toscore <- list(base_mod = base_mod_0,
                     pc1 = pc_mod_1,
                     mpsd_mod_di = mpsd_mod_1_di, 
                     mpsd_mod = mpsd_mod_1)

mpsd_mods <- Score(mpsd_toscore,
                  formula = Surv(time_yr, death) ~ 1, data = shs_df, 
                  times = seq(1, 8, by=1),
                  metrics = "AUC",
                  plots=c("calibration","ROC"))

mpsd_mods

mpsd_mods_di <- mpsd_mods$AUC
mpsd_auc_df <- mpsd_mods_di$score

mpsd_auc_plot <- ggplot(mpsd_auc_df, aes(x = times, y = AUC, colour = model)) + 
  geom_line(size = 1) + 
  theme_bw() +
  xlab("Time (years since MESA-Sleep)") + 
  labs(colour = "Sleep metric")

## SDTST 

sdtst_toscore <- list(base_mod = base_mod_0,
                      pc1 = pc_mod_1,
                      sdtst_mod_di = sdtst_mod_1_di, 
                      sdtst_mod = sdtst_mod_1)

sdtst_mods <- Score(sdtst_toscore,
                    formula = Surv(time_yr, death) ~ 1, data = shs_df, 
                    times = seq(1, 8, by=1),
                    metrics = "AUC",
                    plots=c("calibration","ROC"))

sdtst_mods

sdtst_mods_di <- sdtst_mods$AUC
sdtst_auc_df <- sdtst_mods_di$score

sdtst_auc_plot <- ggplot(sdtst_auc_df, aes(x = times, y = AUC, colour = model)) + 
  geom_line(size = 1) + 
  theme_bw() +
  xlab("Time (years since MESA-Sleep)") + 
  labs(colour = "Sleep metric ")


## SWS

sws_per_toscore <- list(base_mod = base_mod_0,
                        pc1 = pc_mod_1,
                        sws_per_mod_di = n3_mod_1_di, 
                        sws_per_mod = n3_mod_1)

sws_per_mods <- Score(sws_per_toscore,
                      formula = Surv(time_yr, death) ~ 1, data = shs_df, 
                      times = seq(1, 8, by=1),
                      metrics = "AUC",
                      plots=c("calibration","ROC"))

sws_per_mods

sws_per_mods_di <- sws_per_mods$AUC
sws_per_auc_df <- sws_per_mods_di$score

swsper_auc_plot <- ggplot(sws_per_auc_df, aes(x = times, y = AUC, colour = model)) + 
  geom_line(size = 1) + 
  theme_bw() +
  xlab("Time (years since MESA-Sleep)") + 
  labs(colour = "Sleep metric ")

## 

waso_toscore <- list(base_mod = base_mod_0,
                     pc1 = pc_mod_1,
                     waso_mod_di = waso_mod_1_di, 
                     waso_mod = waso_mod_1)

waso_mods <- Score(waso_toscore,
                   formula = Surv(time_yr, death) ~ 1, data = shs_df, 
                   times = seq(1, 8, by=1),
                   metrics = "AUC",
                   plots=c("calibration","ROC"))

waso_mods

waso_mods_di <- waso_mods$AUC
waso_auc_df <- waso_mods_di$score

waso_auc_plot <- ggplot(waso_auc_df, aes(x = times, y = AUC, colour = model)) + 
  geom_line(size = 1) + 
  theme_bw() +
  xlab("Time (years since MESA-Sleep)") + 
  labs(colour = "Sleep metric ")


## 
cluster_toscore <- list(base_mod = base_mod_0,
                     pc1 = pc_mod_1,
                     cluster_f = cluster_mod_1, 
                     tst = tst_mod_1,
                     mpsd = mpsd_mod_1,
                     sdtst = sdtst_mod_1,
                     tst_di = tst_mod_1_di,
                     mpsd_di = mpsd_mod_1_di,
                     sdtst_di = sdtst_mod_1_di)

cluster_mods <- Score(cluster_toscore,
                   formula = Surv(time_yr, death) ~ 1, data = shs_df, 
                   times = seq(1, 8, by=1),
                   metrics = "AUC",
                   plots=c("calibration","ROC"))

cluster_mods

cluster_mods_di <- cluster_mods$AUC
cluster_auc_df <- cluster_mods_di$score

cluster_auc_plot <- ggplot(cluster_auc_df, aes(x = times, y = AUC, colour = model)) + 
  geom_line(size = 1) + 
  theme_bw() +
  xlab("Time (years since MESA-Sleep)") + 
  labs(colour = "Sleep metric ")

## GG save
ggsave(plot = cluster_auc_plot,
       filename = "cluster_auc_plot.png",
       width = 140,
       height = 140,
       units = "mm",
       dpi=1200)

ggsave(plot = mpsd_auc_plot,
       filename = "mpsd_auc_plot.png",
       width = 140,
       height = 140,
       units = "mm",
       dpi=1200)

ggsave(plot = sdtst_auc_plot,
       filename = "sdtst_auc_plot.png",
       width = 140,
       height = 140,
       units = "mm",
       dpi=1200)

ggsave(plot = rem_auc_plot,
       filename = "rem_auc_plot.png",
       width = 140,
       height = 140,
       units = "mm",
       dpi=1200)

ggsave(plot = swsper_auc_plot,
       filename = "swsper_auc_plot.png",
       width = 140,
       height = 140,
       units = "mm",
       dpi=1200)

ggsave(plot = waso_auc_plot,
       filename = "waso_auc_plot.png",
       width = 140,
       height = 140,
       units = "mm",
       dpi=1200)

## ROCs and 
png("tst_roc.png", width=900, height = 900)
tst_roc <- plotROC(tst_mods)
# 3. Close the file
dev.off()

png("cluster_roc.png", width=900, height = 900)
cluster_roc <- plotROC(cluster_mods)
# 3. Close the file
dev.off()



png("waso_roc.png", width=900, height = 900)
waso_roc <- plotROC(waso_mods)
dev.off()


png("rem_roc.png", width=900, height = 900)
rem_roc <- plotROC(rem_mods)
dev.off()


png("sws_roc.png", width=900, height = 900)
sws_roc <- plotROC(sws_per_mods)
dev.off()

png("mpsd_roc.png", width=900, height = 900)
mpsd_roc <- plotROC(mpsd_mods)
dev.off()


png("sdtst_roc.png", width=900, height = 900)
sdtst_roc <- plotROC(sdtst_mods)
dev.off()
