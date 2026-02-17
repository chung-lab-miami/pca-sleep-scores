##################################################################################
##################################################################################
# Sleep variables
#############################################################################################
#############################################################################################
set.seed(8675309)
library(hms)
library(factoextra)
library(FactoMineR)
# From actigraphy
mesa_df$tst <- mesa_df$avgmainsleep5
mesa_df$sme <- mesa_df$avgmainteff5
mesa_df$mpsd_nonhms <- mesa_df$sdsleepmidpoint5
mesa_df$frag <- mesa_df$avgfragmentation5
mesa_df$sdtst <- mesa_df$sdmainsleep5

# From questionnaire
mesa_df$ess <- mesa_df$epslpscl5c
# mesa_df$whi <- mesa_df$whiirs5c # Higher is worse

# From PSG
mesa_df$sws_per <- mesa_df$times34p5
mesa_df$rem_per <- mesa_df$timeremp5
mesa_df$ahi <- mesa_df$oahi3pa5

#############################################################################################
#############################################################################################
# TST: 6-8 hours == favorable
mesa_df$tst_di <- ifelse(mesa_df$tst >= 360 & mesa_df$tst <= 480, 1, 0)

ggplot(mesa_df, aes(x = tst)) + 
  geom_histogram() + 
  theme_bw() +
  geom_vline(xintercept = c(360, 480), linetype = "dashed")

table(mesa_df$tst_di)

# Sleep maintenance efficiency: > 90% == favorable
mesa_df$sme_di <- ifelse(mesa_df$sme > 90, 1, 0)

# Epworth Sleepiness Scale: 5 or 10
mesa_df$ess_di <- ifelse(mesa_df$ess <= 10, 1, 0)

# WHI insomnia scale for quality (RC - higher is better)
# favstats(mesa_df$whi)
# mesa_df$whi_di <- ifelse(mesa_df$whiirs5c < 9, 1, 0)

# Midpoint sd (higher is worse - put in hours)
mesa_df$mpsd_hms <- as.hms(mesa_df$mpsd_nonhms)

# Subtract midnight and calculate in minutes
mesa_df$mpsd_reference <- as.hms(00:00:00)

head(mesa_df$mpsd_hms)
head(mesa_df$mpsd_reference)

head(mesa_df$mpsd_hms - mesa_df$mpsd_reference) # Correct

mesa_df$mpsd <- mesa_df$mpsd_hms - mesa_df$mpsd_reference

head(mesa_df$mpsd)
head(as.numeric(mesa_df$mpsd))

mesa_df$mpsd <- as.numeric(mesa_df$mpsd)
mesa_df$mpsd[mesa_df$mpsd >= 20000] <- NA # Exclude > 20k per Tianyi

mesa_df$mpsd <- mesa_df$mpsd / 60 # Convert to minutes for consistency
mesa_df$mpsd_di <- ifelse(mesa_df$mpsd < 30, 1, 0)

qplot(mesa_df$mpsd) + geom_vline(xintercept = 30)
sum(mesa_df$mpsd < mpsd_q1, na.rm = T)
sum(mesa_df$mpsd < 30, na.rm = T)

sum((mesa_df$mpsd < 30) != (mesa_df$mpsd <= 30), na.rm=T)
which(mesa_df$mpsd == 30)

qplot(log(mesa_df$mpsd))

mesa_df$log_mpsd <- log(mesa_df$mpsd)

## NOTE: MPSD is now in MINUTES

## NEW: 5/19/2020
# TST irregularity: dichotomize at 60, per Tianyi's paper
mesa_df$sdtst_di <- ifelse(mesa_df$sdmainsleep5 < 60, 1, 0)
qplot(mesa_df$sdtst)

# Fragmentation
frag_q1 <- favstats(mesa_df$frag)$Q1
mesa_df$frag_di <- ifelse(mesa_df$frag <= 15, 1, 0)

# SWS percentage (use NSF guidelines)
# sws_q3 <- favstats(mesa_df$sws_per)$Q3
mesa_df$swsper_di <- ifelse(mesa_df$times34p5 >= 16 & mesa_df$times34p5 <= 20, 1, 0)

# REM percentage (use NSF guidelines)
# remper_q3 <- favstats(mesa_df$rem_per)$Q3
mesa_df$remper_di <- ifelse(mesa_df$timeremp5 >= 21 & mesa_df$timeremp5 <= 30, 1, 0)

# AHI
mesa_df$ahi_di <- ifelse(mesa_df$ahi <= 15, 1, 0)

# NSF age ranges
# adult (26-64 years), and older adult (???65 years).
# to 65: 0-20
# 65+  : 0-30

# WASO waso5: PSG WASO
mesa_df$waso <- mesa_df$waso5
mesa_df$waso_di <- ifelse(mesa_df$waso <60, 1, 0) 
table(mesa_df$waso_di)

# Sleep onset latency avgonsetlatency5
mesa_df$sol <- mesa_df$avgonsetlatency5
mesa_df$sol_act_di <- ifelse(mesa_df$sol < 30, 1, 0)
table(mesa_df$sol_act_di)

## Consider difficulties initiating sleep as SOL
mesa_df$sol_sub <- as.numeric(mesa_df$trbleslpng5)
qplot(mesa_df$sol_sub)

mesa_df$sol_di <- ifelse(mesa_df$trbleslpng5 == "1: NO, NOT IN THE PAST 4 WEEKS" |
                           mesa_df$trbleslpng5 == "2: YES, LESS THAN ONCE A WEEK", 1, 0)

# typicalslp5	PAST 4 WEEKS: OVERALL TYPICAL NIGHT SLEEP	0: VERY SOUND OR RESTFUL
# 1: SOUND AND RESTFUL
# 2: AVERAGE QUALITY
# 3: RESTLESS
# 4: VERY RESTLESS
mesa_df$quality <- as.numeric(mesa_df$typicalslp5)
mesa_df$quality_di <- ifelse(mesa_df$typicalslp5 == "0: VERY SOUND OR RESTFUL" |
                               mesa_df$typicalslp5 == "1: SOUND AND RESTFUL", 1, 0)
qplot(mesa_df$quality)
qplot(mesa_df$quality_di)


##  1) code timing again or find code.
##  2) rerun PCA. send to Chandra.
##  3) Run loop. The following all goes within the loop:

## Within the loop:
# 3a) sample with replacement
# 3b) run pca
# 3c) nested models
# 3d) predicted values (empirical CI)

## PCA: bring code in from previous code (cnp). Estimate on ALL of MESA with timing. Subset to 1,736.
## nested models: run each model 0-6 (cnp). 
##                store BETAS for race in a df (colnames = iteration, black, asian, hispanic, r2, p). 
##                        ROWS: each iteration (rownames = i)
## RETURN DATAFRAME

#############################################################################################
#############################################################################################
## TIMING CODE
##  1) Make into a "circular" variable: values > 1440 minutes are recoded to start again from 0
##  2) dichotomize at 2am-4am = 1

## avgsleepmidpoint5
qplot(mesa_df$avgsleepmidpoint5)
head(as.hms(mesa_df$avgsleepmidpoint5))

mesa_df$midpoint_hm_s <- as.hms(mesa_df$avgsleepmidpoint5)
head(mesa_df$midpoint_hm_s)

# Subtract midnight and calculate in minutes
mesa_df$timing_reference <- as.hms(00:00:00)

head(mesa_df$midpoint_hm_s)
head(mesa_df$timing_reference)

head(mesa_df$midpoint_hm_s - mesa_df$timing_reference) # Correct

mesa_df$timing <- mesa_df$midpoint_hm_s - mesa_df$timing_reference

head(mesa_df$timing)
head(as.numeric(mesa_df$timing))

mesa_df$timing <- as.numeric(mesa_df$timing) / 60

qplot(mesa_df$timing) + geom_vline(xintercept = 1440, linetype = "dashed")
sum(mesa_df$timing >1440, na.rm = T)
favstats(mesa_df$timing)


mesa_df$timing_m <- ifelse(mesa_df$timing < 720, mesa_df$timing, mesa_df$timing-1440)
mesa_df$timing_m_c <- abs(mesa_df$timing_m - median(mesa_df$timing_m, na.rm=T))

qplot(mesa_df$timing_m) + geom_vline(xintercept = c(120, 240), linetype = "dashed")
sum(is.na(mesa_df$timing_m))

mesa_df$log_timing <- log(mesa_df$timing_m_c+1)

## PCA
#############################################################################################
#############################################################################################
## SHS-PC1 
pr_df <- mesa_df %>% dplyr::select(tst, sme, log_mpsd, ess, 
                                   quality, frag, sws_per, rem_per, ahi, waso, 
                                   sol, sdtst, log_timing, idno) %>%
  mutate(quality = as.numeric(quality)) %>% na.omit()
dim(pr_df)

# idno must be last in vector
pca_res <- prcomp(scale(pr_df[,1:13]*-1)) # *-1 so that higher = better
summary(pca_res)
plot(pca_res)



fviz_eig(pca_res)

fviz_pca_var(pca_res,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

str(pca_res)

# Eigenvalues
eig.val <- get_eigenvalue(pca_res)
eig.val
write.csv(eig.val, file = "eigval.csv")

# Results for Variables
res.var <- get_pca_var(pca_res)
res.var$coord          # Coordinates
res.var$contrib        # Contributions to the PCs
res.var$cos2           # Quality of representation 

res.ind <- get_pca_ind(pca_res)
res.ind$coord          # Coordinates
res.ind$contrib        # Contributions to the PCs
res.ind$cos2           # Quality of representation 

pca_df <- as.data.frame(res.ind$coord)

pca_df <- cbind(pca_df, pr_df)

var_df <- as.data.frame(res.var$coord)
head(var_df)
write.csv(var_df, file = "dims 1-3.csv")


pc1_df <- pca_df %>% dplyr::mutate(pc1_scaled = Dim.1) %>%
  dplyr::select(pc1_scaled, Dim.2, Dim.3,
                idno)

# pc1_test_df <- merge(pc1_df, mesa_df, by = "idno")

mesa_df <- merge(mesa_df, pc1_df, by = "idno")



mesa_df$pc1_scaleTEST <- scale(mesa_df$pc1_scaled, scale = T, center = T)
favstats(mesa_df$pc1_scaled ~ mesa_df$race)
boxplot(mesa_df$pc1_scaled ~ mesa_df$race)

## SHS
# timing_di <- 2 am to 4 am = 1
mesa_df$timing_di <- ifelse(mesa_df$timing_m >= 120 & mesa_df$timing_m <= 240, 1, 0)

mesa_df$shs <- with(mesa_df, tst_di + sme_di + ess_di + quality_di + mpsd_di + frag_di + swsper_di + remper_di + ahi_di +
                      waso_di + sol_di + timing_di + sdtst_di)
favstats(mesa_df$shs)
shapiro.test(mesa_df$shs)
qplot(mesa_df$shs, binwidth = 1)

summary(aov_mod_shs <- aov(mesa_df$shs ~ mesa_df$race))
TukeyHSD(aov_mod_shs)

## Events
## Events code

## Joon Chung
## jchung26@bwh.harvard.edu

# From Tianyi:
# slpexam5=stdyady5c; /*days of actigraphy relative to exam 5*/
#   if slpexam5=. then slpexam5=stdypdy5c; /*days of psg relative to exam 5*/
#     slpexam1=slpexam5+e15dyc; /*days of sleep study relative to exam 1*/
#       

library(haven)
mesa_events <- read_dta("C:/Users/jj261/Dropbox (Partners HealthCare)/MESAEvThru2018_20210518/MESAEvThru2018_20210518.dta")

mesa_df$slpexam5 <- mesa_df$stdyady5c
which(is.na(mesa_df$slpexam5))

mesa_df$slpexam5[which(is.na(mesa_df$slpexam5))] <- mesa_df$stdypdy5c[which(is.na(mesa_df$slpexam5))]
mesa_df$slpexam1 <- mesa_df$slpexam5 + mesa_df$e15dyc # Days of sleep study relative to Exam 1
# 
# ## Merge mesa_df with mesa_events
# mortality_df <- merge(mesa_df, mesa_events, by = "idno")

## Non cvd events (cancer, copd)
MESANonCVDEvThru2018_20210517 <- read_dta("C:/Users/jj261/Dropbox (Partners HealthCare)/MESANonCVDEvThru2018_20210517/MESANonCVDEvThru2018_20210517.dta")
mesa_noncvd_df <- MESANonCVDEvThru2018_20210517

mesa_cvd_events <- mesa_events %>% dplyr::select(cvda, cvdatt, idno, dth, dthtt)
mesa_noncvd_events <- mesa_noncvd_df %>% dplyr::select(cancer, cancertt, copd, copdtt, idno)
mesa_all_events <- merge(mesa_cvd_events, mesa_noncvd_events, by = "idno")

## End script
