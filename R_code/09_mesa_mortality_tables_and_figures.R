## Joon Chung
## Contact: see README
## 1/17/2022

## Tables and Figures to report

## Table 1: Descriptive statistics in the Multi-Ethnic Study of Athero (n=1,726)
mesa_mortality_tableone_overall

shs_df_summary <- shs_df %>% tbl_summary(type = list(c(shs_scaled) ~ "continuous"))

## Table 2: Cox model results
ShowRegTable(shs_model_0)  # Socio-demographics
ShowRegTable(shs_model_1)  # Lifestyle factors
ShowRegTable(shs_model_2)  # BMI and prevalent CVD, COPD, cancer (sensitivity)
ShowRegTable(sens_shs_1yr) # Excluding deaths w/in 1 year of sleep exam (sensitivity)

shs_test     # Test of Schoenfeld resids

ShowRegTable(di_model_0)  # Socio-demographics
ShowRegTable(di_model_1)  # Lifestyle factors
ShowRegTable(di_model_2)  # BMI and prevalent CVD, COPD, cancer (sensitivity)
ShowRegTable(sens_di_1yr) # Excluding deaths w/in 1 year of sleep exam (sensitivity) ** REDO

pc1_test     # Test of Schoenfeld resids

## Figure 1a: Sleep variables at cutpoints
primary_report_table_di

## Figure 1b: Sleep variables (standardized)
primary_report_table_standard

## Figure 2a: Sleep health score (spline modeled)
shs_model_1_spline
ragg::agg_tiff(paste0("Sleep health score (spline modeled)", ".tiff"), width = 5, height = 5, units = "in", res = 300)
termplot(shs_model_1_spline, term=1, se=TRUE, col.term=1, col.se=1, rug = T, 
         xlab = "Sleep score (summary)",
         ylab = "Log hazard, df=3")
dev.off()


termplot(shs_model_1_spline, term=1, se=TRUE, col.term=1, col.se=1, rug = T, xlab = "Sleep score (summary)")

## Figure 2b: Sleep health score PC1 (spline modeled)
pc1_model_1_spline

ragg::agg_tiff(paste0("Sleep health score PC1 (spline modeled)", ".tiff"), width = 5, height = 5, units = "in", res = 300)
termplot(pc1_model_1_spline, term=1, se=TRUE, col.term=1, col.se=1, rug = T, 
         xlab = "Sleep score (PC1)",
         ylab = "Log hazard, df=3")
dev.off()

## Individual spline-modeled looped, in dropbox
## Figure 2c: Total sleep time (spline modeled)
## Figure 2d: Midpoint irregularity (log; spline modeled)
## Figure 2e: Duration irregularity (spline modeled)
## Figure 2f: The Apnea-Hypopnea Index (spline modeled)


#### SUPPLEMENT TABLES AND FIGURES
## Figure S1: Sleep metrics by sleep score tertiles

## Figure S2a: % R (spline modeled)

## Figure S2b: WASO (spline modeled)

## Table S1: Sleep score (raw) 

## Table 



