## Joon Chung
## Contact: see README
## 
## MESA Mortality with composite sleep scores
##

## Script to call tables/figures
library(tableone)


## Tables and Figures (manuscript) ####
## Table 1. Descriptive statistics x vital status ####
tableone_overall
tableone_stratified

## Table 2. Sleep characteristics x sleep categorized ####
tableone_stratified_sleep

## Table 3. Cox models (composite: score, dichotomous) ####
ShowRegTable(shs_model_0)
ShowRegTable(shs_model_1)
ShowRegTable(shs_model_2)

ShowRegTable(di_model_0)
ShowRegTable(di_model_1)
ShowRegTable(di_model_2)


## Table 4. Cox models (components) ####

## Tables and Figures (Appendix) ####

## Figure A1. Spline modeled sleep score with mortality ####

## Figure A2. Kaplan-Meier curves for dichotomous sleep scores ####

## Table B1. Exclude deaths within 1 year. ####

## Table C1. Cox models for PC1 sleep scores ####

## Table C2. Cox models for standardized metrics ####

## Table D1. Sub-score regressions