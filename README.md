# PCA Sleep Health Scores & All-Cause Mortality in MESA

A data-driven composite sleep health score (PCA on 13 multimodal sleep indicators) that outperforms individual sleep metrics in predicting all-cause mortality.

**[View the Interactive Code Review & Portfolio Page](https://chung-lab-miami.github.io/pca-sleep-scores/)**

## Key Findings

- PCA-derived PC1 (19.75% variance) achieves **highest C-statistic (0.617)** among all 14 sleep metrics tested
- **25% lower mortality per SD** (HR = 0.75, 95% CI: 0.65–0.87)
- Outperforms both the raw sum score (SHS: C = 0.586) and every individual metric
- Proportional hazards assumption met (global p = 0.272); linear dose-response confirmed

## Technical Skills Demonstrated

| Skill | Methods |
|-------|---------|
| Dimensionality Reduction | PCA on 13 multimodal features, bootstrap validation, reduced-variable sensitivity |
| Survival Analysis | Cox PH (nested models), penalized splines, age-as-time-metric, reverse causation |
| Predictive Model Evaluation | C-statistics, time-dependent AUC (1-8 yr), Cox R², IPA, LR tests |
| Feature Engineering | 13 sleep indicators (actigraphy + PSG + survey), AEHI-10 dietary composite |
| Programmatic Modeling | Loop-based Cox over 27 predictors, automated forest plot generation |
| Data Engineering | 26-script pipeline, 6 MESA exam waves + PSG + actigraphy + diet + events |

## Repository Structure

```
├── index.html              # Interactive code review & portfolio page
├── R_code/                 # 28 R scripts
│   ├── 01-04               # Data import, sleep variables, PCA, diet, dataset assembly
│   ├── 05-06               # Cox PH models, individual metric regressions
│   ├── 07-14               # PSM, tables, figures, sensitivity
│   ├── 15-22               # C-statistics, AUC, R², LR tests, random forests
│   ├── 23-26               # Additional sensitivity & review responses
│   ├── Chung - MESA sleep scores (PCA).R    # Standalone PCA extraction
│   └── 22 - MESA PCA bootstrap validation.R # Bootstrap stability test
└── README.md
```

## Publications

**Primary:** Chung J, Goodman M, Huang T, Wallace ML, Lutsey PL, Chen JT, Castro-Diehl C, Bertisch S, Redline S. Multi-dimensional sleep and mortality: The Multi-Ethnic Study of Atherosclerosis. *SLEEP*. 2023;46(9):zsad048. [doi:10.1093/sleep/zsad048](https://doi.org/10.1093/sleep/zsad048)

**Conceptual framework:** Chung J, Goodman M, Huang T, Bertisch S, Redline S. Multidimensional sleep health in a diverse, aging adult cohort. *Sleep Health*. 2021;7(6):699–707. [doi:10.1016/j.sleh.2021.08.005](https://doi.org/10.1016/j.sleh.2021.08.005)

**Racial disparities:** Chung J, Goodman M, Huang T, Wallace ML, Johnson DA, Bertisch S, Redline S. Racial/Ethnic Differences in Indicators of Healthy Sleep. *Am J Epidemiol*. 2024;193(1):107–120. [doi:10.1093/aje/kwab232](https://doi.org/10.1093/aje/kwab232)

## Author

**Joon Chung, PhD**
University of Miami, Miller School of Medicine
*Originally conducted at Brigham and Women's Hospital / Harvard Medical School (2021-2022)*
