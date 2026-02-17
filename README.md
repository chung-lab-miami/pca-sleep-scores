# PCA Sleep Health Scores & All-Cause Mortality in MESA

A data-driven composite sleep health score (PCA on 13 multimodal sleep indicators) that outperforms individual sleep metrics in predicting all-cause mortality.

**[View the Interactive Code Review & Portfolio Page](https://chung-lab-miami.github.io/pca-sleep-scores/)**

## Key Findings

- PCA-derived PC1 achieves **highest C-statistic (0.617)** among all 14 sleep metrics tested
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

## Publication

Chung J, et al. (2022). Multidimensional sleep health and all-cause mortality in the Multi-Ethnic Study of Atherosclerosis. *Sleep*.

## Author

**Joon Chung, PhD**
University of Miami, Miller School of Medicine
*Originally conducted at Brigham and Women's Hospital / Harvard Medical School (2021-2022)*
