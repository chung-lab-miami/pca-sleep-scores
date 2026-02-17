## Short vs normal vs long sleep
short_df <- shs_df %>% filter(tst_hr < 6)
long_df <- shs_df %>% filter(tst_hr > 8)

dim(short_df) # 513
dim(long_df)  # 188

1726 - (513 + 188)

1025/1726 # prop 6-8 hours
513/1726  # prop short
188/1726  # prop long


emptyfit <- survfit(Surv(dthtt/365 - slpexam1/365, death) ~ 1, data = shs_df)
emptyfit

empty_survplot <- ggsurvplot(emptyfit, data = shs_df,
                           risk.table = TRUE,
                           conf.int = TRUE,         # show confidence intervals for
                           # point estimaes of survival curves.
                           xlim = c(0, 8),        # present narrower X axis, but not affect
                           ylim = c(0.80, 1),
                           # survival estimates.
                           break.time.by = 1,     # break X axis in time intervals by 500.
                           ggtheme = theme_minimal(), # customize plot and risk table with a theme.
                           risk.table.y.text.col = T, # colour risk table text annotations.
                           risk.table.y.text = FALSE # show bars instead of names in text annotations
                           # in legend of risk table
)


ggpubr::ggexport(empty_survplot, 
                 filename = "empty_survplot.png",
                 width = 500,
                 height = 500,
                 pointsize = 24) 

## PC1 at means
shs_df$pc1_di <- ifelse(shs_df$pc1_scaled >0, 1, 0)
shsfit <- survfit(Surv((dthtt/365-slpexam1/365), death) ~ pc1_scaled, data = shs_df)
shsfit

shs_df$death <- as.numeric(shs_df$death)

pc_di_survplot <- ggsurvplot(shsfit, data = shs_df,
                           legend.title = "Sleep score Principal Component 1",
                           legend.labs = c("<= average sleep score", 
                                           "> average sleep score"),
                           xlab = "Years",
                           ylim = c(0.8, 1),
                           conf.int = T,
                           ggtheme = theme_light(base_size= 25))

pc_di_survplot

ggpubr::ggexport(pc_di_survplot, 
                 filename = "pc_di_survplot.png",
                 width = 1000,
                 height = 1000,
                 pointsize = 24) 

## Sleep Health Score regressions
shs_model_di <- coxph(Surv(dthtt-slpexam1, death) ~ shs_di + age5c + female + race + 
                       modvig_pa + smoke + income_tertile + 
                       degree_attain + married + aehi_10, data = shs_df)

summary(shs_model_di)


cox_predict <- predict(shsfit)
