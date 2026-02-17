## Make empty data-frame
## age, sex
# test_df <- data.frame(pc1_scaled = c(-1, 0, 1),
#                       age5c = 68.23,
#                       female = 0.5377
#                       
#                       )
# 
# ?set_variable_labels

test_df2 <- data.frame(pc1_scaled = c(-1, 1),
                      age5c = 68.23,
                      female = 0.5377)


test_mod <- coxph(Surv(time_yr, death) ~ pc1_scaled + age5c + female, data = shs_df)
test_preds <- predict(test_mod, type = "lp")
head(exp(test_preds))

test_surv <- survfit(test_mod, newdata = test_df2)

summary(test_surv, times = c(0, 1, 2, 3, 4, 5, 6, 7))

surv_km_plot <- ggsurvplot(test_surv, shs_df,
           ylim=c(.80, 1),
           xlim=c(0, 7.5),
           xlab = "Time (years)",
           
           conf.int = T,
           legend.title = "Sleep score (PC1)",
           legend.labs = c("1 sd below mean", "1 sd above mean"),
           risk.table = T)

surv_km_plot
ggsave(file = "KM Risk Table.pdf", print(surv_km_plot))

pdf("survplot.pdf")
print(surv_km_plot, newpage = FALSE)
dev.off()
