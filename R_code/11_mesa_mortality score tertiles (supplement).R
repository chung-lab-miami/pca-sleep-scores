
vTert_pc = quantile(shs_df$pc1_scaled, c(0:3/3))

# classify values
shs_df$pc1_tertiles = with(shs_df, 
                                     cut(pc1_scaled, 
                                         vTert_pc, 
                                         include.lowest = T, 
                                         labels = c("Low", "Medium", "High")))

## Tableone sleep values by PC1 tertiles


# Visualize by tertiles

ggplot(shs_df, aes(x = pc1_tertiles, y = tst_hr, fill = pc1_tertiles)) + 
  geom_violin() + 
  geom_boxplot(width=0.3, fill = "white") + 
  theme_classic(base_size = 15)+ 
  geom_hline(yintercept = 6, linetype  = "dashed") +
  geom_hline(yintercept = 8, linetype  = "dashed") +
  xlab("PC1 tertiles") +
  ylab("Total Sleep Time (hours)") +
  stat_summary(fun=mean, geom="point", shape=20, size=3, color="red", fill="red") +
  guides(fill=guide_legend(title="PC1 tertiles"))

ggplot(shs_df, aes(x = pc1_tertiles, y = mpsd, fill = pc1_tertiles)) + geom_violin() + 
  geom_boxplot(width=0.3, fill="white") + 
  theme_classic(base_size = 15)+ 
  geom_hline(yintercept = 30, linetype  = "dashed") +
  xlab("PC1 tertiles") +
  ylab("Sleep timing regularity (sd; minutes)") +
  stat_summary(fun=mean, geom="point", shape=20, size=3, color="red", fill="red") +
  guides(fill=guide_legend(title="Tertiles"))

ggplot(shs_df, aes(x = pc1_tertiles, y = sdtst, fill = pc1_tertiles)) + geom_violin() + 
  geom_boxplot(width=0.3, fill = "white") + 
  theme_classic(base_size = 15)+ 
  geom_hline(yintercept = 90, linetype  = "dashed") +
  xlab("Sleep clusters") +
  ylab("Sleep duration regularity (sd; minutes)") +
  stat_summary(fun=mean, geom="point", shape=20, size=3, color="red", fill="red") +
  guides(fill=guide_legend(title="Sleep clusters"))

######################## SLEEP ARCHITECTURE ############################
ggplot(shs_df, aes(x = pc1_tertiles, y = rem_per)) + geom_violin() + geom_boxplot(width=0.3)  + 
  theme_classic()+ 
  xlab("Sleep schedule clusters") +
  ylab("% Rapid eye movement sleep")+
  stat_summary(fun=mean, geom="point", shape=20, size=3, color="red", fill="red") +
  guides(fill=guide_legend(title="Sleep clusters"))


ggplot(shs_df, aes(x = pc1_tertiles, y = sws_per)) + geom_violin() + geom_boxplot(width=0.3)  + 
  theme_classic()+ 
  xlab("Sleep schedule clusters") +
  ylab("% N3 sleep")+
  stat_summary(fun=mean, geom="point", shape=20, size=3, color="red", fill="red") +
  guides(fill=guide_legend(title="Sleep clusters"))


ggplot(shs_df, aes(x = pc1_tertiles, y = ahi, fill = pc1_tertiles)) + geom_violin() + 
  geom_boxplot(width=0.3, fill = "white")  + 
  theme_classic(base_size = 15)+ 
  xlab("Sleep schedule clusters") +
  stat_summary(fun=mean, geom="point", shape=20, size=3, color="red", fill="red") +
  guides(fill=guide_legend(title="Sleep clusters")) + 
  ylab("The Apnea-Hypopnea Index (events/hr)") +
  geom_hline(yintercept = 15, linetype = "dashed")


ggplot(shs_df, aes(x = pc1_tertiles, y = frag)) + geom_violin() + geom_boxplot(width=0.3)  + 
  theme_classic()+ 
  xlab("Sleep schedule clusters") +
  stat_summary(fun=mean, geom="point", shape=20, size=3, color="red", fill="red") +
  guides(fill=guide_legend(title="Sleep clusters"))



ggplot(shs_df, aes(x = pc1_tertiles, y = waso)) + geom_violin() + geom_boxplot(width=0.3)  + 
  theme_classic()+ 
  xlab("Sleep schedule clusters") +
  stat_summary(fun=mean, geom="point", shape=20, size=3, color="red", fill="red") +
  guides(fill=guide_legend(title="Sleep clusters"))


ggplot(shs_df, aes(x = pc1_tertiles, y = sme)) + geom_violin() + geom_boxplot(width=0.3)  + 
  theme_classic()+ 
  xlab("Sleep schedule clusters") +
  stat_summary(fun=mean, geom="point", shape=20, size=3, color="red", fill="red") +
  guides(fill=guide_legend(title="Sleep clusters"))



ggplot(shs_df, aes(x = pc1_tertiles, y = quality)) + geom_violin() + geom_boxplot(width=0.3)  + 
  theme_classic()+ 
  xlab("Sleep schedule clusters") +
  stat_summary(fun=mean, geom="point", shape=20, size=3, color="red", fill="red") +
  guides(fill=guide_legend(title="Sleep clusters"))



ggplot(shs_df, aes(x = pc1_tertiles, y = sol)) + geom_violin() + geom_boxplot(width=0.3)  + 
  theme_classic()+ 
  xlab("Sleep schedule clusters") +
  stat_summary(fun=mean, geom="point", shape=20, size=3, color="red", fill="red") +
  guides(fill=guide_legend(title="Sleep clusters"))



ggplot(shs_df, aes(x = pc1_tertiles, y = log_timing)) + geom_violin() + geom_boxplot(width=0.3)  + 
  theme_classic()+ 
  xlab("Sleep schedule clusters") +
  stat_summary(fun=mean, geom="point", shape=20, size=3, color="red", fill="red") +
  guides(fill=guide_legend(title="Sleep clusters"))

