dim(pr_df)
colnames(pr_df)

resampled_rows <- sample(1:1740, size = 1740, replace = T)
resampled_rows

resampled_df <- pr_df[resampled_rows,]

pca_res <- prcomp(scale(resampled_df[,1:11]))
summary(pca_res)
plot(pca_res)

save(pr_df, des_df, file = "pca_des_df.Rda")

library(factoextra)
fviz_eig(pca_res)

fviz_pca_var(pca_res,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
