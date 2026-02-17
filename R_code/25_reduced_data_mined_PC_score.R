## PCA
#############################################################################################
#############################################################################################
## SHS-PC1 
pr_df_reduced <- mesa_df %>% dplyr::select(tst, sme, log_mpsd, 
                                   frag, ahi, waso, 
                                   sdtst, idno) %>% na.omit()
dim(pr_df_reduced)

# idno must be last in vector
pca_res <- prcomp(scale(pr_df_reduced[,1:7]*-1)) # *-1 so that higher = better
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
# write.csv(eig.val, file = "eigval_reducedPC.csv")

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
# write.csv(var_df, file = "dims for pc_reduced.csv")


pc1_df <- pca_df %>% dplyr::mutate(pc1_scaled_reduced = Dim.1*-1) %>%
  dplyr::select(pc1_scaled_reduced, Dim.2, Dim.3,
                idno)

shs_reduced_df <- merge(shs_df, pc1_df, by="idno")

shs_model_1 <- coxph(Surv(dthtt-slpexam1, death) ~ pc1_scaled_reduced + Dim.2 + Dim.3 + + age5c + female + race + 
                       modvig_pa + smoke + income_tertile + 
                       degree_attain + married + aehi_10, data = shs_reduced_df)

summary(shs_model_1)
ShowRegTable(shs_model_1)
