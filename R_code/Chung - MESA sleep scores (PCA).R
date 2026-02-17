## Joon Chung
## Contact: see README

## Principal Components sleep scores in MESA

## Subset
pr_df <- mesa_df %>% dplyr::select(tst, sme, log_mpsd, ess, 
                                   quality, frag, sws_per, rem_per, ahi, waso, 
                                   sol, sdtst, log_timing, idno) %>%
  mutate(quality = as.numeric(quality)) %>% na.omit()
dim(pr_df)

# idno must be last in vector
pca_res <- prcomp(scale(pr_df[,1:13]*-1)) # *-1 so that higher = better
summary(pca_res)
plot(pca_res)

# Visualizations
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

# Results for Variables
res.var <- get_pca_var(pca_res)
res.var$coord          # Coordinates
res.var$contrib        # Contributions to the PCs
res.var$cos2           # Quality of representation 

res.ind <- get_pca_ind(pca_res)
res.ind$coord          # Coordinates
res.ind$contrib        # Contributions to the PCs
res.ind$cos2           # Quality of representation 

# Individual coordinates
pca_df <- as.data.frame(res.ind$coord)

pca_df <- cbind(pca_df, pr_df)

pc1_df <- pca_df %>% dplyr::mutate(pc1_scaled = Dim.1) %>%
  dplyr::select(pc1_scaled, Dim.2, Dim.3,
                idno)

mesa_df <- merge(mesa_df, pc1_df, by = "idno") # Merge with analytic dataset

mesa_df$pc1_scaled.x[shs_df$idno == 3010023]


