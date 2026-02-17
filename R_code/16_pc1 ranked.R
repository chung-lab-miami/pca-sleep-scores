## Rank order by PC score
shs_df$pc1_rank <- rank(shs_df$pc1_scaled)/nrow(shs_df)


shs_df$pc1_rank_norm <- rank.normalize(shs_df$pc1_scaled)/nrow(shs_df)



head(shs_df$pc1_rank)
head(shs_df$pc1_rank_norm)

## Sleep Health Score regressions BMI + prevalent disease adjusted
rank_model_2 <- coxph(Surv(dthtt-slpexam1, death) ~ pc1_rank + age5c + female + race + 
                       modvig_pa + smoke + income_tertile + 
                       degree_attain + married + aehi_10 , 
                      data = shs_df)

summary(rank_model_2)
ShowRegTable(rank_model_2)

boxplot(shs_df$pc1_rank_norm ~ shs_df$death)

## Example
not.normal <- rlnorm(100)
hist(not.normal)
hist(rank.normalize(not.normal))

# Different function
hist(rank.normalize(not.normal, FUN = qexp))

# Breaking ties
hist(rank.normalize(not.normal, ties.method = "random"))

# Missing values
not.normal[10] <- NA
hist(rank.normalize(not.normal)) # Shouldn't fail
## Not run: hist(rank.normalize(not.normal, na.action = na.fail)) # Should fail