pacman::p_load(pacman, rio, dplyr, Stat2Data, skimr, tsoutliers, ggpubr)
rm(list = ls())


df <- as_tibble(GrinnellHouses)
skim(df)
glimpse(df)

model <- lm(SalePrice ~ SPLPPct + CostPerSqFt + DaySold, df)
resid <- model$residuals

#1. Detect normality in residuals
##1.1. With graph
ggqqplot(resid)
ols_plot_resid_qq(model)


#1.2. Shapiro-Wilk’s test
s <- shapiro.test(resid) # need p > 0.05
ifelse(s$p.value>0.05, "Lolos Uji Normalitas pada residual", "Tidak lolos Uji Normalitas pada residual")

#1.3. Kolmogorov-Smirnov test
k <- ks.test(resid, "pnorm", mean=mean(resid), sd=sd(resid))
ifelse(k$p.value>0.05, "Lolos Uji Normalitas pada residual", "Tidak lolos Uji Normalitas pada residual")

#1.4. Jarque-Bera
j <- JarqueBera.test(model$residuals) # need p > 0.05
ifelse(as.vector(j[[1]][3]$p.value)>0.05, "Lolos Uji Normalitas pada residual", "Tidak lolos Uji Normalitas pada residual")


ols_test_normality(model)
