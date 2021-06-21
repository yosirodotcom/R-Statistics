pacman::p_load(pacman, rio, dplyr, Stat2Data, skimr, tsoutliers, ggpubr, corrplot, mctest, ppcor, car, lmtest, broom)
rm(list = ls())

data(GrinnellHouses)
df <- as_tibble(GrinnellHouses)
df <- df %>% dplyr::select(SalePrice, SPLPPct, CostPerSqFt, DaySold, SquareFeet, MonthSold)
df <- na.omit(df)
skim(df)
glimpse(df)

model <- lm(SalePrice ~ SPLPPct + CostPerSqFt + DaySold + SquareFeet + MonthSold, df)
summary(model)
glance(model)



# RESET Test

reset.test <- resettest(model)
ifelse(reset.test$p.value > 0.05, "Lolos uji kesalahan bentuk spesifikasi model", "Tidak lolos uji kesalahan bentuk spesifikasi model")
# RESET TEST need p-value > 0.05
#If the null-hypothesis is rejected, then the model suffers from misspecification.

# Recursive Residual 
# https://rdrr.io/cran/strucchange/man/recresid.html

plot(efp(SalePrice ~ ., 
         data=df, 
         type = "Rec-CUSUM")) # Recursive Residual: Need the black line inside the band


# Chow Test

chow <- sctest(SalePrice ~ ., 
       data=df, 
       type = "Chow", 
       point = 10)

ifelse(chow$p.value < 0.05, "Tidak lolos uji struktur model pada Chow Test", "lolos uji struktur model pada Chow Test")

#If we reject the null hypothesis, we have sufficient evidence to say that there is a structural break point in the data and two regression lines can fit the data better than one.

#If we fail to reject the null hypothesis, we do not have sufficient evidence to say that there is a structural break point in the data.






