pacman::p_load(pacman, rio, dplyr, Stat2Data, skimr, tsoutliers, ggpubr, corrplot, mctest, ppcor, car, lmtest)
rm(list = ls())

data(GrinnellHouses)
df <- as_tibble(GrinnellHouses)
df <- df %>% dplyr::select(SalePrice, SPLPPct, CostPerSqFt, DaySold, SquareFeet, MonthSold)
df <- na.omit(df)
skim(df)
glimpse(df)

model <- lm(SalePrice ~ SPLPPct + CostPerSqFt + DaySold + SquareFeet + MonthSold, df)

# Durbin-Watson
dwtest <- dwtest(model)
ifelse(dwtest$p.value < 0.05, "Lolos Uji Autokorelasi", "Tidak lolos uji Autokorelasi")

# Breusch-Godfrey
bgtest <- bgtest(model)
ifelse(bgtest$p.value < 0.05, "Lolos Uji Autokorelasi", "Tidak lolos uji Autokorelasi")


# Fix Autocorrelation
model.fix1 <- lm(diff(SalePrice) ~ 
                         diff(SPLPPct) + 
                         diff(CostPerSqFt) + 
                         diff(DaySold) + 
                         diff(SquareFeet) + 
                         diff(MonthSold), df)

model.fix2 <- lm(SalePrice ~
                         SPLPPct + 
                         CostPerSqFt + 
                         DaySold + 
                         SquareFeet + 
                         MonthSold+
                         lag(SalePrice,1)+
                         lag(SPLPPct,1)+
                         lag(CostPerSqFt,1)+
                         lag(DaySold,1)+
                         lag(SquareFeet,1)+
                         lag(MonthSold,1), df)

bgtest(model)
bgtest(model.fix1)
bgtest(model.fix2)
