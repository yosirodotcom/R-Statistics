pacman::p_load(pacman, rio, dplyr, Stat2Data, skimr, tsoutliers, ggpubr, corrplot, mctest, ppcor, car)
rm(list = ls())

data(GrinnellHouses)
df <- as_tibble(GrinnellHouses)
df <- df %>% dplyr::select(SalePrice, SPLPPct, CostPerSqFt, DaySold, SquareFeet, MonthSold)
df <- na.omit(df)
skim(df)
glimpse(df)

model <- lm(SalePrice ~ SPLPPct + CostPerSqFt + DaySold + SquareFeet + MonthSold, df)


#1. Detect multicolliniearity 
##1.1. with graph

corrplot(cor(df[,-1])) 

##1.2. Detect multicolliniearity with Farrar – Glauber Test
mctest::omcdiag(model) # secara umum ada multicoliniearity antar variabel, tapi belum tahu variabel mana
mctest::imcdiag(model) # ada masalah multikol jika nilai VIF nya yang besar

##1.3. Detect multicolliniearity with t-test)
ppcor::pcor(df[,-1], method = "pearson") # fokus pada nilai variabel yang dicurigai, pada $statistic nilai yang besar menunjukkan adanya masalah multikol antar 2 variabel. (Di atas 1000)

##1.4. Detect multicolliniearity with VIF 
car::vif(model) # nilai VIF masih terlalu besar >10 pada semua variabel

##1.5. Regresi auxilary

aux.X1 <- lm(SPLPPct ~ CostPerSqFt+DaySold+SquareFeet+MonthSold, data = df) # buat model
aux.X2 <- lm(CostPerSqFt ~ SPLPPct+DaySold+SquareFeet+MonthSold, data = df)
aux.X3 <- lm(DaySold ~ SPLPPct+CostPerSqFt+SquareFeet+MonthSold, data = df)
aux.X4 <- lm(SquareFeet ~ SPLPPct+CostPerSqFt+DaySold+MonthSold, data = df)
aux.X5 <- lm(MonthSold ~ SPLPPct+CostPerSqFt+DaySold+SquareFeet, data = df)

r2.aux.x1 <- summary(aux.X1)$r.squared # ambil nilai r2
r2.aux.x2 <- summary(aux.X2)$r.squared
r2.aux.x3 <- summary(aux.X3)$r.squared
r2.aux.x4 <- summary(aux.X4)$r.squared
r2.aux.x5 <- summary(aux.X4)$r.squared

n.aux <- length(df$SalePrice) # ambil nilai n (jumlah sampel)
k.aux <- 6

F.X1 <- (r2.aux.x1/(k.aux-1))/((1-r2.aux.x1)/(n.aux-k.aux)) # ambil F hitung
F.X2 <- (r2.aux.x2/(k.aux-1))/((1-r2.aux.x2)/(n.aux-k.aux))
F.X3 <- (r2.aux.x3/(k.aux-1))/((1-r2.aux.x3)/(n.aux-k.aux))
F.X4 <- (r2.aux.x4/(k.aux-1))/((1-r2.aux.x4)/(n.aux-k.aux))
F.aux.kritis <- qf(.95, df1=k.aux-1, df2=n.aux-k.aux) # ambil F kritis

if(F.X1 > F.aux.kritis){
        print("Model X1 mengandung masalah multikolinearitas")
}else{
        print("Model X1 tidak ada masalah multikolinieritas")
}
if(F.X2 > F.aux.kritis){
        print("Model X2 mengandung masalah multikolinearitas")
}else{
        print("Model X2 tidak ada masalah multikolinieritas")
}
if(F.X3 > F.aux.kritis){
        print("Model X3 mengandung masalah multikolinearitas")
}else{
        print("Model X3 tidak ada masalah multikolinieritas")
}
if(F.X4 > F.aux.kritis){
        print("Model X4 mengandung masalah multikolinearitas")
}else{
        print("Model X4 tidak ada masalah multikolinieritas")
}
if(F.X5 > F.aux.kritis){
        print("Model X5 mengandung masalah multikolinearitas")
}else{
        print("Model X5 tidak ada masalah multikolinieritas")
}

#1.6 Deteksi klien


if(r2.aux.x1 < summary(model)$r.squared){
        print("Model X1 mengandung masalah multikolinearitas")
}else{
        print("Model X1 tidak ada masalah multikolinieritas")
}
if(r2.aux.x2 < summary(model)$r.squared){
        print("Model X2 mengandung masalah multikolinearitas")
}else{
        print("Model X2 tidak ada masalah multikolinieritas")
}
if(r2.aux.x3 < summary(model)$r.squared){
        print("Model X3 mengandung masalah multikolinearitas")
}else{
        print("Model X3 tidak ada masalah multikolinieritas")
}
if(r2.aux.x4 < summary(model)$r.squared){
        print("Model X4 mengandung masalah multikolinearitas")
}else{
        print("Model X4 tidak ada masalah multikolinieritas")
}
if(r2.aux.x5 < summary(model)$r.squared){
        print("Model X5 mengandung masalah multikolinearitas")
}else{
        print("Model X5 tidak ada masalah multikolinieritas")
}

#2. Healing Multicolliniearity

## Remove problematic variabel, in this case is EXPERIENCE
model.fix <- lm(SalePrice ~ SPLPPct + DaySold + SquareFeet + MonthSold, df)
car::vif(model.fix) # nilai VIF sudah normal untuk setiap variabel
corrplot(cor(df[,c(-1,-3)])) 
