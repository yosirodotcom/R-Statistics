update.packages()
pacman::p_load(pacman, 
               tidyverse, 
               magrittr, 
               ecm, 
               vars, 
               MuMIn, 
               PerformanceAnalytics, 
               rio,
               AER,
               dynlm,
               stargazer,
               scales,
               quantmod,
               urca,
               readxl,
               forecast)
rm(list = ls())
theme_set(theme_bw())

# Preparing data

data("edhec")
df <- edhec

ncol <- length(names(df))

names(df)<-sprintf("x%d",0:ncol)
colnames(df)[1] <- "y"


# Finding optimum lag for AR
chart.ACFplus(df$Y, maxlag = 12) # with chart PACF


lag_select <- VARselect(df$Y, lag.max = 15, type = "const") # with combination model
lag_select$selection


# Build AR (Autoregressive) model
## Cara 1
model1 <- lm(Y ~ lag(Y,1)+
                     lag(Y,2), df)
summary(model1)

## Cara 2
model2 <- ar(df$Y, F, 2)
print(model2)


# Uji Autokorelasi
## Durbin h (hanya untuk AR1 model)

varlag <- lag(as.vector(df$Y), 1)
model3 <- lm(df$Y ~ varlag)
test.durbin.h <- durbinH(model3, "varlag")
test.durbin.h[1]
ifelse(test.durbin.h[1] > -1.96 & test.durbin.h[1] < 1.96, "Lolos Uji, Autokorelasi tidak terdeteksi", "Tidak lolos uji, Autokorelasi terdeteksi")


## lagrange multiplier

x <- bgtest(model1, order = 2) # jumlah IV yang optimal adalah 2, berdasarkan kriteria AIC di atas
print(x)
x$coefficients
x$p.value

if(as.vector(x$p.value) > 0.05){
        print("Tidak ada masalah autokorelasi berdasarkan Lagrange Multiplier Breusch-Godfrey")
        
}else{
        print("Ada masalah autokorelasi berdasarkan Lagrange Multiplier Breusch-Godfrey")
}















################# Other Examples ################# 
# setwd("D:/Cloud Data/Dropbox/R Programming/R-Statistics/Time Series")
# # subset data
# 
# # load US macroeconomic data
# USMacroSWQ <- read_xlsx("us_macro_quarterly.xlsx",
#                         sheet = 1,
#                         col_types = c("text", rep("numeric", 9)))
# 
# # format date column
# USMacroSWQ$...1 <- as.yearqtr(USMacroSWQ$...1, format = "%Y:0%q")
# 
# # adjust column names
# colnames(USMacroSWQ) <- c("Date", "GDPC96", "JAPAN_IP", "PCECTPI", 
#                           "GS10", "GS1", "TB3MS", "UNRATE", "EXUSUK", "CPIAUCSL")
# 
# # GDP series as xts object
# GDP <- xts(USMacroSWQ$GDPC96, USMacroSWQ$Date)["1960::2013"]
# 
# # GDP growth series as xts object
# options(na.f)
# GDPGrowth <- xts(400 * log(GDP/stats::lag(GDP)))
# 
# GDPGRSub <- GDPGrowth["1962::2012"]
# 
# # reproduce Figure 14.1 (a) of the book
# plot(log(as.zoo(GDP)),
#      col = "steelblue",
#      lwd = 2,
#      ylab = "Logarithm",
#      xlab = "Date",
#      main = "U.S. Quarterly Real GDP")
# 
# # reproduce Figure 14.1 (b) of the book
# plot(as.zoo(GDPGrowth),
#      col = "steelblue",
#      lwd = 2,
#      ylab = "Logarithm",
#      xlab = "Date",
#      main = "U.S. Real GDP Growth Rates")



#｡☆✼★━━━━━━━━━━━━★✼☆｡ AR (1) ｡☆✼★━━━━━━━━━━━━★✼☆｡

ar.ols(GDPGRSub, 
       order.max = 1, 
       demean = F, 
       intercept = T)
#atau
# length of data set
N <-length(GDPGRSub)

GDPGR_level <- as.numeric(GDPGRSub[-1])
GDPGR_lags <- as.numeric(GDPGRSub[-N])

armod <- lm(GDPGR_level ~ GDPGR_lags)
coeftest(armod, vcov. = vcovHC, type = "HC1")

# FORECASTING
# assign GDP growth rate in 2012:Q4
new <- data.frame("GDPGR_lags" = GDPGR_level[N-1])

# forecast GDP growth rate in 2013:Q1
forecast(armod, newdata = new)

# compute the forecast error
forecast(armod, newdata = new)$mean - GDPGrowth["2013"][1] #error rate nya terlalu besar

# R^2
summary(armod)$r.squared

# SER
summary(armod)$sigma


#｡☆✼★━━━━━━━━━━━━★✼☆｡ AR (2) ｡☆✼★━━━━━━━━━━━━★✼☆｡

GDPGR_AR2 <- dynlm(ts(GDPGR_level) ~ L(ts(GDPGR_level)) + L(ts(GDPGR_level), 2))
coeftest(GDPGR_AR2, vcov. = sandwich)

# R^2
summary(GDPGR_AR2)$r.squared

# AR(2) forecast of GDP growth in 2013:Q1 
forecast <- c("2013:Q1" = coef(GDPGR_AR2) %*% c(1, GDPGR_level[N-1], GDPGR_level[N-2]))

# compute AR(2) forecast error 
GDPGrowth["2013"][1] - forecast
