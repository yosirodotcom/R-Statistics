pacman::p_load(pacman, dplyr, magrittr, ecm, vars, MuMIn, PerformanceAnalytics, rio)
rm(list = ls())

# Preparing data

data("edhec")
df <- edhec
n_col <- length(names(df))

names(df)[1] <- "Y"
names(df)[2] <- "X1"
names(df)[3] <- "X2"
names(df)[4] <- "X3"
names(df)[5] <- "X4"
names(df)[6] <- "X5"
names(df)[7] <- "X6"
names(df)[8] <- "X7"
names(df)[9] <- "X8"
names(df)[10] <- "X9"
names(df)[11] <- "X10"
names(df)[12] <- "X11"
names(df)[13] <- "X12"


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
