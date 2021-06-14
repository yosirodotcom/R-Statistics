pacman::p_load(pacman, dplyr, lmtest, skimr, broom, MuMIn, strucchange)
rm(list = ls())

head(iris) # peek data
skim(iris) # explore data

model <- lm(Sepal.Length ~ ., iris)
summary(model) # Lihat F p-value
broom::glance(model)

resettest(model) # RESET TEST

options(na.action ="na.fail")
dredge(global.model = lm(Sepal.Length ~ ., 
                         data = iris)) # Based on the lowest AIC

plot(efp(Sepal.Length ~ ., 
         data=iris, 
         type = "Rec-CUSUM")) # Recursive Residual: Need the black line inside the band

sctest(Sepal.Length ~ .,  # Chow Test: need p-value > 0.05
       data=iris, 
       type = "Chow", 
       point = 10)
