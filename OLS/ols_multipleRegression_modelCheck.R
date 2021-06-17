pacman::p_load(pacman, dplyr, lmtest, skimr, broom, MuMIn, strucchange)
rm(list = ls())

head(iris) # peek data
skim(iris) # explore data

model <- lm(Sepal.Length ~ ., iris)
summary(model) # Lihat F p-value
broom::glance(model)

resettest(model) # RESET TEST need p-value > 0.05
#If the null-hypothesis is rejected, then the model suffers from misspecification.

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
#If we reject the null hypothesis, we have sufficient evidence to say that there is a structural break point in the data and two regression lines can fit the data better than one.

#If we fail to reject the null hypothesis, we do not have sufficient evidence to say that there is a structural break point in the data.