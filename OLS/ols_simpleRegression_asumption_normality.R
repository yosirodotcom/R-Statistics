p_load(tsoutliers, ggpubr)
rm(list = ls())

data(iris3)

model <- lm(as.vector(iris3[,2,3]) ~ as.vector(iris3[,3,3]))

#1. Detect normality in residuals
##1.1. With graph
ggqqplot(model$residuals)


#1.2. With statistical test: Shapiro-Wilk’s test
shapiro.test(model$residuals) # need p > 0.05

#1.3. With statistical test: Jarque-Bera
tsoutliers::JarqueBera.test(model$residuals) # need p > 0.05


