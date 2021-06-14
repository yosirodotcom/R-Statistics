p_load(ggpubr, lmtest)
rm(list = ls())


data(iris3)
y <- iris3[,1,1]
x1 <- iris3[,1,2]
x2 <- iris3[,1,3]
x3 <- iris3[,2,1]
x4 <- iris3[,2,2]
x5 <- iris3[,2,3]
x6 <- iris3[,3,3]



model <- lm(as.vector(y ~ x1+x2+x3+x4+x5+x6))

plot(model, 1)

resettest(model, power=2:3, type="princomp", data=iris3)

