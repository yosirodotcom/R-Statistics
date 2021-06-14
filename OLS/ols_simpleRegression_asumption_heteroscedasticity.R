lmMod <- lm(dist ~ speed, data=cars)

#1. identify heteroscedasticity with graph
par(mfrow=c(2,2)) # init 4 charts in 1 panel
plot(lmMod)
##1.1. red line must be flat if there is no heteroscedasticity

#2. identify heteroscedasticity with statistical test

##2.2. Breaush Pagan Test
lmtest::bptest(lmMod)
##2.3 NCV Test
car::ncvTest(lmMod)

### H0 : variance of the residuals is constant (homoscedastic)
### p < alpha = heteroscedasticity is present
### p > alpha = heteroscedasticity is not present (pass the test)

#3. Healing heteroscedasticity with Box-Cox transformation
distBCMod <- caret::BoxCoxTrans(cars$dist)
cars <- cbind(cars, dist_new=predict(distBCMod, cars$dist))
lmMod_bc <- lm(dist_new ~ speed, data=cars) # new healing model
lmtest::bptest(lmMod_bc) # test again to make sure our new model is not heteroscedasticity
