update.packages()
pacman::p_load(pacman, rio, tidyverse, magrittr, LogisticDx, DescTools, AER, stargazer)
rm(list = ls())
theme_set(theme_bw())

# Preparing data
data(PimaIndiansDiabetes2)
df <- as_tibble(PimaIndiansDiabetes2)
df <- na.omit(df)

glimpse(df)

# Building model
model0 <- glm(diabetes ~ 1, df, family = binomial(link = "probit"))
model <- glm(diabetes ~ glucose+mass, df, family = binomial(link = "probit"))
summary(model)

# Goodness of fit tests
df <- 2 # jumlah variabel x
chi_critical <- qchisq(0.05, df, lower.tail = F)
chi_test <- -2*(logLik(model0)-logLik(model))

ifelse(chi_test > chi_critical, "Lolos Uji, model nya layak", "Tidak lolos uji, modelnya kurang layak")

# Pseudo R-squared
PseudoR2(model, which = c("all"))

# Prediction
predicted <- predict(model, df, type = "response")
predicted[1]
# atau kita masukkan ke dalam formula
pnorm(-4.877636+0.023878*89+0.042083*28.1)






################ Other Examples ################ 

############ 1 Variable X ############

data(HMDA)
head(HMDA)
summary(HMDA)
glimpse(HMDA)

# convert 'deny' to numeric
HMDA$deny <- as.numeric(HMDA$deny) - 1

# rename the variable 'afam' for consistency
colnames(HMDA)[colnames(HMDA) == "afam"] <- "black"

# estimate the simple probit model
denyprobit <- glm(deny ~ pirat, 
                  family = binomial(link = "probit"), 
                  data = HMDA)

coeftest(denyprobit, vcov. = vcovHC, type = "HC1")

# compute pseudo-R2 for the probit model
pseudoR2 <- 1 - (denyprobit$deviance) / (denyprobit$null.deviance)
pseudoR2


# plot data
plot(x = HMDA$pirat, 
     y = HMDA$deny,
     main = "Probit Model of the Probability of Denial, Given P/I Ratio",
     xlab = "P/I ratio",
     ylab = "Deny",
     pch = 20,
     ylim = c(-0.4, 1.4),
     cex.main = 0.85)

# add horizontal dashed lines and text
abline(h = 1, lty = 2, col = "darkred")
abline(h = 0, lty = 2, col = "darkred")
text(2.5, 0.9, cex = 0.8, "Mortgage denied")
text(2.5, -0.1, cex= 0.8, "Mortgage approved")

# add estimated regression line
x <- seq(0, 3, 0.01)
y <- predict(denyprobit, list(pirat = x), type = "response")

lines(x, y, lwd = 1.5, col = "steelblue")

# We use predict() to compute the predicted change in the denial probability when  P/I ratio is increased from 0.3 to 0.4

# 1. compute predictions for P/I ratio = 0.3, 0.4
predictions <- predict(denyprobit, 
                       newdata = data.frame("pirat" = c(0.3, 0.4)),
                       type = "response")

# 2. Compute difference in probabilities
diff(predictions) # We find that an increase in the payment-to-income ratio from 0.3 to 0.4 is predicted to increase the probability of denial by approximately 6.2%.



############ More than 1 Variable X ############

denyprobit2 <- glm(deny ~ pirat + black, 
                   family = binomial(link = "probit"), 
                   data = HMDA)

coeftest(denyprobit2, vcov. = vcovHC, type = "HC1")

# 1. compute predictions for P/I ratio = 0.3
predictions <- predict(denyprobit2, 
                       newdata = data.frame("black" = c("no", "yes"), 
                                            "pirat" = c(0.3, 0.3)),
                       type = "response")

# 2. compute difference in probabilities
diff(predictions) # In this case, the estimated difference in denial probabilities is about 15.8%.

# compute pseudo-R2 for the probit model
pseudoR2 <- 1 - (denyprobit2$deviance) / (denyprobit2$null.deviance)
pseudoR2
