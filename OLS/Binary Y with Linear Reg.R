update.packages()
pacman::p_load(pacman, rio, dplyr, magrittr, AER, stargazer)

data(HMDA)
head(HMDA)
summary(HMDA)
glimpse(HMDA)

# convert 'deny' to numeric
HMDA$deny <- as.numeric(HMDA$deny) - 1



############ 1 Variable X ############

# estimate a simple linear probabilty model
denymod1 <- lm(deny ~ pirat, data = HMDA)
denymod1

# plot the data
plot(x = HMDA$pirat, 
     y = HMDA$deny,
     main = "Scatterplot Mortgage Application Denial and the Payment-to-Income Ratio",
     xlab = "P/I ratio",
     ylab = "Deny",
     pch = 20,
     ylim = c(-0.4, 1.4),
     cex.main = 0.8)

# add horizontal dashed lines and text
abline(h = 1, lty = 2, col = "darkred")
abline(h = 0, lty = 2, col = "darkred")
text(2.5, 0.9, cex = 0.8, "Mortgage denied")
text(2.5, -0.1, cex= 0.8, "Mortgage approved")

# add the estimated regression line
abline(denymod1, 
       lwd = 1.8, 
       col = "steelblue")

# print robust coefficient summary (ganti se nya)
coeftest(denymod1, vcov. = vcovHC, type = "HC1") # a one percent point increase in P/I ratio leads to increase in the probability of a loan denial by 0.603535*0.01 = 0.6%





############ More than 1 Variable X ############

# rename the variable 'afam' for consistency
colnames(HMDA)[colnames(HMDA) == "afam"] <- "black"

# estimate the model
denymod2 <- lm(deny ~ pirat + black, data = HMDA)
coeftest(denymod2, vcov. = vcovHC) # being black increases the probability of a mortgage application denial by about  17.7%


