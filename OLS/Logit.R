update.packages()
pacman::p_load(pacman, rio, tidyverse, caret, skimr, magrittr, mlbench, DescTools, AER, stargazer)
rm(list = ls())
theme_set(theme_bw())

# Preparing data
data(PimaIndiansDiabetes2)
df <- as_tibble(PimaIndiansDiabetes2)
df <- na.omit(df)

# Inspect data
skim(df)
glimpse(df)
sample_n(df, 3)


# Split the data into training and test set
set.seed(123)
training.samples <- createDataPartition(df$diabetes, p = 0.8, list = FALSE)
train.data  <- df[training.samples, ]
test.data <- df[-training.samples, ]


# Logistic
model0 <- glm(diabetes ~ 1, data = train.data, family = binomial(link = "logit"))
model <- glm(diabetes ~ glucose+mass+pedigree, data = train.data, family = binomial(link = "logit"))
summary(model)
## formula: p =exp(-9.5 + 0.04*glucose + 0.08*mass + 1.28*pedigree)/(1 + exp(-9.5 + 0.04*glucose + 0.08*mass + 1.28*pedigree))

# Goodness of fit tests
df <- 2 # jumlah variabel x
chi_critical <- qchisq(0.05, df, lower.tail = F)
chi_test <- -2*(logLik(model0)-logLik(model))

ifelse(chi_test > chi_critical, "Lolos Uji, model nya layak", "Tidak lolos uji, modelnya kurang layak")

# Pseudo R-squared
PseudoR2(model, which = c("all"))


# Make Predictions
prob <- model %>% predict(test.data, type = "response")
prob

predicted.classes <- ifelse(prob > 0.5, "pos", "neg")
predicted.classes

# Model Accuracy
mean(predicted.classes == test.data$diabetes)








# Simple Logistic Regression
model <- glm( diabetes ~ glucose, data = train.data, family = binomial(link = "logit"))
summary(model)$coef

newdata <- df[,"glucose"]
probabilities <- model %>% predict(newdata, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")
predicted.classes

train.data %>%
        mutate(prob = ifelse(diabetes == "pos", 1, 0)) %>%
        ggplot(aes(glucose, prob)) +
        geom_point(alpha = 0.2) +
        geom_smooth(method = "glm", method.args = list(family = "binomial")) +
        labs(
                title = "Logistic Regression Model", 
                x = "Plasma Glucose Concentration",
                y = "Probability of being diabete-pos"
        )


###################

eko <- import(file = "clipboard")
fit0 <- glm(Y ~ 1, eko, family = binomial(link = "probit"))
fit <- glm(Y ~., eko, family = binomial(link = "probit"))
summary(fit0)






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

# estimate the simple logit model
denylogit <- glm(deny ~ pirat, 
                 family = binomial(link = "logit"), 
                 data = HMDA)

coeftest(denylogit, vcov. = vcovHC, type = "HC1")

# compute pseudo-R2 for the logit model
pseudoR2 <- 1 - (denylogit$deviance) / (denylogit$null.deviance)
pseudoR2


# plot data
plot(x = HMDA$pirat, 
     y = HMDA$deny,
     main = "Probit and Logit Models Model of the Probability of Denial, Given P/I Ratio",
     xlab = "P/I ratio",
     ylab = "Deny",
     pch = 20,
     ylim = c(-0.4, 1.4),
     cex.main = 0.9)

# add horizontal dashed lines and text
abline(h = 1, lty = 2, col = "darkred")
abline(h = 0, lty = 2, col = "darkred")
text(2.5, 0.9, cex = 0.8, "Mortgage denied")
text(2.5, -0.1, cex= 0.8, "Mortgage approved")

# add estimated regression line of Probit and Logit models
x <- seq(0, 3, 0.01)
y_probit <- predict(denyprobit, list(pirat = x), type = "response")
y_logit <- predict(denylogit, list(pirat = x), type = "response")

lines(x, y_probit, lwd = 1.5, col = "steelblue")
lines(x, y_logit, lwd = 1.5, col = "black", lty = 2)

# add a legend
legend("topleft",
       horiz = TRUE,
       legend = c("Probit", "Logit"),
       col = c("steelblue", "black"), 
       lty = c(1, 2))


############ More than 1 Variable X ############
# estimate a Logit regression with multiple regressors
denylogit2 <- glm(deny ~ pirat + black, 
                  family = binomial(link = "logit"), 
                  data = HMDA)

coeftest(denylogit2, vcov. = vcovHC, type = "HC1")

# For comparison we compute the predicted probability of denial for two hypothetical applicants that differ in race and have a P/I ratio of 0.3

# 1. compute predictions for P/I ratio = 0.3
predictions <- predict(denylogit2, 
                       newdata = data.frame("black" = c("no", "yes"), 
                                            "pirat" = c(0.3, 0.3)),
                       type = "response")

predictions

# 2. Compute difference in probabilities
diff(predictions) # We find that the white applicant faces a denial probability of only 7.5% , while the African American is rejected with a probability of 22.4%, a difference of 14.9%


# compute pseudo-R2 for the logit model
pseudoR2 <- 1 - (denylogit2$deviance) / (denylogit2$null.deviance)
pseudoR2
