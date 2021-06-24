pacman::p_load(pacman, rio, tidyverse, magrittr, LogisticDx, DescTools)
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
