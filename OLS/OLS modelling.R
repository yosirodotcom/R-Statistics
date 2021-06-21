pacman::p_load(pacman, rio, dplyr, Stat2Data, skimr, broom)
rm(list = ls())

data("FirstYearGPA")
# Prepare data
df <- FirstYearGPA %>% as_tibble()
skim(df)
str(df)

# Simple Regression
model1.1 <- lm(GPA ~ HSGPA, data = df)
summary(model1.1)
glance(model1.1)

model1.2 <- ols_regress(GPA ~ HSGPA, data = df)
model1.2

# Multiple Regression
model2.1 <- lm(GPA ~ ., data = df)
summary(model2.1)
glance(model2.1)

model2.2 <- ols_regress(GPA ~ HSGPA + SATV + SATM, data = df)
model2.2

# Variable X is a factor

df$Male <- as.factor(df$Male)
df$FirstGen <- as.factor((df$FirstGen))

model3.1 <- lm(GPA ~ HSGPA + Male + FirstGen, data = df)
summary(model3.1)
glance(model3.1)

# Bentuk transformasi jika data antar variabel jauh berbeda nilainya

model4.1 <- lm(log(GPA) ~ log(SATV) + log(SATM), data = df)
summary(model4.1)
glance(model4.1)
model4.2 <- lm(GPA ~ HSGPA + log(SATM), data = df)
summary(model4.2)
glance(model4.2)
model4.3 <- lm(log(SATV) ~ GPA + HSGPA, data = df)
summary(model4.3)
glance(model4.3)


