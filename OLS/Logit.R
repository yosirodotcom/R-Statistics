pacman::p_load(pacman, rio, tidyverse, caret, skimr, magrittr, mlbench, DescTools)
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
