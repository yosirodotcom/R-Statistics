pacman::p_load(pacman, rio, dplyr, Stat2Data, skimr, olsrr)
rm(list = ls())

model <- lm(dist ~ speed, data=cars)

# Breaush Pagan Test
test_pagan <- as.vector((bptest(model))$p.value)
ifelse(test_pagan > 0.05, "Lolos Uji Heterokedastisitas pada Breaush Pagan Test", "Tidak lolos Uji Heteroskedastisitas pada Breaush Pagan Test")
ols_test_breusch_pagan(model)


# Park Test
residual <- abs(resid(model))
y_hat <- abs(fitted(model))
seperX <- 1/cars$dist
model_park.hetero <- lm(log(residual^2) ~ log(y_hat), data = cars)
test_park <- as.vector(summary(model_park.hetero)$coefficients[2,4])
ifelse(test_park > 0.05, "Lolos Uji Heterokedastisitas pada Park Test", "Tidak lolos Uji Heteroskedastisitas pada Park Test")
ols

# Glejser Test
model_glejser_1 <- lm(abs(residual) ~ dist, cars)
test_Geljser1 <- as.vector(summary(model_glejser_1)$coefficients[2,4])
ifelse(test_Geljser1 < 0.05, "Lolos Uji Heterokedastisitas pada model Glejser 1", "Tidak lolos Uji Heteroskedastisitas pada model Glejser 1")


model_glejser_2 <- lm(abs(residual) ~ sqrt(dist), cars)
test_Geljser2 <- as.vector(summary(model_glejser_2)$coefficients[2,4])
ifelse(test_Geljser2 < 0.05, "Lolos Uji Heterokedastisitas pada model Glejser 2", "Tidak lolos Uji Heteroskedastisitas pada model Glejser 2")

model_glejser_3 <- lm(abs(residual) ~ seperX, cars)
test_Geljser3 <- as.vector(summary(model_glejser_3)$coefficients[2,4])
ifelse(test_Geljser3 < 0.05, "Lolos Uji Heterokedastisitas pada model Glejser 3", "Tidak lolos Uji Heteroskedastisitas pada model Glejser 3")

# Metode Korelasi Spearman
spearman1 <- cor.test(x = abs(y_hat), y = abs(residual), method = c("spearman")) 
test_spearman <- as.vector(spearman1$p.value)
ifelse(test_spearman < 0.05, "Lolos Uji Heterokedastisitas pada Korelasi Spearman", "Tidak lolos Uji Heteroskedastisitas pada Korelasi Spearman")


# Metode White
model_white <- lm(post.test.3 ~ pretest.1+pretest.2, data = Baumann)
test_white <- white_lm(model_white)
ifelse(test_white$p.value > 0.05, "Lolos Uji Heterokedastisitas pada metode White", "Tidak Lolos Uji Heterokedastisitas pada metode White")

# Uji F
ols_test_f(model)


# Penyembuhan

df <- as_tibble(valueofstocks)

## model yang bermasalah dengan hetero
hetero_model <- lm(VST ~ MB, df)
test_bp1 <- bptest(hetero_model) 
ifelse(test_bp1$p.value > 0.05, "Lolos Uji Heterokedastisitas pada Breaush Pagan Test", "Tidak lolos Uji Heteroskedastisitas pada Breaush Pagan Test")

## disembuhkan dengan metode robust se
coeftest(hetero_model, vcov = vcovHC(hetero_model, "HC1"))

## disembuhkan dengan 1/sqrt(X)
homo_model1 <- lm(VST/sqrt(MB) ~ MB/sqrt(MB), df)
bptest(homo_model1)

## disembuhkan dengan 1/X
homo_model2 <- lm(VST/MB ~ MB/MB, df)
bptest(homo_model2)
