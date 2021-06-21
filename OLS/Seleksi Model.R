pacman::p_load(Stat2Data, skimr,leaps, dplyr, magrittr, MuMIn, olsrr)

data("FirstYearGPA")
df <- FirstYearGPA %>% na.omit()
skim(FirstYearGPA)

# Kriteria Mallow Cp

full_model <- lm(GPA ~ ., data = df)
mallow_model1 <- lm(GPA ~ HSGPA+SATV+SATM+Male, data = df)
mallow_model2 <- lm(GPA ~ HSGPA+SATV+SATM+Male+HU, data = df)
mallow_model3 <- lm(GPA ~ HSGPA+SATV+SATM+Male+HU+SS, data = df)
mallow_model4 <- lm(GPA ~ HSGPA+SATV+SATM+Male+HU+SS+FirstGen, data = df)
ols_mallows_cp(mallow_model1, full_model)
ols_mallows_cp(mallow_model2, full_model)
ols_mallows_cp(mallow_model3, full_model)
ols_mallows_cp(mallow_model4, full_model)

### dengan fungsi dredge

options(na.action ="na.fail")
dredge(global.model = lm(GPA ~ ., data = df))
options(na.action ="na.omit")

# exhaustive method
model_fit_exhaustive <- regsubsets(GPA ~ ., data = df, nbest = 1, method = "exhaustive")
df_fit_exhaustive <- with(summary(model_fit_exhaustive), data.frame(rsq, adjr2, cp, bic, rss, outmat)) %>% print()

#### berdasarkan cp terendah 

### backward method
model_fit_backward <- regsubsets(GPA ~ ., data = df, nbest = 1, method = "backward")
df_fit_backward <- with(summary(model_fit_backward), data.frame(rsq, adjr2, cp, bic, rss, outmat)) %>% print()
ols_step_backward_p(full_model)


### forward method
model_fit_forward <- regsubsets(GPA ~ ., data = df, nbest = 1, method = "forward")
df_fit_forward <- with(summary(model_fit_forward), data.frame(rsq, adjr2, cp, bic, rss, outmat)) %>% print()
ols_step_forward_p(full_model)


### combine method
model_fit_combine <- regsubsets(GPA ~ ., data = df, nbest = 1, method = "seqrep")
df_fit_combine <- with(summary(model_fit_combine), data.frame(rsq, adjr2, cp, bic, rss, outmat)) %>% print()

ols_step_both_p(full_model)
