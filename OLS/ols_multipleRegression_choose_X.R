pacman::p_load(Stat2Data, skimr,leaps, dplyr, magrittr, MuMIn)

data("FirstYearGPA")
skim(FirstYearGPA)

### exhaustive method
model_fit_exhaustive <- regsubsets(GPA ~ ., data = FirstYearGPA, nbest = 1, method = "exhaustive")
df_fit_exhaustive <- with(summary(model_fit_exhaustive), data.frame(rsq, adjr2, cp, bic, rss, outmat)) %>% print()
#### berdasarkan cp terendah kita memilih model nomor 5

### dengan fungsi dredge

options(na.action ="na.fail")
dredge(global.model = lm(GPA ~ ., data = FirstYearGPA))
options(na.action ="na.omit")

### backward method
model_fit_backward <- regsubsets(GPA ~ ., data = FirstYearGPA, nbest = 1, method = "backward")
df_fit_backward <- with(summary(model_fit_backward), data.frame(rsq, adjr2, cp, bic, rss, outmat)) %>% print()
#### berdasarkan cp terendah kita memilih model nomor 5

### forward method
model_fit_forward <- regsubsets(GPA ~ ., data = FirstYearGPA, nbest = 1, method = "forward")
df_fit_forward <- with(summary(model_fit_forward), data.frame(rsq, adjr2, cp, bic, rss, outmat)) %>% print()
#### berdasarkan cp terendah kita memilih model nomor 5

### combine method
model_fit_combine <- regsubsets(GPA ~ ., data = FirstYearGPA, nbest = 1, method = "seqrep")
df_fit_combine <- with(summary(model_fit_combine), data.frame(rsq, adjr2, cp, bic, rss, outmat)) %>% print()
#### berdasarkan cp terendah kita memilih model nomor 5