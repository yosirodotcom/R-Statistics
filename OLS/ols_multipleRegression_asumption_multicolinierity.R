p_load(rio, corrplot, mctest, ppcor, car)
rm(list = ls())

wagesmicrodata <- import(file = "clipboard") # or load environment
attach(wagesmicrodata)

fit1<- lm(log(WAGE)~OCCUPATION+SECTOR+UNION+EDUCATION+EXPERIENCE+AGE+SEX+MARR+RACE+SOUTH)
summary(fit1)

#1. Detect multicolliniearity 
##1.1. with graph
X<-wagesmicrodata[,-6]
corrplot::corrplot(cor(X)) # see AGE and EXPERIENCE is highly correlated

##1.2. Detect multicolliniearity with Farrar – Glauber Test
mctest::omcdiag(fit1) # secara umum ada multicoliniearity antar variabel, tapi belum tahu variabel mana
mctest::imcdiag(fit1) # ada 3 variabel yang dicurigai mempunyai masalah multikol, terlihat dari nilainya yang besar

##1.3. Detect multicolliniearity with t-test)
ppcor::pcor(X, method = "pearson") # fokus pada nilai variabel yang dicurigai, pada $statistic nilai yang besar menunjukkan adanya masalah multikol antar 2 variabel. (Di atas 1000)

##1.4. Detect multicolliniearity with VIF 
car::vif(fit1) # nilai VIF masih terlalu besar >10 pada 3 variabel


#2. Healing Multicolliniearity

## Remove problematic variabel, in this case is EXPERIENCE
fit2 <- lm(log(WAGE)~OCCUPATION+SECTOR+UNION+EDUCATION+AGE+SEX+MARR+RACE+SOUTH)
summary(fit2)
car::vif(fit2) # nilai VIF sudah normal untuk setiap variabel
