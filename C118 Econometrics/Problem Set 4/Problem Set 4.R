library(tidyverse)
library("readxl")
library(lmtest)
library(plm)
library(AER)
library(ivreg)
library(Matrix)
library(aod)

collapp <- read_excel("C:/R/C118/Problem Set 4/CollegeApplication.xls")
demsup <- read_excel("C:/R/C118/Problem Set 4/RevisedDMSupply&Demand.xlsx")

# Q1AD
baddemand <- lm(lnq ~ lnp + DisIncome + TurkeyPrice, data=demsup)
summary(baddemand)
tslsd <- lm(lnp ~ DisIncome + TurkeyPrice + Immigration + Technology, data=demsup)
phat <- fitted.values(tslsd)
tsls2 <- lm(lnq ~ phat + DisIncome + TurkeyPrice, data=demsup)
summary(tsls2)

badsupply <- lm(lnq ~ lnp + Immigration + Technology, data=demsup)
summary(badsupply)
tsls2s <- lm(lnq ~ phat + Immigration + Technology, data=demsup)
summary(tsls2s)

iv_demand <- ivreg(lnq ~ DisIncome + TurkeyPrice | lnp | Immigration + Technology, data = demsup)
summary(iv_demand, diagnostics = TRUE)

iv_supply <- ivreg(lnq ~ Immigration + Technology | lnp | DisIncome + TurkeyPrice, data = demsup)
summary(iv_supply, diagnostics = TRUE)

cov_ols <- vcov(baddemand)
cov_iv <- vcov(iv_demand)

# Calculate the Hausman test statistic
diff_coefs <- coef(baddemand) - coef(iv_demand)
hausman_statistic <- t(diff_coefs) %*% solve(cov_iv - cov_ols) %*% diff_coefs

cov_ols2 <- vcov(badsupply)
cov_iv2 <- vcov(iv_supply)
diff_coefs <- coef(badsupply) - coef(iv_supply)
hausman_statistic2 <- t(diff_coefs) %*% solve(cov_ols2 - cov_iv2) %*% diff_coefs
df2 <- length(coef(iv_supply))

# Degrees of freedom
df <- length(coef(baddemand))

rank_iv <- rankMatrix(Matrix(cov_ols2))
rank_ols <- rankMatrix(Matrix(cov_iv2))

# Calculate the degrees of freedom
degrees_of_freedom <- rank_iv - rank_ols

# Critical value at 5% significance level
critical_value <- qchisq(0.95, 3)

# Perform the Hausman test
p_value <- 1 - pchisq(hausman_statistic, df)

#Q2A
model_unlikely <- lm(unlikely ~ gpa, data = collapp)
model_likely <- lm(likely ~ gpa, data = collapp)
model_verylikely <- lm(verylikely ~ gpa, data = collapp)
summary(model_unlikely)
summary(model_likely)
summary(model_verylikely)

#Q2D
glmunlikely_model <- glm(unlikely ~ gpa, family = binomial(link="logit"), data = collapp)
glmlikely_model <- glm(likely ~ gpa, family = binomial(link="logit"), data = collapp)
glmverylikely_model <- glm(verylikely ~ gpa, family = binomial(link="logit"), data = collapp)

summary(glmunlikely_model)
summary(glmlikely_model)
summary(glmverylikely_model)

hypothesis_matrix1 <- c("gpa = 0")
wald_result <- waldtest(glmunlikely_model, hypothesis_matrix1)
wald.test(Sigma = vcov(glmverylikely_model), b = coef(glmverylikely_model), Terms = 1)

#Q2 LRT
restricted_unlikelymodel <- glm(unlikely ~ 1, family = binomial(link="logit"), data = collapp)
restricted_likelymodel <- glm(likely ~ 1, family = binomial(link="logit"), data = collapp)
restricted_verylikelymodel <- glm(verylikely ~ 1, family = binomial(link="logit"), data = collapp)

lr_unlikely <- -2 * (logLik(glmunlikely_model) - logLik(restricted_unlikelymodel))
lr_likely <- -2 * (logLik(glmlikely_model) - logLik(restricted_likelymodel))
lr_verylikely <- -2 * (logLik(glmverylikely_model) - logLik(restricted_verylikelymodel))

lr_unlikely2 <- lrtest(restricted_unlikelymodel, glmunlikely_model)
lr_likely2 <- lrtest(restricted_likelymodel, glmlikely_model)
lr_verylikely2 <- lrtest(restricted_verylikelymodel, glmverylikely_model)
lr_unlikely2_statistic <- lr_unlikely2$statistic
lr_likely2_statistic <- lr_likely2$statistic
lr_verylikely2_statistic <- lr_verylikely2$statistic

