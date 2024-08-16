# Graphs 850 x 640 px
library("readxl")
library(tidyverse)
library(lmtest)
library(ggplot2)
library(skedastic)
library(nlme)
install.packages('car')
library(car)

# Load Data
gretlhomi <- read_csv("C:/R/C118/Problem Set 2/data7-14.csv")
MIT_Crimes <- read_excel("C:/R/C118/Problem Set 2/MITCrimes.xlsx")

# Q4 ABCE
mitmultmodel <- lm(Crimes ~ PovRate + UnemRate + GdpPC + PoliceExp, data=MIT_Crimes)
summary(mitmultmodel)

# Q4D
white <- white(mitmultmodel, interactions = TRUE)

# Q5A
gretlmultmodel <- lm(mr ~ exec + south + ue + capital + pcy, data=gretlhomi)
summary(gretlmultmodel)

# Q5B
ggplot(data=gretlhomi, aes(x=exec, y=mr, group=1)) +
  geom_point() +
  theme_linedraw() +
  ggtitle("mr on exec")

ggplot(data=gretlhomi, aes(x=ue, y=mr, group=1)) +
  geom_point() +
  theme_linedraw() +
  ggtitle("mr on ue")

ggplot(data=gretlhomi, aes(x=pcy, y=mr, group=1)) +
  geom_point() +
  theme_linedraw() +
  ggtitle("mr on pcy")

# Q5C
ggplot(gretlhomi, aes(x=gretlhomi$exec, y=predict(gretlmultmodel))) + 
  geom_point() +
  labs(x='exec', y='Fitted Values of mr', title='Fitted mr vs. exec') + 
  theme_linedraw()

ggplot(gretlhomi, aes(x=gretlhomi$ue, y=predict(gretlmultmodel))) + 
  geom_point() +
  labs(x='ue', y='Fitted Values of mr', title='Fitted mr vs. ue') + 
  theme_linedraw()

ggplot(gretlhomi, aes(x=gretlhomi$pcy, y=predict(gretlmultmodel))) + 
  geom_point() +
  labs(x='pcy', y='Fitted Values of mr', title='Fitted mr vs. pcy') + 
  theme_linedraw()


#Q5D
res <- resid(gretlmultmodel)
plot(gretlhomi$exec, res,
     ylab="Residuals", xlab="exec", 
     main="Error Terms on exec", pch=20)
abline(0,0, lty=2)

plot(gretlhomi$ue, res,
     ylab="Residuals", xlab="ue", 
     main="Error Terms on ue", pch=20)
abline(0,0, lty=2)

plot(gretlhomi$pcy, res,
     ylab="Residuals", xlab="pcy", 
     main="Error Terms on pcy", pch=20)
abline(0,0, lty=2)

# 5E
white2 <- white(gretlmultmodel, interactions = TRUE)

# 5H
glsmodel <- gls(mr ~ exec + south + ue + capital + pcy, 
                data = gretlhomi, weights = varPower())
summary(glsmodel)

res2 <- resid(glsmodel)

plot(gretlhomi$exec, res2,
     ylab="Residuals", xlab="exec", 
     main="GLS Error Terms on exec", pch=20)
abline(0,0, lty=2)

plot(gretlhomi$ue, res2,
     ylab="Residuals", xlab="ue", 
     main="GLS Error Terms on ue", pch=20)
abline(0,0, lty=2)

plot(gretlhomi$pcy, res2,
     ylab="Residuals", xlab="pcy", 
     main="GLS Error Terms on pcy", pch=20)
abline(0,0, lty=2)

# 5J
dw_test <- dwtest(gretlhomi)
durbinWatsonTest(gretlhomi)

