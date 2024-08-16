# Loading Libraries
library(tidyverse)
library(car)
library(lmtest)
library(orcutt)
library(strucchange)

# Loading Datasets and cleaning
airline <- read_csv("C:/R/C118/Problem Set 1/Airline.csv")

# 1a
airlinemult <- lm(pass ~ time + jan + feb + mar + apr + may + jun + jul + aug + sep + oct + nov, data = airline)
summary(airlinemult)

# 1b
resairline <- resid(airlinemult)
plot(fitted(airlinemult), resairline, pch=20, title('OLS Error Plot'), ylab = 'Errors', xlab = 'Fitted Values')
abline(0,0, lty=2)

# 1c
durbinWatsonTest(airlinemult)
dwtest(formula = airlinemult)

# 1d
orcutt1 <- cochrane.orcutt(airlinemult)

# 4a 
sctest(airlinemult, type = 'Chow')
