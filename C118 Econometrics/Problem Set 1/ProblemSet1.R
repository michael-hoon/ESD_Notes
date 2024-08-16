# Images Exported in 850 x 640px
# Loading libraries
library("readxl")
library(psych)
library(tidyverse)

# Loading Datasets and cleaning
airline <- read_csv("C:/R/C118/Problem Set 1/Airline.csv")
# airline[is.na(airline)] <- 0
MIT_Salary <- read_excel("C:/R/C118/Problem Set 1/MIT_Faculty_Salaries.xls")
MIT_Salary$Experience2 <- MIT_Salary$Experience^2

# Question 2
summary_table <- describe(MIT_Salary)

# Question 3
salary <- MIT_Salary$Salary
hist(salary, main = "Salary Histogram Plot", xlab = "Salary")
kde <- density(salary)
plot(kde, main = "Non-parametric Kernel Density Salary Plot", xlab = "Salary")

# ggplot(data = MIT_Salary, aes(x = MIT_Salary$Salary)) +
#   geom_histogram()

# Question 4
multmodel <- lm(MIT_Salary$Salary ~ MIT_Salary$SexM1 + MIT_Salary$Experience + MIT_Salary$Experience2 + MIT_Salary$Articles)
summary(multmodel)

# Question 5A
# Gender
# modelgender <- lm(MIT_Salary$Salary ~ MIT_Salary$SexM1, data= MIT_Salary)
# summary(modelgender)

# Experience
# modelexperience <- lm(MIT_Salary$Salary ~ MIT_Salary$Experience, data= MIT_Salary)
# summary(modelexperience)

# Experience^2

# modelexperience2 <- lm(MIT_Salary$Salary ~ MIT_Salary$Experience2, data= MIT_Salary)
# summary(modelexperience2)

# Articles
# modelarticles <- lm(MIT_Salary$Salary ~ MIT_Salary$Articles, data= MIT_Salary)
# summary(modelarticles)

# Question 5C
ggplot(data=MIT_Salary, aes(x=Articles, y=Salary, group=1)) +
  geom_point() +
  theme_linedraw() +
  ggtitle("Salaries on Articles")
  
# Question 5D
salaryarticlemodel <- lm(MIT_Salary$Salary ~ MIT_Salary$Articles, data = MIT_Salary)
res <- resid(salaryarticlemodel)
plot(MIT_Salary$Articles, res,
     ylab="Residuals", xlab="Articles", 
     main="Error Terms on Articles", pch=20)
abline(0,0, lty=2)

# Question 6E 
airlinemult <- lm(airline$pass ~ airline$time + airline$jan + airline$feb + airline$mar + airline$apr + airline$may + airline$jun + airline$jul + airline$aug + airline$sep + airline$oct + airline$nov, data = airline)
summary(airlinemult)

# Question 6I
ggplot(data=airline, aes(x=time, y=pass, group=1)) +
  geom_point() +
  theme_linedraw() +
  ggtitle("Passengers on Time")

# Question 6J
passtimemodel <- lm(airline$pass ~ airline$time, data = airline)
res2 <- resid(passtimemodel)
plot(airline$time, res2,
     ylab="Residuals", xlab="Time", 
     main="Error Terms on Time", pch=20)
abline(0,0, lty=2)