# Question 1
print("Michael")

# Question 2
print("1006617")

# Question 3
print(1006617 %% 10000)

# Question 4
vec <- c(1,0,0,6,6,1,7)
print(vec)

# Question 5
length(vec)

# Question 6
ifelse(vec[length(vec)] %% 2 == 0, "even", "odd")

# Question 7
for (i in length(vec):1) {
  print(vec[i])
}

# Question 8
teamnumber <- 10
cohortnumber <- 2

dba <- function(a, b) {
  result <- a^2 / b
  return(result)
}

yes <- dba(teamnumber, cohortnumber)
print(yes)

# Question 9
ccdata <- read.csv("C:/DBA_Project/Homework/cheddar-cheese.csv")

# Question 10
head(ccdata)

# Question 11
mean(ccdata$Taste)
