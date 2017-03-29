library(tidyverse)
library(dummies)
library(car)

dm1 <- read_csv("DirectMarketing.csv")

# Amount spent is our target variable
dm1

# Age variable
table(dm1$Age)
ggplot(dm1, aes(Age, AmountSpent)) + geom_boxplot()
# Combining Middle and Old categories
dm1$Age1 <- ifelse(dm1$Age != "Young", "Middle-Old", dm1$Age)
dm1$Age1 <- as.factor(dm1$Age1)

# Gender variable
table(dm1$Gender)
dm1 %>% group_by(Gender) %>% summarise(mean(AmountSpent))
# Quite a bit of difference between amount spent by males and females.. Interestingly, males seem to spend more
dm1$Gender <- as.factor(dm1$Gender)

# OwnHome variable

check <- function(var,var_same) {
  table <- table(var_same)
  summarise_amount <- dm1 %>% group_by_(.dots = var) %>% summarise(mean(AmountSpent))
  plot <- ggplot(dm1, aes(x = var_same,AmountSpent, group = var_same)) + geom_boxplot()
  list(table, summarise_amount, plot)
}
check2 <- function(var) {
  colindex <- which(colnames(dm1)==var)
  variable <- dm1[,colindex] %>% unlist() %>% as.factor()
  table <- table(dm1[,colindex])
  summarise_amount <- dm1 %>% group_by_(.dots = var) %>% summarise(mean(AmountSpent))
  plot <- ggplot(dm1, aes(x = variable,y = AmountSpent)) + geom_boxplot()
  list(table, summarise_amount, plot)
}
## Group_by_ and Summarise works in this case
# Ggplot and table dont work
with(dm1,check("OwnHome", OwnHome))
dm1$OwnHome <- as.factor(dm1$OwnHome)

with(dm1, check("Married",Married))
# Married people seem to spend more than the single ones
dm1$Married <- as.factor(dm1$Married)

with(dm1, check("Location",Location))
# Those who are far seem to spend more than the ones who are close to the marketer
dm1$Location <- as.factor(dm1$Location)

ggplot(dm1, aes(Salary, AmountSpent)) + geom_point()
cor.test(dm1$Salary, dm1$AmountSpent)
# Correlation is pretty high.. 70%
# But the variance seems to increase as salary increases.. This might create problems in our model
str(dm1)

with(dm1, check("Children", Children))
with(dm1, check2("OwnHome"))

# People with 2 or 3 children behave quite similarly but in the different way 
# Lets combine people with 2 and 3 children

dm1$Children1 <- ifelse(dm1$Children==2 | dm1$Children == 3, "2-3", as.character(dm1$Children))
with(dm1, check("Children1", Children1))
which(colnames(dm1)=="Children1")

with(dm1, check2("Children1"))

dm1$Children1 <- as.factor(dm1$Children1)

# History column-----
with(dm1, check2("History"))
# Since we cant impute missing value, lets name them as another column
# With increasing salary, amount spent also increases

index <- which(is.na(dm1$History))
dm1$History[index] <- "Missing"
# Creating dummies for History column
dm1 <- data.frame(dm1, dummy(dm1$History))

# Catalogs column
with(dm1, check2("Catalogs"))
# As the no. of catalogs increases, the amount spent also increases
# Creating dummies for Catalogs column
dm1 <- data.frame(dm1, dummy(dm1$Catalogs))

# Now, we will drop the variable out of which we created other transformed other variables

dm <- dm1 %>% select(-Age, -Children, -History, -Catalogs)
str(dm)
mod1 <- lm(formula = AmountSpent ~ ., data = dm)
summary(mod1)

# We will remove the statistically insignificant variables from the regression analysis
mod2 <- lm(formula = AmountSpent ~ Location + Salary + Children1 + dm1Low + dm1Medium + dm16 + dm112 + dm118, data = dm)
summary(mod2)
# All the variables considered are statistically significant in this analysis

# 1. Adjusted R square is 0.74.. 74% of the variation in amount spent can be explained by these 5 variables (Location, Salary, Children, History, Catalogs)

# Checking for regression assumptions
# 2. Checking for collinearity between variables, variance inflation factor
vif(mod2) 
# All of them are comfortably less than 10

# 3. Normality check of residuals
qqPlot(mod2$residuals)
ggplot(data=NULL, aes(x = mod2$residuals)) + geom_histogram()
# It is not normal.. This doesn't satisfy our conditions for linear regression

# 4. Constancy of variance check
ggplot(data=NULL, aes(y = mod2$residuals, x = mod2$fitted.values)) + geom_point()
# This is a funnelled chart

# Trying log transformation of dependent variable
mod3 <- lm(
  formula = log(AmountSpent) ~ Location + Salary + Children1 + dm1Low + dm1Medium +  dm16 + dm112 + dm118,
  data = dm
)
summary(mod3)
# Looks like a better model

# 1. Adjusted R square is 0.84

# 2. Variance inflation check
chisq.test(dm$Location, dm$Salary)
vif(mod3)
# No problems with that

#3. Constancy in variance check
ggplot(data=NULL, aes(mod3$fitted.values, mod3$residuals)) + geom_point()
# Very slightly tunnelled towards the end.. We can live with that

#4. Normality check of residuals
ggplot(data=NULL, aes(mod3$residuals)) + geom_histogram()
qqPlot(mod3)
# Seems good enough

# We finalize this model
dat <- data.frame(actual = log(dm$AmountSpent), predicted = mod3$fitted.values)
dat %>% head()
p <- ggplot(data = dat, aes(x = 1:nrow(dat), y = actual)) + geom_line()
p + geom_line(data = dat, aes(y = predicted), color = "blue")
# This is the fit chart
# As we can see, there are a lot of overlaps.. This is hence a good model