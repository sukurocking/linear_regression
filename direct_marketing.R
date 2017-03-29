dm <- read.csv("DirectMarketing.csv")
summary(dm)
dm$Children <- as.factor(dm$Children)
library(tidyverse)
str(dm)

## Data exploration and preparation----

# Cross-checking the patterns of amount spent against all variables

## Amountspent ~ Age
ggplot(dm, aes(Age, AmountSpent)) + geom_boxplot()
# Middle and Old have almost similar patterns in terms of amount spent.. So let's combine them

dm$Age1 <- ifelse(dm$Age!= "Young", "Middle-Old", as.character(dm$Age))

## Amountspent ~ Gender
ggplot(dm, aes(Gender, AmountSpent)) + geom_boxplot()
# Males seem to spend more than females

## Amountspent ~ OwnHome
ggplot(dm, aes(OwnHome, AmountSpent)) + geom_boxplot()

# Those having Own home seem to spend more than those who live on rent.. A sensible conclusion that can be derived at

## AmountSpent ~ Married
ggplot(dm, aes(Married, AmountSpent)) + geom_boxplot()
# Married people tend to spend more than the single ones.. Another reasonable deduction

## Amountspent ~ Location
ggplot(dm, aes(Location, AmountSpent)) + geom_boxplot()
# Those who are far seem to spend more than the ones who are closeby

## Amountspent ~ Salary
ggplot(dm, aes(Salary, AmountSpent)) + geom_point()
# As salary increases, the Amount spent tends to increase.. Another reasonable deduction
# But the variation seems to increase as salary increases


## Amountspent ~ Children
ggplot(dm, aes(Children, AmountSpent)) + geom_boxplot()
# Those who have more than 1 child seem to behave differently than those having 0 children and 1 child
# Lets combine rows with 2 and 3 children
dm$Children1 <- ifelse(dm$Children== "2" | dm$Children == "3", "2-3", as.character(dm$Children))

## Amountspent ~ History
summary(dm$History) # We notice a lot of missing values.. 
# We need to impute the missing values
dm %>% group_by(History) %>% summarise('Avg. Amount Spent' = mean(AmountSpent))
# Calculating the average amount spent by each History group

which(is.na(dm$History)) %>% dm[.,] %>% summarise(Mean_amount_spent = mean(AmountSpent))
# Notice the format carefully.. dm[.,] # dot represents the previous value

# Calculated the average amount spent for those who have missing history
# The group closest to NA group is the medium history group but that's still too far (1239.9 ~ 950.4)
dm$History1 <- ifelse(is.na(dm$History),"Missing", as.character(dm$History))
dm %>% group_by(History1) %>% summarise('Avg. Amount Spent' = mean(AmountSpent))

ggplot(dm, aes(History1, AmountSpent)) + geom_boxplot()
# As expected, those having higher salaries tend to spend more

## Amountspent ~ Catalogs
unique(dm$Catalogs)
# There are only 4 unique values of catalogs, so we need to transform it to factor variable
dm$Catalogs <- as.factor(dm$Catalogs)
dm %>% group_by(Catalogs) %>% summarise(mean(AmountSpent))
ggplot(dm, aes(Catalogs, AmountSpent)) + geom_boxplot()
# People buying catalogs seem to spend more.. This is kinda expected

# Now its time to remove those variables that we modified earlier
dm1 <- dm %>% select(-Age,-Children,-History)
str(dm1)
dm1 <- dm1 %>% type_convert(col_types = cols(Age1 = col_factor(levels = c("Middle-Old", "Young")), Children1 = col_factor(levels = c("0","1","2-3")), History1 = col_factor(levels = c("High", "Low", "Medium", "Missing"))))
str(dm1)


# Now, we will build the regression model----
library(car)
x <- lm(formula = AmountSpent~., data = dm1)
summary(x)
## In the console, we can see the statistically significant variables marked ***

summary(dm1)
# Lets add the dummy variables for the categorical variables
library(dummies)

dm1 <- data.frame(dm1,
           dummy(dm1$Catalogs),
           dummy(dm1$Children1),
           dummy(dm1$History1))
str(dm1)
lm(
  formula = AmountSpent ~ Location + Salary + Catalogs + Children1 + History1,
  data = dm1
) %>% summary()

mod1 <- lm(
  formula = AmountSpent ~ Location + Salary + Catalogs + Children1 + dm1Low + dm1Medium,
  data = dm1
)
summary(mod1)
# The R squared value is 0.74.. This means 74% of the variation in Amount spent can be explained by these 5 variables

## Checking regression assumptions----

# Check for collinearity - correlation between variables
vif(mod1) #Variance inflation factors # Should be less than 10


# Checking for variance in residuals
qplot(mod1$fitted.values, mod1$residuals)
# As it can be seen, the variance in residuals is not constant
# It is funnelled

# Checking for normality of residuals
qqPlot(mod1$residuals)
ggplot(data = NULL, aes(mod1$residuals)) + geom_histogram()
# Non-normal behaviour observed

# So, this is not a very good model

## Applying log transform---------

mod2 <- lm(
  formula = log(AmountSpent) ~ Location + Salary + Catalogs + Children1 + dm1Low + dm1Missing,
  data = dm1
)
summary(mod2)
# Adjusted R square is 84%

# 1. Test for collinearity
vif(mod2)
# Variation Inflation Factors
# No problems with collinearity

#2. Test for normality
qqPlot(mod2$residuals)
qplot(mod2$residuals) #A histogram of residuals
# Slight variations from normal.. but we can live with that

#3. Test for constancy of variance of residuals
qplot(mod2$fitted.values, mod2$residuals) #Slight funnelling towards the end

## Another model

mod3 <- lm(
  formula = sqrt(AmountSpent) ~ Location + Salary + Catalogs + Children1 + dm1Low + dm1Missing,
  data = dm1
)
summary(mod3)
# This model has less adjusted R square than the previous one

# Testing for assumptions
vif(mod3) # No high correlations
# They should be less than 10

#Checking if the residuals are normal
qqPlot(mod3$residuals) 
ggplot(data=NULL, aes(mod3$residuals)) + geom_histogram()
# Looks okay

ggplot(data=NULL, aes(mod3$fitted.values, mod3$residuals)) + geom_point()
# Looks slightly funnelled towards the end

# We finalize the previous model.. The log transform one
predicted <- mod2$fitted.values
actual <- log(dm1$AmountSpent)
length(mod3$fitted.values)
dat <- data.frame(predicted,actual)

nrow(dat)
p <- ggplot(dat, aes(x = 1:nrow(dat), y = predicted)) + geom_line()
p + geom_line(data = dat, aes(y = actual), color = "blue")
# The fit chart! Checking if the predicted and the actual values overlap
