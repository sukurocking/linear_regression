boston <- read.csv(file = "boston_house_prices.csv", header = TRUE, skip = 1, nrows = 506,col.names = c("CRIM", "ZN", "INDUS", "Charles.River.dummy.variable","nitric.oxides.concentration", "X.rooms.dwelling", "AGE", "DIS", "RAD", "TAX", "PTRATIO","B","LSTAT", "MEDV"))

# Our dependent variable is the MEDV variable
# Our objective is to predict boston housing prices

# Data exploration and preparation
dim(boston)
colSums(is.na(boston))
summary(boston)

# It seems there are no missing values

# Checking outliers in our dependent variable
out <- boxplot(boston$MEDV)$out
# 37 outliers.. Removing these
# boston_wo_outlier <- boston %>% filter(MEDV %in% out == FALSE)

library(tidyverse)

# Univariate analysis -------

# CRIM
boston %>% ggplot(aes(x = CRIM)) + geom_histogram()
boston %>% count(CRIM >= 4)
quantile(boston$CRIM,probs = 0:100/100)
summary(boston$CRIM)

# Need to divide this into categorical data, indicating if a town has high, medium, low or very high crime rate
boston$crime_rate <- with(boston, cut(x = CRIM, breaks = quantile(CRIM, probs = 0:5/5), labels = c("Very Low", "Low", "Medium", "High", "Very High"), include.lowest = TRUE, ordered_result = TRUE)) 

boston %>% group_by(crime_rate) %>% summarise(min(CRIM), max(CRIM), mean(CRIM), count = n())
# count, minimum crime rate, max crime rate, average crime rate by each ordered group 

# ZN
count(boston, ZN == 0)
# A lot of the towns don't have residential land zoned
quantile(boston$ZN, probs = 0:100/100)
unique(boston$ZN)
# Creating a variable indicating if the town has residential plot zoned
boston$ZN1 <- ifelse(boston$ZN == 0, 0, 1)

# INDUS
# Proportion of non-retail business acres per town
summary(boston$INDUS)
hist(boston$INDUS)
# Creating a catgorical variable for INDUS
boston$INDUS1 <- with(boston, cut(INDUS, breaks = quantile(INDUS), labels = c("Very Low", "Low", "Medium", "High"), include.lowest = TRUE, ordered_result = TRUE))
boston %>% group_by(INDUS1) %>% summarise(min(INDUS), max(INDUS), n())

# Charles.River
boston %>% count(Charles.River.dummy.variable)
boston %>% ggplot(aes(as.factor(Charles.River.dummy.variable))) + geom_bar(stat = "count")
# There are a lot of towns in which tract doesnt bound river

# Nitric Oxides concentration
summary(boston$nitric.oxides.concentration)

# X rooms dwelling
# Average no. of rooms per dwelling
summary(boston$X.rooms.dwelling)
boston$X.rooms.dwelling1 <- with(boston, cut(X.rooms.dwelling, breaks = quantile(X.rooms.dwelling, probs = 0:2/2), labels = c("Low","High"), include.lowest = TRUE, ordered_result = TRUE))
boston %>% group_by(X.rooms.dwelling1) %>% summarise(min = min(X.rooms.dwelling),max = max(X.rooms.dwelling), n = n(), avg = mean(X.rooms.dwelling))

# AGE 
# Proportion of owner-occupied units built prior to 1940
summary(boston$AGE)
quantile(boston$AGE,probs = 0:10/10)

# DIS
summary(boston$DIS)

# RAD
summary(boston$RAD)
unique(boston$RAD)
count(boston, RAD)
# Accessibility to radial highways, 24 is the most common amongst towns

# TAX
summary(boston$TAX)

# PTRATIO
summary(boston$PTRATIO)

# B
summary(boston$B)

# LSTAT
summary(boston$LSTAT)

# Bivariate analyses ----
ggplot(data = boston, aes(x = CRIM, y = MEDV)) + geom_point()
ggplot(data = boston, aes(x = crime_rate, y = MEDV)) + geom_boxplot()
# As crime rate increases, the housing prices seem to decrease

ggplot(boston, aes(ZN,MEDV)) + geom_point()
boston %>% filter(ZN>0) %>% ggplot(aes(ZN, MEDV)) + geom_point()
# Roughly speaking, as the number of zones increases, the prices seem to increase

ggplot(boston, aes(x = INDUS, y = MEDV)) + geom_point()
# With increasing proportion of non-retail business in town, the prices tend to decrease

boston %>% ggplot(aes(Charles.River.dummy.variable, MEDV, group = Charles.River.dummy.variable)) + geom_boxplot()
# If the tract is bounded by river, the prices tend to increase

ggplot(boston, aes(nitric.oxides.concentration, MEDV)) + geom_point()
# The prices seem to decrease with increase in nitric oxide concentration

ggplot(boston, aes(X.rooms.dwelling, MEDV)) + geom_point()
# This is a clearly significant pattern
# As the no. of average rooms per dwelling increases, the prices tend to increase

ggplot(boston, aes(AGE, MEDV)) + geom_point()
# With increasing proportion of aged houses, the prices tend to decrease

ggplot(boston, aes(DIS, MEDV)) + geom_point()
# Most of the values are concentrated close to 2.5 DIS.. 
# As distance increases, the prices increase for a certain point and then become constant

ggplot(boston, aes(RAD, MEDV)) + geom_point()

ggplot(boston, aes(TAX, MEDV)) + geom_point()
# At the highest taxes, the prices are the lowest

ggplot(boston, aes(PTRATIO, MEDV)) + geom_point()


ggplot(boston, aes(B, MEDV)) + geom_point()
# prices are considerably and abnormally high as B > 350

ggplot(boston, aes(LSTAT, MEDV)) + geom_point()
# If the town has higher % lower status population, the prices seem to be decrease

# Correlation matrix
boston <- boston %>% select(-X.rooms.dwelling1,-INDUS1,-crime_rate)
boston <- boston %>% select(-ZN1)
cor(boston)

# Building regression models-----
# Building training and test data
library(caret)

index <- createDataPartition(y = boston$MEDV, times = 1,p = 0.7,list = FALSE)
training_data <- boston[index,]
test_data <- boston[-index,]

attach(boston)
# Creating regression models on our training data
reg1 <- lm(MEDV~., data = training_data)
summary(reg1)

# Dropping those variables which are insignificant
reg2 <- lm(MEDV~CRIM + ZN + Charles.River.dummy.variable + nitric.oxides.concentration + X.rooms.dwelling + DIS + RAD + TAX + PTRATIO + B + LSTAT)
summary(reg2)
# Adjusted R squared is 0.735, seems good

# Checking the assumptions of regression
# 1. Variance inflation factor
library(car)
vif(reg2)
# All the values are less than 10.. TAX and RAD are dangerously close

# 2. Normality check of residuals
qqPlot(reg2)
hist(reg2$residuals)
# Non-normal behaviour observed

# 3. Constancy of variance check of residuals
ggplot(data = NULL, aes(x = reg2$fitted.values, y = reg2$residuals)) + geom_point()
# Definitely not constant variance
qplot(x = reg2$fitted.values,
      y = (reg2$residuals - mean(reg2$residuals)) ^ 2)

# Log transformation of dependent variable
reg3 <- lm(log(MEDV)~., training_data)
summary(reg3)

# Removing insignificant variables
reg4 <- lm(log(MEDV) ~ . - ZN - INDUS - AGE, training_data)
summary(reg4)
# Adjusted R squared is 0.79.. This seems like a better model

# Regression assumptions
# 1. Variance inflation check
vif(reg4)
# There seems to be some correlation but nothing too serious

# 2. Normality check of residuals
qqPlot(reg4)
hist(reg4$residuals)
# Non-normal behaviour observed

# 3. Constancy of variance check of residuals
ggplot(data = NULL, aes(x = reg4$fitted.values, y = reg4$residuals)) + geom_point()
# Definitely not constant variance
qplot(x = reg4$fitted.values,
      y = (reg4$residuals - mean(reg4$residuals)) ^ 2)


reg5 <- lm(MEDV ~ CRIM + Charles.River.dummy.variable + log(nitric.oxides.concentration) + log(X.rooms.dwelling) + log(DIS) + log(RAD) + log(PTRATIO) + log(B) + log(LSTAT), training_data) 
summary(reg5)
# Adjusted R squared in 0.8

# Regression assumptions
# 1. Variance inflation check
vif(reg5)
# looks good.. no problems with this


# 2. Normality check of residuals
qqPlot(reg5)
hist(reg5$residuals)
# Normal behaviour observed

# 3. Constancy of variance check of residuals
ggplot(data = NULL, aes(x = reg5$fitted.values, y = reg5$residuals)) + geom_point()
# Slightly non-constant variance, but this is definitely our best model
qplot(x = reg5$fitted.values,
      y = (reg5$residuals - mean(reg5$residuals)) ^ 2)

# Fit-chart
fit_df <- data.frame(predicted = reg5$fitted.values, actual = training_data$MEDV)
p <- fit_df %>% ggplot(aes(x = 1:nrow(.), y = predicted)) + geom_line(color = "blue")
p + geom_line(data = fit_df, color = "black", aes(y = actual))
# Our model closely follows the actual values.. This is a good model

# Using our model to predict the values in test data
test_prediction <- predict(object = reg5, newdata = test_data)
length(test_prediction)
test_df <- data.frame(predicted = test_prediction, actual = test_data$MEDV )
test_df <- test_df %>% mutate(error = abs((actual - predicted)/actual))
mean(test_df$error) 
# 17.5% error

# Fit chart of the test data
p_test <- test_df %>% ggplot(aes(1:nrow(.), actual)) + geom_line(color = "blue")
p_test
p_test + geom_line(data = test_df, aes(y = predicted), color = "red")
# The "red" line closely follows the "blue" line... i.e. the predicted values closely follow the actual values