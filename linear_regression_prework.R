## Linear regression----

library(tidyverse)
mmix <- read_csv("MMix.csv")
str(mmix)
fix(mmix)
unique(mmix$`Website Campaign`)
mix <- mmix %>% select(-`Website Campaign`, -NewspaperInserts)

# Here, our target variable is Newvolsales
# Our hypothesis is that sales is dependent on each of the variables (base price, radio advertisement, TV ad, instore pricing, Facebook, Twitter, Website campigns or just online campaigns in general , Newspaper inserts)

# We will try and see the variable-wise impacts on NewVolSales
# Data exploration

# Base Price ----
# Intuitively speaking, Base price should negatively affect NewVolSales
cor(mix$`Base Price`,mix$NewVolSales)
# -0.72.. pretty high negative correlation
# kinda expected
ggplot(mix, aes(`Base Price`,NewVolSales)) + geom_point()
# Understandably, as Base Price increases, sales decreases

# Radio----
cor(mix$Radio, mix$NewVolSales)
# Low correlation of 0.025
ggplot(mix, aes(Radio, NewVolSales)) + geom_point()
# Apparently there seem to be some zeroes in the Radio variable
summary(mix$Radio)
count(mix, mix$Radio==0)
# There are 8 zeroes in the Radio variable
# Lets create another variable Radio1, which would make note of only zeroes
mix$Radio1 <- ifelse(mix$Radio==0, 0,1)
# Now lets analyze those non-zeroes in Radio variable
mix %>% filter(Radio > 0) %>% ggplot(aes(Radio, NewVolSales)) + geom_point()
# There seems to be no evident pattern
mix %>% filter(Radio > 0) %>% summarise(cor = cor(.$NewVolSales, .$Radio))
# Correlation is also low.. 0.19

# Radio1
mix %>% group_by(Radio1) %>% summarise(mean(NewVolSales))
# Mean newvolsales.. not much difference between the 2 groups

# TV ----

cor(mix$TV, mix$NewVolSales)
# Correlation is 0.13... very low
ggplot(data = mix, aes(x = TV, y = NewVolSales)) + geom_point()
# No evident pattern.. just randomly spread points..

# Instore price ----
# Intuitively, there should be a negative relation between Instore pricing and NewVolSales

cor(mix$InStore, mix$NewVolSales)
# 0.44 correlation coefficient.. Thats totally different from our intuition
ggplot(data = mix, aes(x = InStore, y = NewVolSales)) + geom_point()
# Roughly speaking, as instore prices increase, sales tends to increase

# Facebook ----
chisq.test(as.factor(mix$Facebook), mix$NewVolSales)
# No strong association detected
mix %>% group_by(Facebook) %>% summarise(mean(NewVolSales))
# Not much difference between the average sales
mix %>% group_by(Facebook) %>% summarise(avgsales = mean(NewVolSales)) %>% ggplot(aes(x = Facebook, y = avgsales)) + geom_bar(stat = "identity")
mix %>% ggplot(aes(x = Facebook, y = NewVolSales)) + geom_boxplot(aes(group = Facebook))
# Actually, it seems that sales seem to decrease as Facebook campaign 

# Twitter----
mix %>% group_by(Twitter) %>% summarise(mean(NewVolSales))
# There seems to be not much difference in the sales
cor.test(mix$Twitter, mix$NewVolSales)
# No strong association.. Weak correlation (0.19)
# Lets see the visualization
mix %>% ggplot(aes(x = Twitter, y = NewVolSales)) + geom_boxplot(aes(group = Twitter))
# Increase expected and observed if twitter campaign is done

# Website campaign----
cor(mix$NewVolSales, mix$WebCamp)
# Surprisingly, negative correlation..
mix %>% group_by(WebCamp) %>% summarise(mean(NewVolSales))
# If website campaigning is done, the sales volume seems to decrease
mix %>% ggplot(aes(x = WebCamp, y = NewVolSales)) + geom_boxplot(aes(group = WebCamp))
# This negative difference seems to be considerable.. we may see this variable later during our modelling

# Online campaign----
cor(mix$Online, mix$NewVolSales)
# Again, surprisingly negative correlation
mix %>% group_by(Online) %>% summarise(mean(NewVolSales))
mix %>% ggplot(aes(x = Online, y = NewVolSales)) + geom_boxplot(aes(group = Online))
# Slight decrease in sales.. no evident pattern

# Inserts----
cor(mix$NewVolSales, mix$Inserts)
# Very low correlation
mix %>% group_by(Inserts) %>% summarise(mean(NewVolSales))
mix %>% ggplot(aes(x = Inserts, y = NewVolSales)) + geom_boxplot(aes(group = Inserts))
# Not much difference can be observed..

# Linear Modelling
library(car)
mod1 <- lm(formula = NewVolSales~.,data = mix)
summary(mod1)

mod2 <- lm(formula = NewVolSales ~ `Base Price` + InStore + WebCamp, data = mix)
summary(mod2)
# Adjusted R square is 0.71
# 71% of the variation in NewVolSales is explained by Base Price, Instore Price, Website Campaign

# Testing Assumptions of regression
# Variance Inflation factor
vif(mod2)
# Looks good, all of them comfortably below 10

# Constancy of variance check
ggplot(data = NULL, aes(x = mod2$fitted.values, y = mod2$residuals)) + geom_point()
# Funnelled towards the beginning

# Normality check
qqPlot(mod2)
qplot(mod2$residuals, bins = 50)
# Kind-of normal.. need better models

# Square root transformation
mod3 <- lm(formula = sqrt(NewVolSales) ~ ., data = mix)
summary(mod3)

mod4 <- lm(formula = sqrt(NewVolSales)~`Base Price` + InStore + WebCamp, data = mix)
summary(mod4)
# Adjusted R square 0.71

# Testing regression assumptions
# Variance inflation factor
vif(mod4)
# Looks good

# Normality check
qqPlot(mod4)
qplot(mod4$residuals)
# Normal behaviour observed

# Constant variance check
ggplot(data = NULL, aes(x = mod4$fitted.values, y = mod4$residuals)) + geom_point()
# Again funnelled in the beginning

## Log transformation
mod5 <- lm(log(NewVolSales)~`Base Price`+InStore+WebCamp, mix)
summary(mod5)
# Adjusted R square 0.71

# Checking regression assumptions
# Variance inflation factor
vif(mod5)
# Looks good, all less than 10

# Normality check
qqPlot(mod5)
qplot(mod5$residuals, bins = 20)
# Normal behaviour

# Constancy of variance check
ggplot(data = NULL, aes(mod5$fitted.values, mod5$residuals)) + geom_point()
# Still funnelled a bit towards the beginning

# Lets plot the fit-chart
dat <- data.frame(actual = log(mix$NewVolSales), predicted = mod5$fitted.values)
p <- ggplot(dat, aes(x = 1:nrow(dat), y = actual)) + geom_line()
p
p + geom_line(data = dat, color = "blue", aes(y = predicted))
# The model is not a good one.. not a lot of overlaps..