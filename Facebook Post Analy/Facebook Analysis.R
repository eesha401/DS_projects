
facebook = read.csv('dataset_Facebook.csv', header = TRUE)

#Post.Month redo  
facebook$MonthLevel = facebook$Post.Month 
facebook$MonthLevel[facebook$Post.Month >= 1 & facebook$Post.Month <= 3] = "Quarter1"
facebook$MonthLevel[facebook$Post.Month >= 4 & facebook$Post.Month <= 6] = "Quarter2"
facebook$MonthLevel[facebook$Post.Month >= 7 & facebook$Post.Month <= 9] = "Quarter3"
facebook$MonthLevel[facebook$Post.Month >= 10 & facebook$Post.Month <= 12] = "Quarter4"

#Post.Hour redo 
facebook$HourLevel = facebook$Post.Hour 
facebook$HourLevel[facebook$Post.Hour >= 1 & facebook$Post.Hour <= 6] = "Hour_Quarter1"
facebook$HourLevel[facebook$Post.Hour >= 7 & facebook$Post.Hour <= 12] = "Hour_Quarter2"
facebook$HourLevel[facebook$Post.Hour >= 13 & facebook$Post.Hour <= 18] = "Hour_Quarter3"
facebook$HourLevel[facebook$Post.Hour >= 19 & facebook$Post.Hour <= 24] = "Hour_Quarter4"

#Post.Weekday redo
facebook$DayLevel = facebook$Post.Weekday 
facebook$DayLevel[facebook$Post.Weekday >= 2 & facebook$Post.Weekday <= 6] = "Weekday"
facebook$DayLevel[facebook$Post.Weekday ==1 | facebook$Post.Weekday ==7] = "Weekend"

attach(facebook)

Type = factor(Type)
Category = factor(Category)
MonthLevel = factor(MonthLevel) 
HourLevel = factor(HourLevel)  
DayLevel = factor(DayLevel)  
Paid = factor(Paid)

summary(facebook)

#model fit 
mlrfit = lm(Total.Interactions ~ Page.total.likes+ Type + factor(Category) + MonthLevel + HourLevel + DayLevel + Paid, data = facebook)
summary(mlrfit)
plot(mlrfit)
library(car)
vif(mlrfit)

logfit = lm((Total.Interactions)^(0.5) ~ Page.total.likes+ Type + factor(Category) + MonthLevel + HourLevel + DayLevel + factor(Paid), data = facebook)
summary(logfit)
plot(logfit)
anova(logfit)
vif(logfit)

confint(logfit)

library(MASS)
boxcox(logfit)

cooks = cooks.distance(logfit)
sort(cooks)
qf(0.5, 7, 492)
cooks[cooks>0.9077854]

leverage = hatvalues(logfit)
leverage[leverage > (2*7/499)]

sres = studres(logfit)
sres[sres > 4]

## reduced model 

redfit = logfit = lm((Total.Interactions)^(0.5) ~ Page.total.likes+ Type + factor(Category) + HourLevel + DayLevel + factor(Paid), data = facebook)
summary(redfit)
anova(redfit)


#boxplots 
boxplot(Total.Interactions ~ Type)
boxplot(Total.Interactions ~ Category)
boxplot(Total.Interactions ~ MonthLevel)
boxplot(Total.Interactions ~ HourLevel)
boxplot(Total.Interactions ~ DayLevel)
boxplot(Total.Interactions ~ Paid)

