setwd('C:/Users/Lee/iCloudDrive/Document/Boston University/CS555 Data with R/Final Project')
getwd()

##### read csv
# X1 Relative Compactness
# X2 Surface Area
# X3 Wall Area
# X4 Roof Area
# X5 Overall Height
# X6 Orientation
# X7 Glazing Area
# X8 Glazing Area Distribution
# y1 Heating Load
dfReduced = read.csv('reducedData.csv')
dfReduced = dfReduced[colnames(dfReduced) != 'X'] # drop the index columns

##### My best selection
# correlation
# check correlation matrix first
library(psych)
pairs.panels(dfReduced, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

# do t-test on Correlation on X6, X7, X8, based on alpha = 0.95
a = 0.95

temp = cor.test(x = dfReduced$X6, y = dfReduced$Y1)
print(temp$p.value)
temp$p.value < (1 - a) / 2 # if False, fail to reject the null hypothesis

temp = cor.test(x = dfReduced$X7, y = dfReduced$Y1)
print(temp$p.value)
temp$p.value < (1 - a) / 2 # if False, fail to reject the null hypothesis

temp = cor.test(x = dfReduced$X8, y = dfReduced$Y1)
print(temp$p.value)
temp$p.value < (1 - a) / 2 # if False, fail to reject the null hypothesis

# after the t-test, we should drop X6 because there is no correlation between X5 and Y1
drop = c('X6') # drop three columns
dfReduced1 = dfReduced[, !(names(dfReduced) %in% drop)]
colnames(dfReduced1) # check if droping successfully

pairs.panels(dfReduced1[, 1:7], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

# X1 has -0.99 correlated to X2
# drop one of them, drop X2
a = 0.95
temp = cor.test(x = dfReduced1$X1, y = dfReduced1$X2)
print(temp$p.value)
temp$p.value < (1 - a) / 2 # if False, fail to reject the null hypothesis
drop = c('X2') # drop three columns
dfReduced1 = dfReduced1[, !(names(dfReduced1) %in% drop)]
colnames(dfReduced1) # check if droping successfully

# X4 has -0.98 correlated to X5
# drop one of them, drop X5
a = 0.95
temp = cor.test(x = dfReduced1$X1, y = dfReduced1$X2)
print(temp$p.value)
temp$p.value < (1 - a) / 2 # if False, fail to reject the null hypothesis
drop = c('X5') # drop three columns
dfReduced1 = dfReduced1[, !(names(dfReduced1) %in% drop)]
colnames(dfReduced1) # check if droping successfully

# check how much improvement we achieve
# mBefore is using the raw data
# mAfter is using the data after removing top residual, and reducing the data down to 500
# mAfterClean is using the data after dropping irrelevant columns
mAfterClean = lm(data = dfReduced1, formula = Y1 ~ .) # make a linear model for fully cleaned dataset

summary(mAfterClean)
sum(mAfterClean$residuals^2) # res SS

# compare between 
# check if mAfter and mAfterClean have a significant difference
alpha = 0.95

anova(mAfter, mAfterClean)$`Pr(>F)`[2] < (1 - alpha) # if True, reject the null hypothesis

par(mfrow = c(1,2))
plot(mAfter, which = 1)
plot(mAfterClean, which = 1)


##### MLR modeling comparison
# dfReduced is the dataset with only 500 data rows

# my best
myBest = lm(data = dfReduced, formula = Y1 ~ X1 + X3 + X4 + X7 + X8)
summary(myBest)
# forward
m2 = lm(data = dfReduced, formula = Y1 ~ 1)
mForward = step(m2, direciton = 'forward', scope = ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8)
summary(mForward)
sum(mForward$residuals^2)
# backward
m3 = lm(data = dfReduced, formula = Y1 ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8)
mBackward = step(m3, direction = 'backward')
summary(mBackward)
sum(mBackward$residuals^2)


