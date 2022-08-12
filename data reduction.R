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
# y2 Cooling Load
df = read.csv('ENB2012_data.csv')

##### clean
df = subset(df, select = -c(X, X.1)) # drop redundant columns
names(df)[names(df) == 'ï..X1'] = 'X1' # rename one column
df = na.omit(df) # drop na rows
df = df[colnames(df) != 'Y2'] # drop Y2, we are doing only hearing 

n = nrow(df) # total rows of raw data
n

# reduce data rows into 500 data rows only
mBefore = lm(data = df, formula = Y1 ~ .) # make a MLR with raw data

temp = sort(abs(mBefore$residuals), decreasing = TRUE) # residual
index = names(temp[1:(n - 500)]) # extract the data index with top residual
index = as.numeric(index)
dfReduced = df[-index, ] # remove the data causing big residual, reduce the data into 500 rows
nrow(dfReduced) # check data rows after reducing

mAfter = lm(data = dfReduced, formula = Y1 ~ .) # make a MLR with reduced data


par(mfrow = c(1,2))
plot(mBefore, which = 1)
plot(mAfter, which = 1)

summary(mBefore)
sum(mBefore$residuals^2) # res SS

summary(mAfter)
sum(mAfter$residuals^2) # res SS


write.csv(dfReduced, file = 'reducedData.csv') # save the 500 data rows into a csv file





