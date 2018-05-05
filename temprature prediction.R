df <- read.csv('weatherHistory.csv')
##head(df)
##df2 = subset(df, select = -c(Formatted.Date))   ###removing date column as it had many factors
# head(df2)

                ###dealing with missing data
#any(is.na(df2))


#### checking the correlation between the values

##we need numerical valyes only...

num.cols <- sapply(df2, is.numeric)

###numerical values separated


df2 <- subset(df,select = -c(Pressure..millibars.,Loud.Cover))
cor.data <- cor(df2[,num.cols])
print(corrplot(cor.data,method = 'color'))

## from the plot we get to know there is a strong corelation between temperature and humidity
## and a moderate negative corelation between humidity and visibility

df3 <- subset(df2,select = -c(Daily.Summary,Formatted.Date,Summary,Precip.Type))
print(df3)

####removed some more categorical data###

#################SPLITING DATA IN TRAINING AND TESTING SETS#############

set.seed(105)    ###generating random data points
sample.data <- sample.split(df3,SplitRatio = 0.7)   

train <- subset (df3,sample =TRUE)
test <- subset(df3, sample = FALSE)
 

my.model <- lm(Apparent.Temperature..C. ~.,train)
print(summary(my.model))
final.prediction <- predict(my.model,test)
result <- cbind(final.prediction,test$Apparent.Temperature..C.)
colnames(result) <- c("prediction","actual")
print(head(result))




