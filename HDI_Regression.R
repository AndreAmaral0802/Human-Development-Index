# HDI analysis 
# Run a linear regression and then K-means Clustering (Divide the countries by debt)

df <- read.csv('HDI_analysis_2.csv', row.names='Country')
head(df)
str(df)
tail(df)

# Statistical and Visualization summary
library(ggplot2)
summary(df)

ggplot(data = df) + geom_point(mapping = aes(x = GDP.USD, y = HDI))
qplot(GDP.USD,HDI,data=df,color=HDI)

ggplot(data = df) + geom_point(mapping = aes(x = GDP.per.capta.USD, y = HDI))
qplot(GDP.per.capta.USD,HDI,data=df,color=HDI)

ggplot(data = df) + geom_point(mapping = aes(x = Debt.to.GDP, y = HDI))
qplot(Debt.to.GDP,HDI,data=df,color=HDI)

ggplot(data = df) + geom_point(mapping = aes(x = Inflation, y = HDI))
qplot(Inflation,HDI,data=df,color=HDI)

qplot(Debt.to.GDP,HDI,data=df,color=HDI)

qplot(Unemp,HDI,data=df,color=HDI)

qplot(Interest.rate,HDI,data=df,color=HDI)

qplot(ave.Inc.tax,HDI,data=df,color=HDI)

qplot(Life.Expectancy,HDI,data=df,color=HDI)

qplot(Expected.years.of.schooling,HDI,data=df,color=HDI)

# Filter to numeric columns for correlation
cor.data <- cor(df[,1:10])
print(cor.data)

library(corrplot)
library(corrgram)
corrplot(cor.data,method='color')

corrgram(df,order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt)

# Import Library
library(caTools)
# Split up the sample, basically randomly assigns a booleans to a new column "sample"
sample <- sample.split(df$HDI, SplitRatio = 0.70) # SplitRatio = percent of sample==TRUE

# Training Data
train = subset(df, sample == TRUE)

# Testing Data
test = subset(df, sample == FALSE)

# linear regression
model <- lm(HDI ~ GDP.USD + GDP.per.capta.USD + Inflation + Debt.to.GDP + Interest.rate + Unemp + ave.Inc.tax
            + Life.Expectancy + Expected.years.of.schooling,train)
summary(model)

# Grab residuals
res <- residuals(model)

# Convert to DataFrame for gglpot
res <- as.data.frame(res)

head(res)
str(res)
# Histogram of residuals
ggplot(res,aes(res)) +  geom_histogram(fill='blue',alpha=0.5)

plot(model)

# Checking the residuals using box.test for white noise
Box.test(res, lag = 12, fitdf = 0, type = "Lj")

library(forecast)
checkresiduals(naive(df$HDI))

HDI_pred <- predict(model,test)

results <- cbind(HDI_pred,test$HDI) 
colnames(results) <- c('predicted','real')
results <- as.data.frame(results)
results

# prediction

a = 4.389e-01
B1 = 5.328e-07
X1 = 48754.39 # current GDP per capita is 47334.36 if we increase 3% = 14200 = 48754.39
B2 = 3.729e-03
X2 = 82.357 # current life exp 80.7422 if we increase by 2% = 1.6148 = 82.357
B3 = 9.754e-03
X3 = 17.6559 # current years of schooling is 17.30971909 if we increase by 2 % = 0.3462 = 17.6559
e = 0.01322

Y_hat = a + B1*X1 + B2*X2 + B3*X3 + e
Y_hat # positive

Y_hat_2 = a + B1*X1 + B2*X2 + B3*X3 - e
Y_hat_2

# Seperate low debts to high debt level
Debt <- function(Debt_GDP){
  if(Debt_GDP > 60){
    return('High debt level')
  }else if(Debt_GDP < 60){
    return('Low debt level')
  }else{
    return(edu)
  }
}

# apply the function 
df$Debt.to.GDP <- sapply(df$Debt.to.GDP,Debt)
table(df$Debt.to.GDP)
df$Debt.to.GDP
