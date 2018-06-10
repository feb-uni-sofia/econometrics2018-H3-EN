## Homework 3 / Problem 1
## Problem description:
## https://firebasestorage.googleapis.com/v0/b/uni-sofia.appspot.com/o/homeworks%2FHomework_3.pdf?alt=media

## Install the following packages if necessary. Uncomment the following line and execute it.
## install.packages(c('dplyr', 'ggplot2'))

## Download and read the data
wines <- read.csv(file = 'https://s3.eu-central-1.amazonaws.com/sf-timeseries/data/wine.csv')

## a)
wines <- within(wines, {
  Price <- exp(LogPrice)
  tempclass <- ifelse(Temperature > mean(Temperature), 'hot', 'cold')
})

## b)
muH0 <- 120

t.test(wines$HarvestRain, mu = muH0, alternative = 'greater')

## We reject the null hypothesis because
## the p-values = 0.04468 is smaller than 
## 0.05. I.e. it is not likely to observe
## a value of the test-statisticof t = 1.76 or greater
## if the null hypothesis is true.

## c)
n <- nrow(wines)

criticalUpper <- qt(0.99, df = n - 1)

testStatistic <- sqrt(n) * ((mean(wines$HarvestRain) - muH0) / sd(wines$HarvestRain))
testStatistic > criticalUpper
pvalue <- (1 - pt(testStatistic, df = n - 1))
pvalue
## We cannot reject the null nypothesis 
## at a 99% significance level (1% error probability)
## because the p-value is larger than 0.01
