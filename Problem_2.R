## Homework 2 / Problem 2
## Problem description:
## https://firebasestorage.googleapis.com/v0/b/uni-sofia.appspot.com/o/homeworks%2FHomework_3.pdf?alt=media

## Install the following packages if necessary. The following line must be commented when you submit.
## install.packages(c('ggplot2', 'dplyr'))

library(dplyr)
library(ggplot2)

# Enter your student ID in set.seed() so that your results are reproducible
## Replace 1 with your student Id. One student id from anyone in the group is
## sufficient.
set.seed(45345)

# use the rnorm function to generate values from the standard normal distribution
# you can compute e^x with the exp function, e.g. e^2 = exp(2).

## a)
mu <- 1
sigma <- 1.3
popmean <- exp(mu + sigma ^ 2 /2)
popvar <- (exp(sigma ^ 2) - 1) * exp(2 * mu + sigma ^ 2)
B <- 5000
n <- 20
z <- rnorm(n * B)
x <- exp(mu + sigma * z)

## b)

samplesData <- data.frame(
  minutes = x, 
  r = rep(1:B, each = n)
)

groupedSamples <- group_by(samplesData, r)
samplesSummary <- summarise(
  groupedSamples,
  sampleMean = mean(minutes),
  sampleStdDev = sd(minutes)
)

samplesData <- within(samplesSummary, {
  lowerLimit <- sampleMean - qt(0.95, df = n - 1) * sampleStdDev / sqrt(n)
  upperLimit <- sampleMean + qt(0.95, df = n - 1) * sampleStdDev / sqrt(n)
  inCI <- popmean >= lowerLimit & popmean <= upperLimit
})

## c)
ggplot(data = samplesData[1:20, ], aes(x = r)) + 
  geom_errorbar(aes(ymin = lowerLimit, ymax = upperLimit)) +
  geom_hline(yintercept = popmean, linetype = 2, color = "dark blue") 

## d)
mean(samplesData$inCI)

## e)

## Yes, because the nominal coverage is 90% and we see
## that the population mean was within the CI in only 76.7% () of the samples.

## The formula for the confidence intervals
## is based on the assumption that:
## 1) The observations are normally distributed. This is obviously 
## not true because you sample from a log-normal distribution which is
## heavily skewed.
## OR 
## 2) that the sample size is large which is also not 
## true because the sample size is n = 20.

## f, g, h, i)

testFalseH0OneSided <- function(y) {
  testResult <- t.test(y, mu = 5, alternative = 'greater')
  return(testResult$p.value < 0.05)
}

testFalseH0TwoSided <- function(y) {
  TestResult2 <- t.test(y, mu = 5, alternative = 'two.sided')
  return(TestResult2$p.value < 0.05)
}

testTrueH0TwoSided <- function(y) {
  TestResult3 <- t.test(y, mu = 6.3281, alternative = 'two.sided')
  return(TestResult3$p.value < 0.05)
}

testResults <- summarise(
  groupedSamples,
  trueRejectionOneSided = testFalseH0OneSided(minutes),
  trueRejectionTwoSided = testFalseH0TwoSided(minutes),
  falseRejection = testTrueH0TwoSided(minutes)
)

## Share of samples with rejected H0

mean(testResults$trueRejectionOneSided)
mean(testResults$trueRejectionTwoSided)


## The first test performed better, beacuse we reject
## the false null hypothesis more times
## mean(testResults$trueRejectionTwoSided) > mean(testResults$trueRejectionOneSided)


mean(testResults$falseRejection)

## The null hypothesis in this case is true
## so we expect the test to falsly reject it
## is 5% of the samples.
## However, this is not the case here as we see that the test rejects
## the null hypothesis in mean(testResults$falseRejection) of the cases
## which is considerably higher.

## See de discussion about the confidence intervals in e)
