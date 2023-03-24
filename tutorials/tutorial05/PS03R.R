#####
## Applied Stats II - PS03
## Cianna Devitt
#####


# Loading in packages

library(MASS)
library(nnet)
library(ggplot2)

## Question 1

## Loading in data

gdpChange <- read.csv("C:/Users/Cianna Devitt/Documents/GitHub/StatsII_Spring2023/datasets/gdpChange.csv", header=FALSE)
gdpChange <- read.csv("C:/Users/Cianna Devitt/Documents/GitHub/StatsII_Spring2023/datasets/gdpChange.csv")

gdpChange$GDPWdiff <- factor(ifelse(sign(gdpChange$GDPWdiff) == -1, 'negative',
                        ifelse(sign(gdpChange$GDPWdiff) == 0, 'no change','positive')))


gdpChange$GDPWdiff <- relevel(gdpChange$GDPWdiff, ref = 'no change')


## Running base multinomial logit Unordered

multinom_model1 <- multinom(GDPWdiff ~ REG + OIL,
                            data = gdpChange)

summary(multinom_model1)

## in a given country, there is an increase baseline
## odds of 1.76 that the difference in GDP will be positive.
## 

## Running base multinomial logit ordered

multinom_model2 <- polr(GDPWdiff ~ REG + OIL,
                        data = gdpChange)

summary(multinom_model2)

## Finding proportional odds ratios
ci <- confint(multinom_model2)
confint.default(multinom_model2)
exp(cbind(OR= coef(multinom_model2), ci))
## For a unit increase in democratic status (REG), the odds of GDP difference increase
## by 1.5, holding constant all other variables
## For a unit increase in ratio of fuel exports (OIL),
## the odds of being more likely positive GDP difference is 
## 0.836, holding all other variables constant.

## Question 2

MexicoMuniData <- read.csv("C:/Users/Cianna Devitt/Documents/GitHub/StatsII_Spring2023/datasets/MexicoMuniData.csv")

## Running Poisson Regression Model

Mex_poisson <- glm(PAN.visits.06 ~ competitive.district + marginality.06
                   + PAN.governor.06, data = MexicoMuniData, family = poisson)

summary(Mex_poisson)
coeffs <- coefficients(Mex_poisson)

## With a unit increase in 'competitive district' ie a swing district, had a diminished
## liklihood by 0.08 of having candidate visitations, holding all other variables constant.

## With a unit increase in 'marginality.06' ie a measure of poverty, had a diminished
## liklihood by 2.08 of having candidate visitations, holding all other variables constant.

## With a unit increase in 'PAN.governor.06' ie whetther state has a PAN=affiliated governor, had a diminished
## liklihood by 0.31 of having candidate visitations, holding all other variables constant.
coeffs[1]

lambda30 <- exp(coeffs[1] + coeffs[2]*1)
lambda30
# estimated mean number of visitations: 0.00049