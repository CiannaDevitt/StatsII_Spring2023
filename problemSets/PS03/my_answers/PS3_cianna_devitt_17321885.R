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


## Question 2

MexicoMuniData <- read.csv("C:/Users/Cianna Devitt/Documents/GitHub/StatsII_Spring2023/datasets/MexicoMuniData.csv")

## Running Poisson Regression Model

Mex_poisson <- glm(PAN.visits.06 ~ competitive.district + marginality.06
                   + PAN.governor.06, data = MexicoMuniData, family = poisson)

summary(Mex_poisson)
coeffs <- coefficients(Mex_poisson)



lambda30 <- exp(coeffs[1] + coeffs[2]*1)
lambda30
# estimated mean number of visitations: 0.00049