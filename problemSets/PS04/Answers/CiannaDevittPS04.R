#####
## Applied Stats II - PS04
##
## Cianna Devitt 17321885
#####



#####
## Importing data + Packages
#####


library(eha)
library(survival)
data <- child


#####
## Estimating a Cox PH model
####

add_surv <- coxph(Surv(enter, exit, event) ~ m.age + sex, data=data)
summary(add_surv)

#####
## Plotting Cox Probability Hazard Function
####


plot_coxph <- coxreg(Surv(enter, exit, event) ~ m.age + sex, data=data)
plot(plot_coxph, main = 'Cox PH plot')

#####
## Assessing Model Quality
#####

drop1(add_surv, test = 'Chisq')
