#####################
# Applied Stats II
# ProblemSet 2
# Cianna Devitt
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)


lapply(c(),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

# load data
load(url("https://github.com/ASDS-TCD/StatsII_Spring2023/blob/main/datasets/climateSupport.RData?raw=true"))

climateSupport$choice <- as.numeric(ifelse(climateSupport$choice == 'Supported', 1,0))



model <- glm(choice ~ sanctions + countries, 
             family = binomial(link = 'logit'), 
             data = climateSupport)


summary(model)



model_null <- glm(choice ~ 1, 
                  family = binomial(link = 'logit'), 
                  data = climateSupport)
anova(model_null, model, test = 'LRT')


model_int <- glm(choice ~ sanctions * countries, 
                 family = binomial(link = 'logit'), 
                 data = climateSupport)
summary(model_int)

anova(model_null, model_int, test = 'LRT')

1-pchisq((11783-11568)/(8499-8494))


1/1 + exp(0.005 + ((-0.27)*(80/192))) 
