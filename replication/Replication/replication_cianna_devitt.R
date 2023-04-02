########
## Applied Stats II _ Replication
##
## Cianna Devitt (17321885)
#######



#####
### Import Data
#####


data4 <- read.delim("C:/Users/Cianna Devitt/Downloads/Protecting People from Natural Disasters Replication Data.tab")



#####
## Libraries Used
#####
library(pscl)
library(stargazer)
library(MAS)
library(lmtest)
library(AER)


#####
## Models
#####

m1 <- glm.nb(stations ~ ln_distance_km + lnstm_dis + lncoast + pacific + 
               ioc_mship + dipl_rep_num + pop2000_lecz_pc, data = data4)
m2 <- glm.nb(stations ~ ln_distance_km + lnstm_dis + lncoast + pacific + 
               ioc_mship + dipl_rep_num + pop2000_lecz_pc + W + ship_cntc, data = data4)
m3 <- glm.nb(stations ~ capital_sea_50p + Wcapital_sea_50p + lnstm_dis + 
               lncoast + pacific + ioc_mship + dipl_rep_num + pop2000_lecz_pc + W + ship_cntc, data = data4)
m4 <- zeroinfl(stations ~ capital_sea_50p + Wcapital_sea_50p + lnstm_dis + lncoast + pacific + ioc_mship + 
                 dipl_rep_num + pop2000_lecz_pc + ship_cntc + W | W + lnstm_dead + Wlnstm_dead,
               dist = 'negbin', data = data4)



stargazer(m1,m2,m3,m4)



#####
## My Analysis: Negative binmoial vs Poisson model
####




poisson_model <- glm(stations ~ ln_distance_km + lnstm_dis + lncoast + pacific + 
            ioc_mship + dipl_rep_num + pop2000_lecz_pc, family='poisson', data = data4)




summary(poisson_model)
summary(m1)


lrtest(poisson_model, m1)


dispersiontest(poisson_model)
