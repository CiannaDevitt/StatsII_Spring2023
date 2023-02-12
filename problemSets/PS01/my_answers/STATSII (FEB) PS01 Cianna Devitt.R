
####
# Applied Stats II - PS01
#
# Cianna Devitt
#
####

####
# Question 1
####

# Create data

set.seed(123)
var <- rcauchy(1000, location = 0, scale = 1)
ECDF <- ecdf(var)
empiricalCDF <- ECDF(var)


KS_TEST <- function(data){
  ECDF <- ecdf(data)
  empiricalCDF <- ECDF(data)
  D <- max(abs(empiricalCDF - pnorm(data)))
  print('One sample Kolmogorov-Smirnoff Test')
  return(print(paste('Test Statistic: ', D)))
}

KS_TEST(var)

ks.test(var, 'pnorm')


####
# Question Two
####

set.seed(123)
data <- data.frame(x = runif(200, 1, 10))
data$y <- 0 + 2.75*data$x +rnorm(200, 0, 1.5)

linear.lik <- function(theta, y, X){
  n <- nrow(X)
  k <- ncol(X)
  beta <- theta[1:k]
  sigma2 <- theta[k +1]^2
  e <- y - X%*%beta
  logl <- -.5*n*log(2*pi) -.5*n*log(sigma2)- ( (t(e) %*%
                                                  e)/ (2*sigma2))
  return(-logl)
}

linear.MLE <- optim(fn=linear.lik, par = c(1,1,1), hessian = TRUE, 
                    y= data$y, X=cbind(1, data$x), method = "BFGS")
linear.MLE$par



summary(lm(y~x, data))

