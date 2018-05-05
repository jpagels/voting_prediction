# Load libraries
library(mvtnorm)

# Read in data
voters <- read.csv("processed_voter_data.csv", header = TRUE)

# Remove superfluous variables (faminc_didnotsay, pid_other, employment_other, marstat_other, educ_nocollege)
voters <- voters[,-c(2, 8, 11, 16, 19)]

# Extract Y, get the length of the data, create model matrix
Y	<- voters$voted_clinton
n	<- length(Y)
intercept <- rep(1,n)
X	<- cbind(intercept, voters[,c(2:15)])

# Fit regular glm to get beta_hat and the covariance matrix
fit		<- glm(voted_clinton ~ ., data = voters, family = binomial(link = 'logit'))
bhat	<- coef(fit)
vbeta	<- vcov(fit)
B		<- 40000

beta		<- matrix(0, nrow = B, ncol = ncol(X))
ar			<- vector('numeric', length = B)

beta[1,]	<- bhat

# Note: I'm not exponentiating in this function because it was coming to 0 
# since we had values like exp(-3000)
# However, below, instead of doing r <- tdens(t(bstar), X, Y)/tdens(beta[t-1,], X, Y),
# I'm doing r <- exp(tdens(t(bstar), X, Y) - tdens(beta[t-1,], X, Y))
tdens	<- function(b, X, y){
  X <- as.matrix(X)
  y <- t(as.matrix(y))
  y%*%(X%*%b) - sum(log(1 + exp(X%*%b)))
}

tau	<- 0.42 # need to tune this

for(t in 2:B){
  # This print statement is only here so that we know this is working at a reasonable pace
  if (t %% 1000 == 0) {
    print(t)
  }
  bstar	<- rmvnorm(1, as.matrix(beta[t-1,]), tau*vbeta)
  r		<- exp(tdens(t(bstar), X, Y)-tdens(beta[t-1,], X, Y)) 
  U		<- runif(1)
  if(U < min(1,r)){
    beta[t,]	<- bstar
    ar[t]		<- 1
  } else {
    beta[t,]	<- beta[t-1,]
    ar[t]		<- 0
  }
  
}

# Check acceptance rate
mean(ar)
t(apply(beta, 2, quantile, probs = c(0.025, 0.5, 0.975)))

# Plot diagnostic plots
# Note: there is significant autocorrelation so we have to thin by around 50 to get rid of 
# most of the autocorrelation
library(mcmcplots)

beta0_thinned <- as.matrix(beta[(B/2+1):B,1][seq(from = 1, to = B/2, by = 50)])
colnames(beta0_thinned) <- 'beta[0]'
beta1_thinned <- as.matrix(beta[(B/2+1):B,2][seq(from = 1, to = B/2, by = 50)])
colnames(beta1_thinned) <- 'beta[1]'

mcmcplot1(beta0_thinned, greek = TRUE)
mcmcplot1(beta1_thinned, greek = TRUE)
