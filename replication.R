#replicate data for sensitivity analysis

B	<- 10000
k	<- dim(hers)[2]-1
n	<- dim(hers)[1]
y	<- hers[,1]
X	<- as.matrix(cbind(rep(1, n), hers[,c('treatment', 'sbp', 'statins')]))
colnames(X) <- c('intercept', colnames(X)[2:4])

sig			<- rep(0,B)
betamat	<- matrix(0, nrow = B, ncol = k)
yrep		<- matrix(0, nrow = B, ncol = n)
bhat		<- c(solve(t(X)%*%X)%*%(t(X)%*%y))

sig[1]			<- (1/(n-k))*sum((y-X%*%bhat)^2)
betamat[1,]	<- bhat

set.seed(2335)
for (t in 2:B) { 
	# Sample Beta #
	v			<- solve(t(X)%*%X) 
	m			<- v%*%(t(X)%*%y)  # more generally, diag(t0mat)%*%m0
	betamat[t,] <- c(rmvnorm(1, m, sig[t-1]*v)) 

	# Sample Sigma^2 #
	sig[t]		<- rinvgamma(1, n/2, sum((y-X%*%betamat[t-1,])^2)/2)
	
	# Sample y^rep #
	my			<- X%*%betamat[t,]
	yrep[t,]	<- rnorm(n, mean = my, sd = sqrt(sig[t]))
}
colnames(betamat)	<- colnames(X)

betai	<- betamat[(B/2+1):B,]
sigi	<- sig[(B/2+1):B]
yrepi	<- yrep[(B/2+1):B,]

bh		<- matrix(apply(betai, 2, mean), ncol = 1)
obse	<- y - X%*%bh
mean(abs(obse) > 0.2)

repe	<- t(yrepi) - X%*%t(betai)
abse	<- apply(abs(repe) > 0.2, 1, mean)
hist(abse)
abline(v = mean(abs(obse) > 0.2), col = 'blue', lwd = 4)

mean(abse < mean(abs(obse) > 0.2))
