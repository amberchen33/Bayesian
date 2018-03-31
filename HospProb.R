rm(list=ls())
dev.off()

alpha=16
beta=15174
yobs=1
exp=66

lambdaA=rgamma(1000,shape=alpha+yobs,rate=beta+exp)



yobs=4
exp=1767

lambdaB=rgamma(1000,shape=alpha+yobs,rate=beta+exp)

lambda=seq(0,max(c(lambdaA,lambdaB)),length=500)
par(mfrow=c(2,1))
hist(lambdaA,freq=FALSE,main="",ylim=c(0,1500))
lines(lambda,dgamma(lambda,shape=alpha,rate=beta))
hist(lambdaB,freq=FALSE,main="",ylim=c(0,1500))
lines(lambda,dgamma(lambda,shape=alpha,rate=beta))