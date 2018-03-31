rm(list=ls())
library("LearnBayes")
data(footballscores)
attach(footballscores)
d=favorite-underdog-spread
n=length(d)
v=sum(d^2)

P=rchisq(1000,n)/v
s=sqrt(1/P)
hist(s,main="")

print("Post mean")
postmean=print(mean(s))
print("95% credible interval")
print(quantile(s,probs=c(0.025,0.975)))
