# problem 3
# a
rm(list=ls())
data = seq(50,100,0.05)
speed = 70
sd  = 10
post = c()
for (i in 1: length(data)){
  tmp = data[i]
  prior = pnorm(70,tmp, 10)
  post[i] = prior*(1-prior)^17
}
post
sum(post)
postprob = post/sum(post)
plot(main ='post',data,postprob,lwd =0.8,type= 'l',col='red')

# b 
postMean = sum(postprob*data)

# c
cumulative = 0
for (i in 1:length(data)){
  if (data[i]>80){
    break
  }
  
  cumulative = cumulative+postprob[i]
}
# the probability that average speed exceeds 80 is
1 - cumulative





