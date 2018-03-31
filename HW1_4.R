#2.Suppose that the experiment is repeated ten times and Bob is corrected six times and
#incorrect four times. Find the posterior probabilities of these values of p. What is your
#posterior probability that Bob has no ability?

p_input <- c(0.000, 0.125, 0.250, 0.375, 0.500, 0.625, 0.750, 0.875, 1.000)
pp_input <- c(0.001, 0.001, 0.950, 0.008, 0.008, 0.008, 0.008, 0.008, 0.008)

n=10
x=6

Pr = numeric()
for(i in 1:length(p_input)){
  print(i)
  p = p_input[i]
  pp = pp_input[i]
  Pr_one = factorial(n)/factorial(6)/factorial(4)*p^x*(1-p)^(n-x)*pp
  Pr = c(Pr,Pr_one)
}
post_pp = Pr/sum(Pr)
plot(post_pp)
sum(post_pp)
cbind(p_input,pp_input,post_pp)

#3.Compute the posterior distribution of . Plot this distribution, as was done in class, and
#report the posterior mean, standard deviations, and 95% credible interval.

m_input <- c(20, 30, 40, 50, 60, 70)
mm_input <- c(0.1, 0.15, 0.250, 0.25, 0.15, 0.1)
y_n <- c(38.6, 42.4, 57.5, 40.5, 51.7, 67.1, 33.4, 60.9, 64.1, 40.1, 40.7, 6.4)
y_mean <- mean(y_n)
sigma=10
Pr = numeric()

for(i in 1:length(m_input)){
  print(i)
  m = m_input[i]
  mm = mm_input[i]
  a <- 6/(2*100)*(m_input-y_mean)^2
  Pr_one =  exp(-a) *mm
  Pr = c(Pr,Pr_one)
}
post_mm = Pr/sum(Pr)
plot(post_mm)
sum(post_mm)
cbind(m_input,mm_input,post_mm)
sd(post_mm)
quantile(sd(post_mm), c(0.025, 0.975)) # for a 95% interval.
#4.(a) If 12 trucks break down in a six day period, find the posterior probabilities for the
#different rate values.
L_input <- c(0.5, 1, 1.5, 2, 2.5, 3)
LL_input <- c(0.1, 0.2, 0.3, 0.2, 0.15, 0.05)
t=6
Pr = numeric()

for(i in 1:length(L_input)){
  print(i)
  L = L_input[i]
  LL = LL_input[i]
  Pr_one =  (exp(-t*L))^12 *LL
  Pr = c(Pr,Pr_one)
}
post_LL = Pr/sum(Pr)
plot(post_LL)
sum(post_LL)
cbind(L_input,LL_input,post_LL)

#(b) Find the probability that there are no breakdowns during the next week.

