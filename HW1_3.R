
install.packages("knitr")
install.packages("markdown")
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
