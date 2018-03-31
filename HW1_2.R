Sys.setenv(PATH = paste(Sys.getenv("PATH")

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

