library(bootstrap)
attach(law)
n = length(law[,1])               # sample size
alpha = 0.05
set.seed(1152021)
B = 2000                          # number of bootstrap simulations
boot.replicates = numeric(B)
jack.replicates = numeric(n)
cor.hat = cor(GPA,LSAT)           # estimate based on data
for(b in 1:B){
  i = sample(1:n, size = n, replace = TRUE)   # get a bootstrap sample
  boot.replicates[b] = cor(GPA[i], LSAT[i])   # compute the sample estimate of correlation
}
for(j in 1:n){                                #compute jackknife estimates
  jack.replicates[j] = cor(GPA[-j], LSAT[-j])
}
# bias correction
z0.hat = qnorm(mean(boot.replicates<cor.hat))   # median bias of bootstrap replicates
# skewness acceleration
cor.jack.mean = mean(jack.replicates)
a.hat = sum((cor.jack.mean - jack.replicates)^3)/(6*(sum((cor.jack.mean - jack.replicates)^2))^(3/2))

# interval bounds
alpha1 = pnorm(z0.hat+(z0.hat+qnorm(alpha/2))/(1-a.hat*(z0.hat+qnorm(alpha/2))))
alpha2 = pnorm(z0.hat+(z0.hat+qnorm(1-alpha/2))/(1-a.hat*(z0.hat+qnorm(1-alpha/2))))
bca.ci = quantile(boot.replicates, c(alpha1,alpha2))
