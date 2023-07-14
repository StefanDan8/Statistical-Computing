n=15
nsims = 10000
alpha = 0.05
mu0 = 12
sd0 = 1
pvals = numeric(nsims)
set.seed(1984)
for(j in 1:nsims){
  x = rnorm(n, mu0, sd0)
  pvals[j]=t.test(x, mu = mu0)$p.value
}
error = mean(pvals<alpha)
se = sqrt(error*(1-error)/nsims)
round(cbind(error, se),3)
