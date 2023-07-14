times = c(3,5,7, 18, 43, 85, 91, 98, 100, 130, 230, 487)
B = 200
n = length(times)
replicates = numeric(B)
lambdahat = n/sum(times)
for(k in 1:B){
  i = sample(1:n, size = n, replace = TRUE)
  replicates[k] = n/sum(times[i])
}
bias = mean(replicates)-lambdahat
se = sd(replicates)
hist(replicates)
abline(v = lambdahat)
