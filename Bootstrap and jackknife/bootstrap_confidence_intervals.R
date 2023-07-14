heights = c(173, 183, 187, 179, 180, 186, 179, 196, 202, 198, 197, 185, 194, 185,
            191, 182, 182, 187, 184, 186)
B = 1000
alpha = 0.05
n = length(heights)
mean.height = mean(heights)
replicates = replicate(B, mean(sample(heights, size = n, replace = TRUE)))

# basic
basic.ci = 2*mean.height - quantile(replicates, c(1-alpha/2, alpha/2))

# percentile
percentile.ci = quantile(replicates, c(alpha/2, 1- alpha/2))

# standard normal
std.normal.ci = mean.height + qnorm(c(alpha/2, 1- alpha/2))*sd(replicates)

# t
theta.hat = numeric(B)
t.values = numeric(B)
for(b in 1:B){
  x.sample = sample(heights, size = n, replace = TRUE)
  theta.hat[b] = mean(x.sample)
  theta.hat.rep = numeric(B)
  for(j in 1:B){
    theta.hat.rep[j] = mean(sample(x.sample, size = n, replace = TRUE))
  }
  t.values[b] = (theta.hat[b]-mean.height)/sd(theta.hat.rep)
}
t.ci = mean.height - quantile(t.values,(c(1-alpha/2, alpha/2)))

intervals = rbind(basic = basic.ci,
                  percentile = percentile.ci,
                  standard_normal = std.normal.ci,
                  t = t.ci)








