n = 15
m = 10000
alpha = .05
mu0 = 12
my_pt <- function(x, df){
  return(2*min((1-pt(x, df)), pt(x,df)))
}
s0 = 1
set.seed(1984)
samples = matrix(rnorm(n*m, mu0, s0), nrow = m, byrow = T)
std = apply(samples, 1, sd)
ts = (apply(samples, 1, mean) - mu0)/(std/sqrt(n))
is = sapply(ts, FUN = my_pt, df = 14)
error = mean(is<alpha)
se = sqrt(error*(1-error)/m)
