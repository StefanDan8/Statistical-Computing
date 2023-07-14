alpha = 0.05
set.seed(512021)
conf = 1 - alpha
my_var = 1
nsims = 100
n = 10
chi_lower = qchisq(alpha/2, n-1)
chi_upper = qchisq(1-alpha/2, n-1)
sims = matrix(rnorm(n*nsims, 0, my_var), nrow = nsims)
vars = apply(sims, 1, var)
lowers = vars*(n-1)/chi_upper
uppers = vars*(n-1)/chi_lower
conf_level = mean((lowers<=my_var) & (my_var<=uppers))
se = sqrt(conf_level*(1-conf_level)/nsims)
round(cbind(conf, conf_level, se),3)
deviations = abs(conf_level - conf)/se
deviations
