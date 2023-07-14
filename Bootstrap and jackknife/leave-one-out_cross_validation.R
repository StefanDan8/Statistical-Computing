library(DAAG)
attach(ironslag)
n = length(chemical)
elin = equad = eexp = eloglog = numeric(n)
for(k in 1:n){
  x = chemical[-k]
  y = magnetic[-k]
  c = coef(lm(y~x))
  elin[k] = magnetic[k] - (c[1]+c[2]*chemical[k])
  c = coef(lm(y~x+I(x^2)))
  equad[k] = magnetic[k] - (c[1]+c[2]*chemical[k] + c[3]*chemical[k]^2)
  c = coef(lm(log(y)~x))
  eexp[k] = magnetic[k] - exp(c[1]+c[2]*chemical[k])
  c = coef(lm(log(y)~log(x)))
  eloglog[k] = magnetic[k] - exp(c[1]+c[2]*log(chemical[k]))
}
round(cbind(linear = mean(elin^2),  # 19.556
            quadratic = mean(equad^2), # 17.852
            exponential = mean(eexp^2), # 18.442
            loglog = mean(eloglog^2)),3) # 20.454
