install.packages('pwr')
install.packages('pwr2')
library(pwr)
library(pwr2)

#Power analysis (n)
pwr.t.test(n = NULL, d = 0.8, sig.level = 0.05, power = 0.8, type='paired')
pwr.t.test(n = NULL, d = 0.8, sig.level = 0.05, power = 0.8, type='paired')
pwr.t.test(n = 89, d = 0.25, sig.level = 0.05, power = NULL)
pwr.2way(a=2, b=2, alpha=0.05, f.A=0.004, f.B=0.3, size.A=58,size.B=200,
        delta.A=NULL, delta.B=NULL, sigma.A=NULL, sigma.B=NULL)
