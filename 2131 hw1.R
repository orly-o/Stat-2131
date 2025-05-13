# Question 4

pbar = .496
p0 = .5
n = 7594000000
z = (pbar-p0)/sqrt(p0*(1-p0)/n)
z

alpha = .05
z.half.alpha = qnorm(1-alpha/2)
c(-z.half.alpha, z.half.alpha)

pval = 2*pnorm(z, lower.tail = FALSE)
pval

c(.496 - (z*sqrt(.25/n)), .496 + (z*sqrt(.25/n)))

# Question 5

