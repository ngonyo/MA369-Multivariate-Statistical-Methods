### Chapter 6: Inference about a Mean Vector ####
require(DescTools)
## Hotelling T^2 test

X1 <- c(6, 10, 8)
X2 <- c(9, 6, 3)
Y <- cbind(X1, X2) # Data Matrix
n <- nrow(Y)
p <- ncol(Y)

xbar <- colMeans(Y)
S <- cov(Y)

mu0 <- c(9, 5) # Null hypothesis
#########################################

## Hotelling T2 using formula
(T2 <- n * t(xbar - mu0) %*% solve(S) %*% (xbar - mu0))
(Fstat <- (T2 * (n - p))/(p * (n-1))) # approx F in the following anova codes
(pval <- 1 - pf(Fstat, df1=p, df2= n-p)) # default: lower.tail = TRUE
##########################################

# Using HotellingsT2() from package DescTools
library(DescTools)
HotellingsT2Test(Y, mu=mu0)

###########################################

## Using anova.lm()
Ycentr  <- sweep(Y, 2, mu0, "-") #2 represents column index. 1 would represent row index. sweep function subtracts corresponding mean from each column
# subtract the mean from each variable from each ovservation of that variable
anova(lm(Ycentr ~ 1), test="Hotelling-Lawley")

y <- Y - matrix(mu0, n, p,byrow=T) # alternative to sweep function
anova(lm(y ~ 1))
###########################################

## Check whether in the ellipse 
alpha <- 0.05
mu <- c(8, 6)
C2 <- (p * (n-1)/(n-p)) * qf(alpha, df1=p, df2= n-p)
(T2 <- n * t(xbar - mu) %*% solve(S) %*% (xbar - mu))
T2 < C2
# Since T2 is less than C2, mu is in the 95% confidence ellipsoid.
# Fail to reject the H0.
############################################

## 95% Simultaneous CI for mu1 and mu2
a1 <- c(1, 0)
mu1L <- t(a1)%*%xbar - sqrt(C2/n * t(a1) %*% S %*% a1)
mu1U <- t(a1)%*%xbar + sqrt(C2/n * t(a1) %*% S %*% a1)
(Mu1Simul<- c(mu1L, mu1U))

a2 <- c(0, 1)
mu2L <- t(a2)%*%xbar - sqrt(C2/n * t(a2) %*% S %*% a2)
mu2U <- t(a2)%*%xbar + sqrt(C2/n * t(a2) %*% S %*% a2)
(Mu2Simul<- c(mu2L, mu2U))
###########################################

## Classical 95% t CI
Mu1Clas <- c(xbar[1] - sqrt(S[1,1]/n) * qt(1-alpha/2, df=n-1),
             xbar[1] + sqrt(S[1,1]/n) * qt(1-alpha/2, df=n-1))
Mu1Clas

Mu2Clas <- c(xbar[2] - sqrt(S[2,2]/n) * qt(1-alpha/2, df=n-1),
             xbar[2] + sqrt(S[2,2]/n) * qt(1-alpha/2, df=n-1))
Mu2Clas

c(diff(Mu1Simul), diff(Mu1Clas)) 
# Clearly classical CI for mu1 is much wider than Simultaneous one

c(diff(Mu2Simul), diff(Mu2Clas))
# Clearly classical CI for mu2 is much wider than Simultaneous one
############################################

## Bonferroni 95% CI
alpha.adj <- alpha/p
Mu1Bon <- c(xbar[1] - sqrt(S[1,1]/n) * qt(1-alpha.adj/2, df=n-1),
            xbar[1] + sqrt(S[1,1]/n) * qt(1-alpha.adj/2, df=n-1))
Mu1Bon

Mu2Bon <- c(xbar[2] - sqrt(S[2,2]/n) * qt(1-alpha.adj/2, df=n-1),
            xbar[2] + sqrt(S[2,2]/n) * qt(1-alpha.adj/2, df=n-1))
Mu2Bon

c(diff(Mu1Simul), diff(Mu1Clas), diff(Mu1Bon)) 

c(diff(Mu2Simul), diff(Mu2Clas), diff(Mu2Bon))

