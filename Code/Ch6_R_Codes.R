require(MVTests)
require(mvtnorm)
require(DescTools)
### Dependent samples Hotelling's T2 Test
Pair_T <- read.table("C:/Users/mpatwary/OneDrive - Butler University/BOX_ALL_LATEST/BOX_ALL/S A PATWARY/BUTLER/COURSES/FALL 2021/MA369_MULTIVARIATE STAT ANALYSIS/Lectures/Chapter 6 Comparison of Several Multivariate Means/T6-1.dat", header =  F)
Y1 <- Pair_T[,1:2]; Y2 <- Pair_T[,3:4]

library(MVTests)
result <- Mpaired(T1=Y1,T2=Y2)
summary(result)
names(result)
(Hote_T2 <- result$HT2)
result$F
n <- 11
p <- 2
(Approx_F <- (Hote_T2 * (n-p))/(p * (n-1)))
result$p.value
pf(Approx_F, p, n-p, lower.tail = F)

######################################

### Independent Samples T2 Test
#Install required packages - already done in first 3 lines
#DescTools, mvtnorm, MVTests
wants <- c("DescTools", "mvtnorm", "MVTests")
has   <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has]) #checking if packages installed

## Simulated Data
set.seed(123)
library(mvtnorm)
Nj    <- c(15, 25)
Sigma <- matrix(c(16,-2, -2,9), byrow=TRUE, ncol=2)

mu1   <- c(-4, 4)
Y1    <- round(rmvnorm(Nj[1], mean=mu1, sigma=Sigma))

mu2 <- c(3, 3)
Y2  <- round(rmvnorm(Nj[2], mean=mu2, sigma=Sigma))

Y12 <- rbind(Y1, Y2)
Ind  <- factor(rep(1:2, Nj))

## Using HotellingsT2() from package DescTools
library(DescTools)
HotellingsT2Test(Y12 ~ Ind)

## Using anova.mlm() or manova()
anova(lm(Y12 ~ Ind), test="Hotelling-Lawley")

summary(manova(Y12 ~ IV), test="Hotelling-Lawley")

## Using TwoSamplesHT2() from MVTests Package
# When covariances matrices are homogeneity
results1 <- TwoSamplesHT2(data=Y12,group=Ind,alpha=0.05)
summary(results1)

# When covariances matrices are not homogeneity
results2 <- TwoSamplesHT2(data=Y12,group=Ind,alpha=0.05,Homogenity=FALSE)
summary(results2)
#################################################

## Using Real Data

data(iris)
iris_SV <- iris[1:100,1:4] # 1-50 Setosa, 51-100 Versicolor
Group<-c(rep(1,50),rep(2,50))

# When covariances matrices are homogeneity
Res_Equl_Cov_Mat <- TwoSamplesHT2(data = iris_SV,group = Group, alpha=0.05)
summary(Res_Equl_Cov_Mat)

# When covariances matrices are not homogeneity
Res_UnEqul_Cov_Mat <- TwoSamplesHT2(data = iris_SV, group = Group, alpha=0.05, Homogenity = FALSE)
summary(Res_UnEqul_Cov_Mat)
