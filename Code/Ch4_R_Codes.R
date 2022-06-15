## Chapter 4 R Codes ##

#Install the MVN package
install.packages("MVN")

#load the MVN package
require(MVN)

#The package includes the famous ``iris'' dataset about iris flowers.
data(iris)
dim(iris)
head(iris)

#Let's extract setosa subset to work with.
setosa<-iris[1:50,1:4]
head(setosa)



#mvn() function gives many statistical tests for multivariate normality


## Simple coding with default arguments
mvn(setosa)
?mvn

# With multiple arguments
mvn(setosa,subset=NULL,mvnTest= "hz",
    covariance=TRUE,tol=1e-25,alpha=0.5,scale=FALSE,desc=TRUE,
    transform="none", R=1000,
    univariateTest= "AD",
    univariatePlot="none", multivariatePlot="none",
    multivariateOutlierMethod="none",bc=FALSE,
    bcType="rounded", showOutliers=FALSE,showNewData=FALSE)


#Let's look at ``energy'' test for multivariate normality
result<-mvn(data= setosa, mvnTest="energy")
result

# create univariate Q-Q plots
result <- mvn(data = setosa, univariatePlot = "qqplot")

# create univariate histograms
result <- mvn(data = setosa, univariatePlot = "histogram")
####################################################

#install the package ``moments''
#load the ``moments'' package
require(moments)
X=c(19.09, 19.55, 17.89, 17.73, 25.15, 27.27, 25.24, 21.05, 21.65,
    20.92,22.61, 15.71, 22.04, 22.60, 24.25)

#plot the density curve to see the shape of the dataset
plot(density(X))

#Now find the skewness and kurtosis to compare with the density plot
skewness(X)
kurtosis(X)-3

#Create a normally distributed dataset with mean 55 and sd 4.5
Y=rnorm(n = 10000, mean = 55, sd = 4.5)

#plot the density curve to see the shape of the dataset
plot(density(Y))

#Now find the skewness and kurtosis to compare with the density plot
skewness(Y)
kurtosis(Y)-3


#install the package ``mvShapiroTest''
#Load the package
require(myShapiroTest)
# Generating a 5-dimensional random sample of size 20
X=matrix(rnorm(50*5),ncol=5)

#Check the multivariate normality using Shapiro-Willks test
mvShapiro.Test(X)

####### Let's look at multivariate tests on ``MVN'' package ########
#install ``MVN'' package
#load ``MVN'' package
#Note that MVN package comes with the iris dataset

result = mvn(data = iris[-4], subset = "Species", mvnTest = "hz",
             univariateTest = "AD", univariatePlot = "histogram",
             multivariatePlot = "qq", multivariateOutlierMethod = "quan",
             showOutliers = TRUE, showNewData = TRUE)
result
dim(result$newData$setosa)

## Read data from Table 4.3
#set the working directory
setwd("~/Documents/MA369/Datasets")
dir()
T43=read.table("T4-3.DAT",header=F)
dim(T43)
str(T43)
head(T43)

result = mvn(data = T43[-5], 
             multivariatePlot = "qq",
             multivariateOutlierMethod = "quan",
             showOutliers = TRUE)
result
