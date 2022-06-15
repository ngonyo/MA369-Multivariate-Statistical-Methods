### Chapter 7 Multivariate Linear Regression Models

## Simple Linear Regression (SLR) Models
y <- c(2158.70,1678.15, 2316, 2061.3, 2207.5, 1708.3, 1784.7,
       2575, 2357.9, 2256.7, 2165.2,2399.55,1779.8, 2336.75,
       1765.3, 2053.5, 2414.4, 2200.5, 2654.2, 1753.7)
x <- c(15.5,23.7, 8,17, 5.5, 19, 24, 2.5, 7.5, 11, 13, 3.75, 25, 9.75,
       22, 18, 6, 12.5, 2, 21.5)

fit <- lm(y ~ x) # lm is built in command for linear model.

?lm # for help

fit

summary(fit)

plot(y ~ x, main = "Scatter plot and regression line", 
     xlab = "Age", ylab = "Strength")

abline(fit, col = "red", lwd = 2)
##########################################

### Multiple Linear Regression (MLR) Models

DelTime <-c(16.68, 11.50, 12.03, 14.88, 13.75, 18.11, 8, 17.83,
            79.24, 21.5, 40.33, 21.0, 13.5, 19.75, 24.0, 29, 15.35, 19.0, 9.5,
            35.1, 17.9, 52.32, 18.75,19.83, 10.75)

NumOfCases <- c(7, 3, 3, 4, 6, 7, 2, 7, 30, 5, 16, 10, 4, 6, 9, 10, 6,
                7, 3, 17, 10, 26, 9, 8, 4)

Distance <- c(560, 220, 340, 80, 150, 330, 110, 210, 1460, 605,
              688, 215, 255, 462, 448, 776, 200, 132, 36, 770, 140, 810, 450,
              635, 150 )

dat <- data.frame (cbind(DelTime,NumOfCases,Distance))

pairs(dat)

mymodel <- lm(DelTime ~ NumOfCases + Distance)

summary(mymodel)

anova(mymodel)

fitted(mymodel)

residuals(mymodel)

### Now double check all the above calculations using expressions manually
X <- cbind(1, NumOfCases, Distance)

Y <- DelTime

beta.hat <- solve(t(X) %*% X) %*% t(X) %*% Y

beta.hat

# Do the rest calculations using Expression from slides