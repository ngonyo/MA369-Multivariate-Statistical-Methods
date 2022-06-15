## Chapter 6: Part 2 - One-Way MANOVA

X <- as.matrix(mtcars[, c("mpg","disp","hp","wt")])
dim(X) # n = 32, p = 4
cylinder <- factor(mtcars$cyl) # g = 3 

require(stats)
fit <- manova(X ~ cylinder)

summary(fit) # Default Statistic is Pillai

summary(fit, test = "Wilks") # ANOVA table of Wilks' lambda

summary(fit, test = "Roy") # ANOVA table of Roy's largest root

summary(fit, test = "Hotelling-Lawley") # ANOVA table of Hotelling-Lawley

## One-Way ANOVA from MANOVA 
summary.aov(fit) # univariate ANOVA tables

## Testing Homogeneity of Covariance Matrices
library(biotools)
boxM(X, cylinder)
