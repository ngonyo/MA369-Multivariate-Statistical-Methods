
# Illustration of principal components analysis (PCA) using USArrests data 

# We will use the prcomp function. 
# See the help on the function (uses SVD rather than spectral decomposition) 

?prcomp

# Look at USArrests data (part of base R) 

?USArrests
str(USArrests)

# Extract the names of states 

states <- row.names(USArrests)
states

# Look at mean and sd

apply(USArrests, 2, mean)
apply(USArrests, 2, sd)

# Perform PCA

pca <- prcomp(USArrests, center = TRUE, scale = TRUE) # Standardizing the data
names(pca)


# Check center and scale
pca$center
pca$scale

# Get the loading matrix Phi (order pxp)

(PHI <- pca$rotation)

# Note: we may interpret the first two PCs as follows: 
# PC1: a measure of overall rates of serious crimes 
# PC2: a measure of level of urbanization of state

# Get the score matrix (Z)
dim(pca$x)
Z <- pca$x
head(Z)

# Check the calculation
x.std <- apply(USArrests, 2, function(x){(x-mean(x))/sd(x)})
dim(x.std)
head(x.std, 10)

# apply(x.std, 2, mean)
# apply(x.std, 2, sd)


# Get the score matrix (Z)
score.matrix <- x.std%*%pca$rotation
dim(score.matrix)

max(abs(Z - score.matrix))

# Check the covariance matrix of the scores
round(cov(pca$x), 3)


# Compute the proportion of variance explained (PVE)

(pc.var <- pca$sdev^2)

pve <- pc.var/sum(pc.var)
pve
cumsum(pve)


# Scree plot

plot(pve, xlab = "Principal Component", ylab = "Proportion of Variance Explained", ylim = c(0,1), type = 'b')
#here we would cut it off after two variables as after the third it flattens out (this is discretionary)

# Plot of cumulative PVE

plot(cumsum(pve), xlab = "Principal Component", ylab = "Cumulative Proportion of Variance Explained", ylim = c(0,1), type = 'b')


###################################################################

