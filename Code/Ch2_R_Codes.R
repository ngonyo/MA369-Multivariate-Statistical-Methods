### Chapter 2: Matrix Algebra Review R Codes ###
##############################################
## Dealing with vectors
x <- c(1, 3, 2)
y <- c(-2, 1, -1)

3*x # 3 times x

x + y # addition of x and y

(Lx <- sqrt(sum(x^2))) # Length of x
(Lx <- sqrt(t(x)%*%x)) # Length of x

(Ly <- sqrt(t(y)%*%y)) # Length of y

all.equal(5*(x + y), 5*x + 5*y)

(L3x <- sqrt(t(3*x)%*%(3*x))) # Length of 3x
all.equal(L3x, 3*Lx)

(theta <- acos((t(x)%*%y)/(Lx * Ly))) #angle between x and y

# Dealing with matrices
A <- matrix(c(0, 3, 1,
              1, -1, 1),
            nrow = 2, byrow = T)
A

B <- matrix(c(1, -2, -3,
              2, 5, 1),
            nrow = 2, byrow = T)
B

C <- matrix(c(2, 1, 0, -1), nrow = 2, byrow = F)
C

x <- c(7, -3, 6)

y <- c(5, 8, -4)

z <- c(2, 9)

t(A) # Transpose of A

4 * A # 4 times of A

A + B # Matrix Addition

B + A

A * B  # Element-wise multiplication

A %*% C # Matrix multiplication
# Error in A %*% C : non-conformable arguments

C %*% A  # Matrix multiplication

A %*% x # Marix-vector multiplication

t(x) %*% y # Inner-product of two vectors

x %*% t(y) # Product of two vectors would produce a matrix

t(z) %*% C %*% z # Caclulating a quadratic form
######################################

A <- matrix(c(1, 2, 3,
              0, 1, 4,
              5, 6, 0), nrow = 3, byrow = T)
A

(A.det <- det(A)) # Determinant of a matrix

(A.inv <- solve(A)) # Inverse of a matrix

A %*% A.inv # Give an identity matrix
A.inv %*% A # Give an identity matrix

M <- diag(c(2, 5, 10)) # Diagonal matrix

det(M)
2*5*10

solve(M)
diag(1/c(2, 5, 10))

######################

## Eigenvalue decomposition
A <- matrix(c(5, -5, -5, 10), nrow = 2, byrow = T)
A # Symmetric Matrix

Eig.A <- eigen(A) 

Eigval.A <- Eig.A$values # Eigenvalues

P <- Eig.A$vectors  #Eigenvectors

P %*% t(P) # Identity matrix. P is row-orthogonal
t(P) %*% P # Identity matrix. P is column-orthogonal

(Det.A <- prod(Eigval.A))
det(A)

Lam <- diag(Eigval.A) # Lam matrix

Lam.inv <- diag(1/Eigval.A) # Lam^-1 matrix

Lam.sqrt <- diag(sqrt(Eigval.A)) # Lam^1/2 matrix

Lam.inv.sqrt <- diag(1/sqrt(Eigval.A)) # Lam^-1/2 matrix

A.inv <- P %*% Lam.inv %*% t(P) # Inverse of A
A.inv
solve(A)  # Inverse of A

A.sqrt <- P %*% Lam.sqrt %*% t(P) # Square-root of A
A.sqrt

A.inv.sqrt <- P %*% Lam.inv.sqrt %*% t(P) # Inverse Square-root of A
A.inv.sqrt

A.spectral <- P %*% Lam %*% t(P) 
A.spectral
A
###############################

## Singualr value decomposition
set.seed(123) #set seed to ensure all classmates will get same random values
X1 <- rnorm(10, mean = 1, sd = 2) #randomly generate 10 values from normal dist, with set means and sd
X2 <- rnorm(10, mean = 0, sd = 1)
X3 <- rnorm(10, mean = 1.5, sd = 3)
X4 <- 3 * X1

X <- data.frame(X1, X2, X3, X4)

SVD.X <- svd(X)

U <- SVD.X$u
dim(U)

SVD.X$d # Notice that, last value is practically 0. 
# Be prepare to loose rank of the matrix

LAM <- diag(SVD.X$d)
dim(LAM)

V <- SVD.X$v
dim(V)

X.svd <- U %*% LAM %*% t(V)
max(abs(X - X.svd)) # Maximum absolute Difference is practically 0.

solve(t(X) %*% as.matrix(X)) # As expected, it will not be convertible. 


r <- sum(SVD.X$d > 1e-10) # Getting number of non-zero d values

LAM.r <- LAM[1:r, 1:r]

U.r <- U[, 1:r]

V.r <- V[, 1:r]

X.reconstruct <- U.r %*% LAM.r %*% t(V.r)
max(abs(X - X.reconstruct)) # # Maximum absolute Difference is practically 0.

############################
## Computing correlation matrix from covariance matrix
SIG <- matrix(c(4, 1, 2, 
                1, 9, -3, 
                2, -3, 25), nrow = 3, byrow = T)
SIG

V <- diag(diag(SIG)) # Variance matrix
V

V.sqrt <- diag(sqrt(diag(SIG)) ) # Standard deviation matrix
V.sqrt

V.inv.sqrt <- diag(1/sqrt(diag(SIG)) ) # Inverse of Standard deviation matrix
V.inv.sqrt

RHO <- V.inv.sqrt %*% SIG %*% V.inv.sqrt
RHO # Clearly a correlation matrix 

SIG.back <- V.sqrt %*% RHO %*% V.sqrt
SIG.back

all.equal(SIG, SIG.back) # Both the matrices are identical.
