# This regression model is originally from the paper:
# Rina Foygel Barber and Emmanuel J. Candes, “CONTROLLING THE FALSE DISCOVERY RATE VIA KNOCKOFFS” 
# The Annals of Statistics 2015, Vol. 43, No. 5, 2055–2085
# DOI: 10.1214/15-AOS1337

library(MASS)

linMod2=list()
linMod2$n=3000  # Sample size
linMod2$p=1000  # Number of features
linMod2$k=30    # Number of non-zero coefficients
linMod2$A= 3.5   # Signal amplitude

linMod2$getCoeff<-function(p,k,A){
  beta <- rep(0, p)  # Initialize β as a vector of zeros
  non_zero_indices <- sample(1:p, k)  # Randomly select k indices
  beta[non_zero_indices] <- sample(c(-A, A), k, replace = TRUE)
  return(beta)
}

linMod2$getSample<-function(n,p,beta, X){
  # Generate y from N(Xβ, I)
  y <- X %*% beta + rnorm(n)  
  res=list()
  res$x=X
  res$y=y
  return(res)
}

linMod2$getIndependentX<-function(n,p){
  # Generate X ∈ Rn×p with i.i.d. N(0, 1) entries
  X = matrix(rnorm(n * p), nrow = n, ncol = p)
  # Normalize the columns of X to have mean 0 and standard deviation 1
  X = scale(X, center = TRUE, scale = TRUE)
  return(X)
}

linMod2$getDependentX<-function(n, p, r){
  
  # construct the covariance matrix
  cov_matrix=matrix(0, nrow = p, ncol = p)
  for (i in 1:p) {
    for (j in 1:p) {
      cov_matrix[i, j]= r^(abs(i - j))
    }
  }
  
  # Generate samples
  mu = rep(0, p)  # Mean vector is zero
  X = mvrnorm(n = n, mu = mu, Sigma = cov_matrix)
  return(X)
}