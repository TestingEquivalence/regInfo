library(knockoff)
library(glmnet)

knockoff.calibrate <- function(data) {
  nlambda = 1000
  x=data
  x$y=NULL
  x=as.matrix(x)
  y=data$y
  p=ncol(x)
  
  # 1. Create knockoff copies
  knock <- create.fixed(X)
  x_k <- knock$Xk
  
  # 2. Fit Lasso on combined [X | X_k]
  fit = glmnet(cbind(x, x_k), y, nlambda = nlambda, standardize = TRUE)
  
  # 3. Compute W-statistics
  W = stat.glmnet_coefdiff(x, x_k, y)
  
  # 4. Rank variables by absolute importance
  ranking = order(abs(W), decreasing = TRUE)
  
  models =lapply(seq_len(p), function(k) {
    vars = colnames(x)[ranking[1:k]]
    df =data.frame(y = y, x[, vars, drop = FALSE])
    m=lm("y ~.", data = df)
    return(m)
  })
}
