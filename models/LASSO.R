library(glmnet)

LASSO.calibrate<-function(data){
  x=data
  x$y=NULL
  x=as.matrix(x)
  y=data$y
  
  fit <- glmnet(x, y)
  n_active <- apply(coef(fit)[-1, ] != 0, 2, sum)  # exclude intercept
  
  models <- list()
  
  lambda2size <- sapply(1:max(n_active), function(k) {
    idx <- which(n_active == k)[1]   # first lambda with k active vars
    fit$lambda[idx]
  })
  
 res=list()
 res$fit=fit
 res$lambda2size=lambda2size
 return(res)
}

LASSO.predict<-function(mods,data){
  lambda2size=mods$lambda2size
  fit=mods$fit
  
  x=data
  x$y=NULL
  x=as.matrix(x)

  
  preds = lapply(seq_along(lambda2size), function(k) {
    predict(fit, newx = x, s = lambda_for_size[k])
  })
}
