library(glmnet)

# Fit relaxed lasso and select lambda for each model size
# gamma=1 yields lasso, gamma=0 is completely relaxed, also SSE refit
RelaxedLasso.calibrate<-function(data, gamma){
  x=data
  x$y=NULL
  x=as.matrix(x)
  y=data$y
  max_size=ncol(x)
  
  fit=glmnet(x, y, nlambda = 1000, relax = TRUE, gamma=gamma)
  
  coef_mat = coef(fit$glmnet.fit)[-1, ]
  n_active = apply(coef(fit)[-1, ] != 0, 2, sum)  # exclude intercept
  
  lambda2size=list()
  
  max_size=ncol(x)
  for (k in 1:max_size) {
    idx = which(n_active == k)
    if (length(idx) > 0) {
      lambda2size[[k]]=fit$lambda[idx[1]]
    } else {
      lambda2size[[k]]=NA
    }
  }
  
  res=list()
  res$fit=fit
  res$lambda2size=unlist(lambda2size)
  res$gamma=gamma
  return(res)
}

# prediction helper
RelaxedLasso.predict<-function(mods,data){
  lambda2size=mods$lambda2size
  fit=mods$fit
  
  x=data
  x$y=NULL
  x=as.matrix(x)
  
  preds = lapply(seq_along(lambda2size), function(k) {
    s=lambda2size[k]
    if (is.na(s)){
      l=nrow(x)
      return(rep(NA,l))
    }
    return(predict(fit, newx = x, s = s, gamma=mods$gamma))
  })
  
  return(preds)
}


