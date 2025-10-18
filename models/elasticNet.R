library(glmnet)

# relaxed elastic net path for fixed alpha and gamma
# gamma=1 yields lasso also not relaxed; gamma=0 is completely relaxed, also SSE refit
# alpha=1 yields lasso, alpha =0 yields ridge regression
elastic.net.fixed.calibrate =function(data, alpha, gamma) {
  nlambda = 1000
  x=data
  x$y=NULL
  x=as.matrix(x)
  y=data$y
  max_size=ncol(x)
  
  cvfit=cv.glmnet(x, y,
            alpha = alpha,
            relax = TRUE,
            gamma = gamma,
            nlambda = nlambda)
  
  fit = cvfit$glmnet.fit
  
  # number of active variables at each lambda
  coef_mat <- as.matrix(coef(fit)[-1, , drop = FALSE])  # drop intercept
  n_active <- apply(coef_mat != 0, 2, sum)
  
  
  chosen = vector("list", max_size)
  
  for (k in 1:max_size) {
    idxs = which(n_active == k)
    if (length(idxs) > 0) {
      i=idxs[1]
      chosen[[k]] = list(
        size   = k,
        lambda = fit$lambda[i]
      )
    }
    else{
      chosen[[k]]=NULL
    }
    
  }
  
  return(list(fit = fit,
       chosen = chosen,
       alpha = alpha,
       gamma = gamma))
}

# Prediction helper
elastic.net.fixed.predict = function(mods,data) {
  
  x=data
  x$y=NULL
  x=as.matrix(x)
  
  preds = lapply(seq_along(mods$chosen), function(k) {
    m=mods$chosen[[k]]
    if (is.null(m)){
      l=nrow(x)
      return(rep(NA,l))
    }
    return(predict(mods$fit, newx = x, s = m$lambda, gamma=mods$gamma))
  })
  
  return(preds)
}


elasticNet.CV.calibrate = function(data){
  
  nlambda = 1000
  x=data
  x$y=NULL
  x=as.matrix(x)
  y=data$y
  max_size=ncol(x)
  folds = 10
  alphas = seq(0, 1, by = 0.1)
  gammas = seq(0, 1, by = 0.1)
  
  
  results =list()
  n=1  
    for (a in alphas) {
      for (g in gammas) {
        
        cvfit <- cv.glmnet(
          x, y,
          alpha = a,
          relax = TRUE,
          gamma = g,
          nlambda = nlambda,
          nfolds = folds)
        
        coef_mat= as.matrix(coef(cvfit$glmnet.fit)[-1, , drop = FALSE])
        n_active=apply(coef_mat != 0, 2, sum)
        
        for (k in 1:max_size) {
          idxs=which(n_active == k)
          if (length(idxs) > 0) {
            i=idxs[1]
            results[[n]]=list(
            size = k,
            alpha = a,
            gamma = g,
            lambda = cvfit$lambda[i],
            mse = cvfit$cvm[i],
            cvfit = cvfit)
            n=n+1
            }
        }
      }
    }
        
    
    # pick best α, γ, λ for each size k
    sizes= c(1:max_size)
    chosen=lapply(sizes, function(k) {
      subset=Filter(function(r) r$size == k, results)
      
      if (length(subset) == 0) {
        return(NULL)
      }
      
      best = subset[[which.min(sapply(subset, `[[`, "mse"))]]
      return(best)
    })
    

    return(chosen)
  }

  
  # --- Prediction helper ---
elasticNet.CV.predict<-function(mods,data) {
  
  x=data
  x$y=NULL
  x=as.matrix(x)
  
  preds = lapply(mods, function(m) {
    if (is.null(m)){
      l=nrow(x)
      return(rep(NA,l))
    }
    return(predict(m$cvfit, newx =x, s = m$lambda, gamma=m$gamma))
                   
  })
}

  
  

