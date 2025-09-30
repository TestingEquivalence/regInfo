library(glmnet)

extract_best_gamma_per_lambda <- function(cvfit) {
  
  master_lambda = cvfit$lambda
  statlist = cvfit$relaxed$statlist    # list of cvstats (one per gamma)
  gammas =cvfit$relaxed$gamma        # the gamma values used
  
  # Build lambda x gamma matrix of cvm, aligned to master_lambda
  cvm_mat = sapply(statlist, function(s) {
    vv <- rep(NA_real_, length(master_lambda))
    mm <- match(s$lambda, master_lambda)
    vv[mm] <- s$cvm
    vv
  })
  colnames(cvm_mat) <- paste0("g=", gammas)
  rownames(cvm_mat) <- format(master_lambda, digits = 6)
  
  # best gamma index per lambda (NA if all NA)
  best_idx <- apply(cvm_mat, 1, function(v) {
    if (all(is.na(v))) NA_integer_ else which.min(v)
  })
  
  best_gamma <- rep(NA_real_, length(best_idx))
  best_cvm   <- rep(NA_real_, length(best_idx))
  for (i in seq_along(best_idx)) {
    j <- best_idx[i]
    if (!is.na(j)) {
      best_gamma[i] <- gammas[j]
      best_cvm[i]   <- cvm_mat[i, j]
    }
  }
  
  # number of active variables for lasso path at each lambda
  coef_mat <- as.matrix(coef(cvfit$glmnet.fit)[-1, , drop = FALSE])   # drop intercept
  n_active <- apply(coef_mat != 0, 2, sum)
  
  df <- data.frame(
    lambda = master_lambda,
    n_active = n_active,
    best_gamma = best_gamma,
    best_cvm = best_cvm,
    stringsAsFactors = FALSE
  )
  
  return(list(df = df, cvm_mat = cvm_mat, gammas = gammas, cvfit = cvfit))
}

# Map models by size: choose the first lambda that attains size k (fallback to previous if missing)
choose_by_size <- function(info, maxk) {
  df <- info$df
  chosen <- vector("list", maxk)

  for (k in 1:maxk) {
    idxs = which(df$n_active == k)
    if (length(idxs) > 0) {
    i=idxs[1]  
    chosen[[k]] <- list(
      size = k,
      lambda = df$lambda[i],
      n_active = df$n_active[i],
      gamma = df$best_gamma[i],
      cvm = df$best_cvm[i]
    )
    } else{
    chosen[[k]]=NULL
  } }
  return(chosen)
}




relaxedLassoCV.calibrate=function(data) {
  x=data
  x$y=NULL
  x=as.matrix(x)
  y=data$y
  
  gammas = seq(0, 1, by = 0.1)
  nfolds = 10
  cvfit = cv.glmnet(x, y, relax = TRUE, gamma = gammas, nfolds = nfolds)

  info=extract_best_gamma_per_lambda(cvfit)  
 
  maxK=ncol(x)
  chosen=choose_by_size(info, maxK)
  
  res=list(
    chosen=chosen,
    cvfit=cvfit,
    info=info
  )
  return(res)
}

# prediction helper
relaxedLassoCV.predict<-function(mods,data){
  
  x=data
  x$y=NULL
  x=as.matrix(x)
  chosen=mods$chosen
  
  preds = lapply(seq_along(chosen), function(k) {
    m=chosen[[k]]
    if (is.null(m)){
      l=nrow(x)
      return(rep(NA,l))
    }
      
    
    return(predict(mods$cvfit, newx = x, s = m$lambda, gamma = m$gamma))
  })
  
  return(preds)
}


