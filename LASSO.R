library(glmnet)

LASSO.calibrate<-function(df, nfolds){
  x=df
  x$y=NULL
  x=as.matrix(x)
  y=df$y
  
  # fit LASSO model
  # alpha=1 is the lasso penalty
  fit = glmnet(x, y, alpha = 1)
  
  # perform cross-validation
  cvfit = cv.glmnet(x, y, nfolds=nfolds,alpha = 1) # alpha=1 is the lasso penalty 

  # get the best lambda value
  bestLambda = cvfit$lambda.min
  
  fit$bestLambda=bestLambda
  return(fit)
}

LASSO.getCoef<-function(m){
  
  # Get coefficients corresponding to the best lambda
  finalCoef = predict.glmnet(m, s = m$bestLambda, type = "coefficients")
  vFinalCoef= as.matrix(finalCoef)
  vFinalCoef=as.vector(vFinalCoef)
  names(vFinalCoef)=finalCoef@Dimnames[[1]]
  c=vFinalCoef[1]
  vFinalCoef=vFinalCoef[-1]
  
  return(vFinalCoef)
}


LASSO.predict<-function(m, outOfSample){
  res =predict.glmnet(m, s = m$bestLambda, newx = as.matrix(outOfSample))
  res=as.vector(res)
  return(res)
}

