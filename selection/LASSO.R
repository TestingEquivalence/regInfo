library(glmnet)

LASSO.calibrate<-function(df, nfolds){
  x=df
  x$y=NULL
  x=as.matrix(x)
  y=df$y
  
  # perform cross-validation
  cvfit = cv.glmnet(x, y, nfolds=nfolds,alpha = 1, type.measure = "mse") 
  # alpha=1 is the lasso penalty 
  # CV uses loss measure "mse
  
  fit=cvfit$glmnet.fit
  fit$lambda.min=cvfit$lambda.min
  return(fit)
}

LASSO.getCoef<-function(m){
  
  # Get coefficients corresponding to the best lambda
  finalCoef = predict.glmnet(m, s = m$lambda.min, type = "coefficients")
  vFinalCoef= as.matrix(finalCoef)
  vFinalCoef=as.vector(vFinalCoef)
  names(vFinalCoef)=finalCoef@Dimnames[[1]]
  c=vFinalCoef[1]
  vFinalCoef=vFinalCoef[-1]
  
  return(vFinalCoef)
}


LASSO.predict<-function(m, outOfSample){
  res =predict.glmnet(m, s = m$lambda.min, newx = as.matrix(outOfSample))
  res=as.vector(res)
  return(res)
}

LASSO.relaxed.calibrate<-function(df, nfolds){
  x=df
  x$y=NULL
  x=as.matrix(x)
  y=df$y
  
  # fit LASSO model
  # alpha=1 is the lasso penalty
  fit = glmnet(x, y, alpha = 1, relax=TRUE)
  m=fit$relaxed
  
  # perform cross-validation
  cvfit = cv.glmnet(x, y, nfolds=nfolds,alpha = 1, type.measure = "mse") # alpha=1 is the lasso penalty 
  
  # get the cv lambda value
  m$lambda.min=cvfit$lambda.min
  return(m)
}

LASSO.adaptive.calibrate<-function(df, nfolds){
  # Perform ridge regression using  CV
  x=df
  x$y=NULL
  x=as.matrix(x)
  y=df$y
  rcv = cv.glmnet(x, y, type.measure = "mse", nfold = nfolds,
                  #  ‘alpha = 0’ the ridge penalty.
                  alpha = 0)
  # cv optimal ridge coefficients, without intercept
  cvRidgeCoef = as.numeric(coef(rcv, s = rcv$lambda.min))[-1]
  
  # Perform adaptive LASSO using cv and ridge coefficients
  fit = cv.glmnet(x, y, type.measure = "mse",
                  nfold = nfolds,
                  # ‘alpha = 1’ is the lasso penalty
                  alpha = 1,
                  penalty.factor = 1 / abs(cvRidgeCoef))
  m=fit$glmnet.fit
  m$lambda.min=fit$lambda.min
  return(m)
}
