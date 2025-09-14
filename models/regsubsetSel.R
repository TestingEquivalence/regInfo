# different methods of search are available: 
# try the variants "backward", "forward",  “seqrep”

library(leaps)

regsubset.calibrate<-function(data, method){
  fit = regsubsets(y ~ ., data=data, nbest=1, nvmax = ncol(data)-1, method = method)
  
  summary_fit <- summary(fit)
  
  models = lapply(1:(ncol(data)-1), function(k) {
    # Get logical vector of which vars are selected for size k
    vars <- names(which(summary_fit$which[k, -1]))  # drop intercept
    
    # Build formula with y ~ selected vars
    form <- as.formula(paste("y ~", paste(vars, collapse = " + ")))
    
    # Fit lm
    lm(form, data = data)
  })
  return(models)
}

forward.calibrate<-function(data){
  return(regsubset.calibrate(data,method = "forward"))
}

backward.calibrate<-function(data){
  return(regsubset.calibrate(data,method = "backward"))
}

seqrep.calibrate<-function(data){
  return(regsubset.calibrate(data,method = "seqrep"))
}