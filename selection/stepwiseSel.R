library(olsrr)

calibrate_ols_step_backward<-function(df,p_val){
  m=lm("y~.",df)
  sm=ols_step_backward_p(m, p_val)
  m=sm$model
  
  # selected coef
  df$y=NULL
  allCoef=colnames(df)
  selectedPredictors=names(m$coefficients)
  allCoef01= ifelse(allCoef %in% selectedPredictors, 1, 0)
  m$allCoef=allCoef01
  
  return(m)
}

calibrate_ols_step_backward<-function(df,p_val){
  m=lm("y~.",df)
  sm=ols_step_forward_p(m, p_val)
  m=sm$model
  
  # selected coef
  df$y=NULL
  allCoef=colnames(df)
  selectedPredictors=names(m$coefficients)
  allCoef01= ifelse(allCoef %in% selectedPredictors, 1, 0)
  m$allCoef=allCoef01
  
  return(m)
}
