calibrate_stepAIC<-function(df, direction, steps){
  m=lm("y~.",df)
  sm=stepAIC(m,direction=direction, steps=steps, trace=0)
  
  
  # selected coef
  df$y=NULL
  allCoef=colnames(df)
  selectedPredictors=names(sm$coefficients)
  allCoef01= ifelse(allCoef %in% selectedPredictors, 1, 0)
  sm$allCoef=allCoef01
  
  return(sm)
}
