library(knockoff)
library(doParallel)

# statistics to use with knockoff
# stat.glmnet_coefdiff is standard statistic of knockoff filter
# or originalstatistic from the knockoff paper

originalKnockoffStat = function(X, X_k, y) {
  abs(t(X) %*% y) - abs(t(X_k) %*% y)
}


knockoff.calibrate<-function(df, fdr, statistic=stat.glmnet_coefdiff){
  X=df
  X$y=NULL
  y=df$y

  result = knockoff.filter(X, y, fdr = fdr, statistic = statistic)
  
  if (length(result$selected)>0) {
      selectedPredictors=names(result$selected)
      frm= paste0("y~",paste(selectedPredictors, collapse = " + "))
  } else{
     frm="y~1" 
     selectedPredictors=c()
  }
  
  m=lm(frm,df)
  
  # selected coef
  df$y=NULL
  allCoef=colnames(df)
  allCoef01= ifelse(allCoef %in% selectedPredictors, 1, 0)
  # allCoef
  # selectedPredictors
  # allCoef01
  m$allCoef=allCoef01
  
  return(m)
}




