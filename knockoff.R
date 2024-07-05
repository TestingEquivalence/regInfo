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
  
  repeat{
    result = knockoff.filter(X, y, fdr = fdr, statistic = statistic)
    if (length(result$selected)>0) {
      break
    }
  }
  
  selectedPredictors=names(result$selected)
  frm= paste0("y~",paste(selectedPredictors, collapse = " + "))
  m=lm(frm,df)
  
  return(m)
}




