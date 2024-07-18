# different methods of search are available: 
# try all three variants "exhaustive","backward", "forward", "seqrep"
library(leaps)

regsubset.calibrate<-function(df, method, selector){
  nvmax=ncol(df)-1
  regFit=regsubsets(y ~ ., data = df, nvmax=nvmax, method = method)
  regFitSum=summary(regFit)
  
  minCpIndex=which.min(regFitSum$cp)
  minBICIndex=which.min(regFitSum$bic)
  
  if (selector=="cp"){
    ind=minCpIndex
  } else if (selector=="bic") {
    ind=minBICIndex
  }
  
  selectedCoefs = regFitSum$which[ind,]
  selectedCoefs=selectedCoefs[-1]
  allCoef=as.numeric(selectedCoefs)
  selectedPredictors=names(selectedCoefs[selectedCoefs==TRUE])
  frm= paste0("y~",paste(selectedPredictors, collapse = " + "))
  m=lm(frm,df)
  m$allCoef=allCoef
  return(m)
}

regsubset.getCoef<-function(m){
  return(m$allCoef)
}
