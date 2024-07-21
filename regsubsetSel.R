# different methods of search are available: 
# try all three variants "exhaustive","backward", "forward", "seqrep"
library(leaps)

regsubset.calibrate<-function(df, method, selector){
  nvmax=ncol(df)-1
  regFit=regsubsets(y ~ ., data = df, nvmax=nvmax, method = method)
  regFitSum=summary(regFit)
  
  minCpIndex=which.min(regFitSum$cp)
  minBICIndex=which.min(regFitSum$bic) 
  minAdjR2Index=which.max(regFitSum$adjr2)
  
  if (selector=="cp"){
    ind=minCpIndex
  } else if (selector=="bic") {
    ind=minBICIndex
  } else if (selector=="adjR2"){
    ind=minAdjR2Index
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
