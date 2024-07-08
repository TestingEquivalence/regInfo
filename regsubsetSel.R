# different methods of search are available: 
# try all three variants "exhaustive","backward", "forward", "seqrep"

regsubset.calibrate<-function(df, method){
  nvmax=ncol(df)-1
  regFit=regsubsets(y ~ ., data = df, nvmax=nvmax, method = method)
  regFitSum=summary(regFit)
  minCpIndex=which.min(regFitSum$cp)
  selectedCoefs = regFitSum$which[minCpIndex,]
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
