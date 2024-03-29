library(leaps)

# direction can be "both", "backward", "forward"
step.calibrate<-function(df, direction){
  m=lm("y~.",df)
  finalm = step(m, direction = direction, k = 2, trace=0)
  finalm$df=df
  return(finalm)
}

step.getCoef<-function(m){
  df=m$df
  df$y=NULL
  allNames=names(df)
  coefSelected=coef(m)
  coefSelected=coefSelected[-1]
  nonSelected =setdiff(allNames, names(coefSelected))
  
  allCoef = rep(0, length(allNames))
  names(allCoef)=allNames
  allCoef[names(coefSelected)] = coefSelected
  return(allCoef)
}

# different methods of search are available: 
#method=c("exhaustive","backward", "forward", "seqrep")

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
