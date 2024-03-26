library(leaps)

# direction can be "both", "backward", "forward"
step.calibrate<-function(df, direction){
  m=lm("y~.",df)
  finalm = step(m, direction = direction, k = 2, trace=0)
  return(finalm)
}

regsubset.calibrate<-function(df){
  nvmax=ncol(df)-1
  regFit=regsubsets(y ~ ., data = df, nvmax=nvmax)
  regFitSum=summary(regFit)
  minCpIndex=which.min(regFitSum$cp)
  selectedCoefs = regFitSum$which[minCpIndex,]
  selectedCoefs=selectedCoefs[-1]
  selectedPredictors=names(selectedCoefs[selectedCoefs==TRUE])
  frm= paste0("y~",paste(selectedPredictors, collapse = " + "))
  m=lm(frm,df)
  return(m)
}

