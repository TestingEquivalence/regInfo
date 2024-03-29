source("regressionModel1.R")
source("simulation.R")
source("stepwiseSelection.R")

scenarioNr=1
regMod1=getLinearModel1()
nSample=10
sizeOOS=10000

set.seed(10071977)
inSampleSet=regMod1$getSampleScenario(regMod1,scenarioNr,nSample)
set.seed(18032024)
outOfSample=regMod1$getSample(regMod1,sizeOOS,regMod1$scenario[[scenarioNr]]$errVariance)
outOfSampleResponce=outOfSample$y
outOfSample$y=NULL


# error only, coefficients are known, also full oracle
calibrate<-function(df){
  return(regMod1$beta)
}

getCoef<-function(m){
  return(m)
}

predict<-function(m, outOfSample){
  y=as.matrix(outOfSample) %*% m
  y=y[,1]
  return(y)
}

res=simulate(calibrate,predict,getCoef,inSampleSet,outOfSample, outOfSampleResponce)
cres=evaluateCoef(res$coef,regMod1$beta)

# full model

calibrate<-function(df){
  m=lm("y~.",df)
  return(m)
}
getCoef<-function(m){
  v=coef(m)
  v=v[-1]
  return(v)
}
predict<-function(m, outOfSample){
  y=predict.lm(m,outOfSample)
  return(y)
}

res=simulate(calibrate,predict,getCoef,inSampleSet,outOfSample, outOfSampleResponce)
cres=evaluateCoef(res$coef,regMod1$beta)

# oracle model where all non zero variables are selected

calibrate<-function(df){
  m=lm(regMod1$oracle,df)
  return(m)
}
getCoef<-function(m){
  return(regMod1$beta)
}
predict<-function(m, outOfSample){
  y=predict.lm(m,outOfSample)
  return(y)
}

res=simulate(calibrate,predict,getCoef,inSampleSet,outOfSample, outOfSampleResponce)
cres=evaluateCoef(res$coef,regMod1$beta)


# stepwise selection with AIC
# try all three variants "both", "backward", "forward"

calibrate<-function(df){
  m=step.calibrate(df,"backward")
  return(m)
}

getCoef<-function(m){
  v=step.getCoef(m)
}

predict<-function(m, outOfSample){
  y=predict.lm(m,outOfSample)
  return(y)
}

res=simulate(calibrate,predict,getCoef,inSampleSet,outOfSample, outOfSampleResponce)
cres=evaluateCoef(res$coef,regMod1$beta)

# stepwise selection with Mallows Cp
# Search methods available are: "exhaustive","backward", "forward", "seqrep"

calibrate<-function(df){
  method="forward"
  m=regsubset.calibrate(df,method)
  return(m)
}

getCoef<-function(m){
  v=regsubset.getCoef(m)
}

predict<-function(m, outOfSample){
  y=predict.lm(m,outOfSample)
  return(y)
}

res=simulate(calibrate,predict,getCoef,inSampleSet,outOfSample, outOfSampleResponce)
cres=evaluateCoef(res$coef,regMod1$beta)
