library(olsrr)

source("models/regressionModel1.R")
source("simulation.R")
source("selection/stepwiseSel.R")

scenarioNr=1
regMod1=getLinearModel1()
nSample=1

set.seed(10071977)
inSampleSet=regMod1$getSampleScenario(regMod1,scenarioNr,nSample)
df=inSampleSet[[1]]

set.seed(18032024)
sizeOOS=100000
outOfSample=regMod1$getSample(regMod1,sizeOOS,regMod1$scenario[[scenarioNr]]$errVariance)
outOfSampleResponce=outOfSample$y
outOfSample$y=NULL


# stepwise backward regression
calibrate<-function(df){
  m=calibrate_ols_step_forward(df, 0.10)
}

getCoef<-function(m){
  return(m$allCoef)
}
predict<-function(m,outOfSample){
  return(predict.lm(m,outOfSample))
}

res=simulate(calibrate,predict,getCoef,inSampleSet,outOfSample, outOfSampleResponce)
res
cres=evaluateCoef(res$coef,regMod1$beta)
cres


