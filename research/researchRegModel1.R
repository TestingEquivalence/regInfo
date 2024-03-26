source("regressionModel1.R")
source("simulation.R")

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
  return(c(0,m))
}

predict<-function(m, outOfSample){
  y=as.matrix(outOfSample) %*% m
  y=y[,1]
  return(y)
}

res=simulate(calibrate,predict,getCoef,inSampleSet,outOfSample, outOfSampleResponce)
cres=evaluateCoef(res$coef,c(0, regMod1$beta))

# full model

calibrate<-function(df){
  m=lm("y~.",df)
  return(m)
}
getCoef<-function(m){
  return(coef(m))
}
predict<-function(m, outOfSample){
  y=predict.lm(m,outOfSample)
  return(y)
}

res=simulate(calibrate,predict,getCoef,inSampleSet,outOfSample, outOfSampleResponce)

# oracle model where all non zero variables are selected

calibrate<-function(df){
  m=lm(regMod1$oracle,df)
  return(m)
}
predict<-function(m, outOfSample){
  y=predict.lm(m,outOfSample)
  return(y)
}

resultMSE$oracleSelected=simulate(calibrate,predict,inSampleSet,outOfSample, outOfSampleResponce)



# stepwise selection with AIC
# try all three variants "both", "backward", "forward"



predict<-function(m, outOfSample){
  y=predict.lm(m,outOfSample)
  return(y)
}

resultMSE$beAIC=simulate(calibrate,predict,inSampleSet,outOfSample, outOfSampleResponce)
resultMSE=as.data.frame(resultMSE)
