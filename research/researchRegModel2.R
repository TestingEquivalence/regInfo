source("models/regressionModel2.R")
source("simulation.R")

nSample=10000
sizeOOS=100000

# generate models randomly
models=list()
set.seed(10071977)
for(i in 1:nSample){
  models[[i]]=linMod2$getCoeff(linMod2$p,linMod2$k,linMod2$A)
}

# modelling functions
getInSample<-function(beta){
  X=linMod2$getIndependentX(linMod2$n,linMod2$p)
  sample=linMod2$getSample(linMod2$n,linMod2$p,beta,X)
  sample$beta=beta
  return(sample)
}

getOutOfSample<-function(beta){
  X=linMod2$getIndependentX(sizeOOS,linMod2$p)
  sample=linMod2$getSample(sizeOOS,linMod2$p,beta,X)
  return(sample)
}

# error only, coefficients are known, also full oracle
calibrate<-function(inSample){
  return(inSample$beta)
}

getCoef<-function(m){
  return(m)
}

predict<-function(m, outOfSample){
  y=as.matrix(outOfSample) %*% m
  y=y[,1]
  return(y)
}

res=simulate2(calibrate = calibrate, predict = predict,
              getCoef = getCoef,models = models,
              getInSample = getInSample, getOutOfSample =getOutOfSample)
res$mse
cres=evaluateCoef(res$coef,regMod1$beta)
write.csv(res$mse,file="mse.csv")
write.csv(cres, file="coef.csv")

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
predict=lm.predict

res=simulate(calibrate,predict,getCoef,inSampleSet,outOfSample, outOfSampleResponce)
res$mse
cres=evaluateCoef(res$coef,regMod1$beta)

write.csv(res$mse,file="mse.csv")
write.csv(cres, file="coef.csv")

# oracle model where all non zero variables are selected

calibrate<-function(df){
  m=lm(regMod1$oracle,df)
  return(m)
}
getCoef<-function(m){
  return(regMod1$beta)
}
predict=lm.predict

res=simulate(calibrate,predict,getCoef,inSampleSet,outOfSample, outOfSampleResponce)
cres=evaluateCoef(res$coef,regMod1$beta)
res$mse

write.csv(res$mse,file="mse.csv")
write.csv(cres, file="coef.csv")

# stepwise selection with Mallows Cp and Bic
# Search methods available are: "backward", "forward", "exhaustive", "seqrep"
# Selector can be "cp", "bic" or "adjR2"
source("regsubsetSel.R")

calibrate<-function(df){
  method="seqrep"
  selector="adjR2"
  m=regsubset.calibrate(df,method, selector)
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


write.csv(res$mse,file="mse.csv")
write.csv(cres, file="coef.csv")


# LASSO using cross validation to find optimal parameter lambda 
source("LASSO.R")

# try different values of cross validation: 5, 10 and 20
calibrate<-function(df){
  m=LASSO.calibrate(df,20)
  return(m)
}

getCoef<-function(m){
  v=LASSO.getCoef(m)
}

predict<-function(m, outOfSample){
  y=LASSO.predict(m,outOfSample)
  return(y)
}

res=simulate(calibrate,predict,getCoef,inSampleSet,outOfSample, outOfSampleResponce)
cres=evaluateCoef(res$coef,regMod1$beta)

write.csv(res$mse,file="mse.csv")
write.csv(cres, file="coef.csv")

# use knockoff to select relevant covariates
source("knockoff.R")

# try different values of fdr 1%, 5%, 10%, 20%, 50%
# try different statistic: originalKnockoffStat oder Default statistic
calibrate<-function(df){
  m=knockoff.calibrate(df,fdr=0.50)
  return(m)
}

getCoef<-function(m){
 return(m$allCoef)
}

predict=predict.lm

res=simulate(calibrate,predict,getCoef,inSampleSet,outOfSample, outOfSampleResponce)
cres=evaluateCoef(res$coef,regMod1$beta)

write.csv(res$mse,file="mse.csv")
write.csv(cres, file="coef.csv")
 