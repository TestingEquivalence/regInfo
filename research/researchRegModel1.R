source("models/regressionModel1.R")
source("simulation.R")

scenarioNr=1
regMod1=getLinearModel1()
nSample=10000
sizeOOS=100000

set.seed(10071977)
inSampleSet=regMod1$getSampleScenario(regMod1,scenarioNr,nSample)
set.seed(18032024)
outOfSample=regMod1$getSample(regMod1,sizeOOS,regMod1$scenario[[scenarioNr]]$errVariance)
outOfSampleResponce=outOfSample$y
outOfSample$y=NULL

ls=startSummary()
nCorrect=7

# error only, coefficients are known, also full oracle
sname="known_model"
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

df=simulate(calibrate,predict,getCoef,inSampleSet,outOfSample, outOfSampleResponce, regMod1$beta)
write.csv(df, paste0(sname,".csv"))
ls=appendSummary(ls,df,sname, nCorrect)
write.csv(as.data.frame(ls), "summary.csv")

# full model
sname="full_model"

calibrate<-function(df){
  m=lm("y~.",df)
  return(m)
}
getCoef<-function(m){
  v=coef(m)
  v=v[-1]
  return(v)
}
predict<-function(m,outOfSample){
  return(predict.lm(m,outOfSample))
}

df=simulate(calibrate,predict,getCoef,inSampleSet,outOfSample, outOfSampleResponce, regMod1$beta)
write.csv(df, paste0(sname,".csv"))
ls=appendSummary(ls,df,sname, nCorrect)
write.csv(as.data.frame(ls), "summary.csv")

# oracle model where all non zero variables are selected
sname="selection_oracle"

calibrate<-function(df){
  m=lm(regMod1$oracle,df)
  return(m)
}
getCoef<-function(m){
  return(regMod1$beta)
}
predict<-function(m,outOfSample){
  return(predict.lm(m,outOfSample))
}

df=simulate(calibrate,predict,getCoef,inSampleSet,outOfSample, outOfSampleResponce, regMod1$beta)
write.csv(df, paste0(sname,".csv"))
ls=appendSummary(ls,df,sname, nCorrect)
write.csv(as.data.frame(ls), "summary.csv")


# stepwise selection using AIC, Mallows Cp, BIC, adjR2
# Search methods available are: 
methods=c("backward", "forward", "exhaustive", "seqrep")
# Selectorrs for penalty terms are:
selectors=c("cp", "bic" , "adjR2")
source("selection/regsubsetSel.R")

getCoef<-function(m){
  v=regsubset.getCoef(m)
}

predict<-function(m, outOfSample){
  y=predict.lm(m,outOfSample)
  return(y)
}

for (selector in selectors){
  for (method in methods){
    calibrate<-function(df){
      m=regsubset.calibrate(df,method, selector)
      return(m)
    }
    
    sname=paste0(selector,"_",method)
    
    df=simulate(calibrate,predict,getCoef,inSampleSet,outOfSample, outOfSampleResponce, regMod1$beta)
    write.csv(df, paste0(sname,".csv"))
    ls=appendSummary(ls,df,sname, nCorrect)
    write.csv(as.data.frame(ls), "summary.csv")
  }
}



# LASSO using cross validation to find optimal parameter lambda 
source("selection/LASSO.R")

# try different values of cross validation: 5, 10 and 20
getCoef<-function(m){
  v=LASSO.getCoef(m)
}

predict<-function(m, outOfSample){
  y=LASSO.predict(m,outOfSample)
  return(y)
}

for (nfolds in c(5,10,20)){
  
  calibrate<-function(df){
    m=LASSO.calibrate(df,nfolds)
    return(m)
  }
  
  sname=paste0("Lasso_",nfolds)
  
  df=simulate(calibrate,predict,getCoef,inSampleSet,outOfSample, outOfSampleResponce, regMod1$beta)
  write.csv(df, paste0(sname,".csv"))
  ls=appendSummary(ls,df,sname, nCorrect)
  write.csv(as.data.frame(ls), "summary.csv")
}

# relaxed LASSO using cross validation to find optimal parameter lambda 
source("selection/LASSO.R")

# try different values of cross validation: 5, 10 and 20

getCoef<-function(m){
  v=LASSO.getCoef(m)
}

predict<-function(m, outOfSample){
  y=LASSO.predict(m,outOfSample)
  return(y)
}

for (nfolds in c(5,10,20)){
  
  calibrate<-function(df){
    m=LASSO.relaxed.calibrate(df,nfolds)
    return(m)
  }
  
  sname=paste0("relax_Lasso_",nfolds)
  
  df=simulate(calibrate,predict,getCoef,inSampleSet,outOfSample, outOfSampleResponce, regMod1$beta)
  write.csv(df, paste0(sname,".csv"))
  ls=appendSummary(ls,df,sname, nCorrect)
  write.csv(as.data.frame(ls), "summary.csv")
}

# adoptive LASSO using cross validation to find optimal parameter lambda 
source("selection/LASSO.R")

# try different values of cross validation: 5, 10 and 20

getCoef<-function(m){
  v=LASSO.getCoef(m)
}

predict<-function(m, outOfSample){
  y=LASSO.predict(m,outOfSample)
  return(y)
}

for (nfolds in c(5,10,20)){
  
  calibrate<-function(df){
    m=LASSO.adaptive.calibrate(df,nfolds)
    return(m)
  }
  
  sname=paste0("adopt_Lasso_",nfolds)
  
  df=simulate(calibrate,predict,getCoef,inSampleSet,outOfSample, outOfSampleResponce, regMod1$beta)
  write.csv(df, paste0(sname,".csv"))
  ls=appendSummary(ls,df,sname, nCorrect)
  write.csv(as.data.frame(ls), "summary.csv")
}


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
 