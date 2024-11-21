simulate<-function(calibrate, predict, getCoef,inSampleSet,outOfSample, outOfSampleResponce, trueCoef){
  vmse=c()
  vRsquared=c()
  nCorrectNonZero=c()
  nWrongNonZero=c()
  
  for(df in inSampleSet){
    m=calibrate(df)
    
    predicted=predict(m, outOfSample)
    resid=outOfSampleResponce-predicted

    mse=mean(resid^2)
    vmse=c(vmse,mse)

    SS_res=sum(resid^2)
    meanY=mean(outOfSampleResponce)
    cY=outOfSampleResponce-meanY
    SS_tot=sum(cY^2)
    Rsquared=1-SS_res/SS_tot
    vRsquared=c(vRsquared,Rsquared)
    
    vCoef=getCoef(m)
    coefEval=evaluateCoef(vCoef,trueCoef)
    nCorrectNonZero=c(nCorrectNonZero,coefEval$nCorrectNonZero)
    nWrongNonZero= c(nWrongNonZero, coefEval$nWrongNonZero)
  }
  
  res=list()
  res$mse=vmse
  res$rSquared=vRsquared
  res$nCorrectNonZero=nCorrectNonZero
  res$nWrongNonZero=nWrongNonZero
  
  return(res)
}

evaluateCoef<-function(vCoef, trueCoef){
  boolTrueCoef=(trueCoef!=0)
  bv=(vCoef!=0)
  res=list()
  res$nCorrectNonZero=sum(bv & boolTrueCoef)
  res$nWrongNonZero=sum(!boolTrueCoef & bv)
  return(res)
}

simulate2<-function(calibrate, predict, getCoef, models,
                    getInSample,getOutOfSample){
  vmse=c()
  vcoef=list()
  
  i=1
  for(mod in models){
    # Set a unique seed in each iteration
    seed_value <- 10071977 + i * 1e6
    set.seed(seed_value)
    inSample=getInSample(mod)
    outOfSample=getOutOfSample(mod)
    
    m=calibrate(inSample)
    vcoef[[i]]=getCoef(m)
    
    predicted=predict(m, outOfSample)
    responce=outOfSample$y
    mse=mean((responce - predicted)^2)
    vmse=c(vmse,mse)
    i=i+1
  }
  
  res=list()
  res$coef=vcoef
  res$mse=vmse
  
  return(res)
}

evaluateCoef2<-function(vCoef, vTrueCoef){
  nCorrectNonZero=c()
  nWrongNonZero=c()
  
  i=1
  for (v in vCoef){
    bv=(v!=0)
    trueCoef=vTrueCoef[[i]]
    boolTrueCoef=(trueCoef!=0)
    nCorrectNonZero[i]=sum(bv & boolTrueCoef)
    nWrongNonZero[i]=sum(!boolTrueCoef & bv)
    i=i+1
  }
  
  res=data.frame(nCorrectNonZero,nWrongNonZero)
  return(res)
}
