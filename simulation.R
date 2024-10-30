simulate<-function(calibrate, predict, getCoef,inSampleSet,outOfSample, outOfSampleResponce){
  vmse=c()
  vcoef=list()
  
  i=1
  for(df in inSampleSet){
    m=calibrate(df)
    vcoef[[i]]=getCoef(m)

    predicted=predict(m, outOfSample)
    mse=mean((outOfSampleResponce - predicted)^2)
    vmse=c(vmse,mse)
    i=i+1
  }
  
  res=list()
  res$coef=vcoef
  res$mse=vmse
  
  return(res)
}

evaluateCoef<-function(vCoef, trueCoef){
  nCorrectNonZero=c()
  nWrongNonZero=c()
  boolTrueCoef=(trueCoef!=0)
  
  i=1
  for (v in vCoef){
    bv=(v!=0)
    nCorrectNonZero[i]=sum(bv & boolTrueCoef)
    nWrongNonZero[i]=sum(!boolTrueCoef & bv)
    i=i+1
  }
  
  res=data.frame(nCorrectNonZero,nWrongNonZero)
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
