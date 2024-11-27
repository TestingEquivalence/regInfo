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

appendSummary<-function(ls,df, name, nCorrect){
  ls$name=c(ls$name, name)
  ls$mse=c(ls$mse, mean(df$mse))
  ls$sd.mse=c(ls$sd.mse, sd(df$mse))
  
  ls$rSquared =c(ls$rSquared, mean(df$rSquared))
  ls$sd.rSquared=c(ls$sd.rSquared,   sd(df$rSquared))
  
  ls$nCorrectNonZero=c(ls$nCorrectNonZero, mean(df$nCorrectNonZero))
  ls$sd.nCorrectNonZero=c(ls$sd.nCorrectNonZero, sd(df$nCorrectNonZero))
  
  ls$nWrongNonZero= c(ls$nWrongNonZero, mean(df$nWrongNonZero))
  ls$sd.nWrongNonZero=c(ls$sd.nWrongNonZero,  sd(df$nWrongNonZero))
  
  tdr=df$nCorrectNonZero/nCorrect
  ls$tdr= c(ls$tdr, mean(tdr))
  ls$sd.tdr= c(ls$sd.tdr, sd(tdr))
  
  v=df$nCorrectNonZero+df$nWrongNonZero
  fdr=ifelse(v == 0, 0, df$nWrongNonZero / v)
  ls$fdr=c(ls$fdr, mean(fdr))
  ls$sd.fdr=c(ls$sd.fdr,  sd(fdr))
  
  return(ls)
}

startSummary<-function(){
  ls=list()
  ls$name=vector("character")
  ls$mse=vector("numeric")
  ls$sd.mse=vector("numeric")
  
  ls$rSquared = vector("numeric")
  ls$sd.rSquared=vector("numeric")
  
  ls$nCorrectNonZero= vector("numeric")
  ls$sd.nCorrectNonZero= vector("numeric")
  
  ls$nWrongNonZero= vector("numeric")
  ls$sd.nWrongNonZero= vector("numeric")
  
  ls$tdr=vector("numeric")
  ls$sd.tdr=vector("numeric")
  
  ls$fdr=vector("numeric")
  ls$sd.fdr=vector("numeric")
  
  return(ls)
}
