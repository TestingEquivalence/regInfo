simulate<-function(getModel, getSample, getCalibration, getPrediction,
                   nSimulation, sizeInSample, sizeOutOfSample, resetEveryRun=false){
   
   res=list()
   
   if (!resetEveryRun){
     set.seed(10071977)
     model=getModel()
     oos=getSample(model,n=sizeOutOfSample)  
   }
   
   
   for (i in c(1:nSimulation)){
     set.seed(i*10071977)
     if (resetEveryRun){
       model=getModel()
       oos=getSample(model,n=sizeOutOfSample)  
     }
     sample=getSample(model,n=sizeInSample)
     mods=getCalibration(sample)
     pred=getPrediction(mods,oos)
     res[i]=evaluate(oos,pred)
   }
   
   return (res)
}

simulatePartial<-function(getCalibration, getPrediction, inSamples, oos){
  
  res=list()
  
  i=1
  for (sample in inSamples){
    mods=getCalibration(sample$data)
    pred=getPrediction(mods,oos$data)
    r=evaluate(oos,pred)
    res[[i]]=r
    print(paste0("sim: ",i))
    i=i+1
  }
  
  res=as.data.frame(do.call(rbind, res))
  return (res)
}

evaluate<-function(oos,pred){
  m=length(pred)
  res=numeric(m)
  i=1
  for(py in pred){
   v=oos$data$y-py
   predSSE=mean(v^2)
   
   res[i]=predSSE
   i=i+1
  }
  return(res)
}

getSampleLM<-function(m,n){
  res=list()
  x=rmvnorm(n,sigma=m$sigma)
  f=x %*% m$beta
  f=f[,1]
  
  err=rnorm(n,0,sqrt(m$errVariance))
  y=f+err
  
  x=as.data.frame(x)
  colnames(x)=paste0("x",c(1:m$d))
  x$y=y
  
  res$data=x
  res$err=err
  res$f=f
  return(res)
}  

getSamples<-function(m,n, nSample, getSample){
  res=list()
  for (i in 1:nSample){
    res=append(res, list(getSample(m,n)))
  }
  return(res)
}

getReferenceLM<-function(data){
  m=lm(y~.,data)
  return(list(m))
}

getPredictionLM<-function(mods,data){
  res=list()
  for (m in mods){
    v=predict.lm(m,data)
    res=append(res,list(v))
  }
  return(res)
}

getRelMSE<-function(v,df){
  qt=df /v
  qt=qt-1
  return(qt)
}