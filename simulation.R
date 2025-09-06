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

evaluate<-function(oos,pred){
  res=list()
  
  for(py in pred){
   r=list()
   
   v=py-mean(py)
   r$maxSSE=mean(v^2)
   
   v=oos$y-py
   r$predSSE=mean(v^2)
   
   v=oos$err
   r$minSSE=mean(v^2)
   
   res=append(res,r)
  }
}