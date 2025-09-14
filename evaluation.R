eval.transformToDF<-function(simRes, key="predSSE"){
  rr=do.call(Map, c(f = list, simRes)) 
  
  res=list()
  for (e in rr){
    rrr=do.call(rbind, lapply(e, as.data.frame))
    res=append(res,list(rrr))
  }
  
  rres=lapply(res, `[[`, key)
  rres=as.data.frame(rres)
  
  # Rename columns m1, m2, ...
  names(rres)=paste0("m", seq_along(rres))
  return(rres)
}

eval.refModRes<-function(simRes){
  df=eval.transformToDF(simRes)
  refModRes=data.frame(predSSE=df$m1)
  df=eval.transformToDF(simRes, "maxSSE")
  refModRes$maxSSE=df$m1
  df=eval.transformToDF(simRes, "minSSE")
  refModRes$minSSE=df$m1
  return(refModRes)
}

eval.meanQtSSE<-function(v,df){
  qt=as.data.frame(sweep(df, 1, v, `/`))
  qt=qt-1
  qt=colMeans(qt)
  return(qt)
}

eval.QtMeanSSE<-function(v,df){
  mv=mean(v)
  df=df/mv
  df=df-1
  res=colMeans(df)
  return(res)
}