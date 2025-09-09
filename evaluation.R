eval.transformToDF<-function(simRes){
  rr=do.call(Map, c(f = list, simRes)) 
  
  res=list()
  for (e in rr){
    rrr=do.call(rbind, lapply(e, as.data.frame))
    res=append(res,list(rrr))
  }
  
  rres=lapply(res, `[[`, "predSSE")
  rres=as.data.frame(rres)
  
  # Rename columns m1, m2, ...
  names(rres)=paste0("m", seq_along(rres))
  return(rres)
}