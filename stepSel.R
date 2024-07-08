library(leaps)

# try all three variants "exhaustive","backward", "forward", "seqrep"
step.calibrate<-function(df, direction){
  m=lm("y~.",df)
  finalm = step(m, direction = direction, k = 2, trace=0)
  finalm$df=df
  return(finalm)
}

step.getCoef<-function(m){
  df=m$df
  df$y=NULL
  allNames=names(df)
  coefSelected=coef(m)
  coefSelected=coefSelected[-1]
  nonSelected =setdiff(allNames, names(coefSelected))
  
  allCoef = rep(0, length(allNames))
  names(allCoef)=allNames
  allCoef[names(coefSelected)] = coefSelected
  return(allCoef)
}
