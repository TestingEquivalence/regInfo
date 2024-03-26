calibrate<-function(df){
  return(regMod1$beta)
}
predict<-function(m, outOfSample){
  y=as.matrix(outOfSample) %*% m
  y=y[,1]
  return(y)
}
