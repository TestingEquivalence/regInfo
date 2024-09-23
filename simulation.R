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
                    getInSample,getOutOfSample, outOfSampleResponce){
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
  
  # for (i in 1:10) {
  #   # Set a unique seed in each iteration
  #   seed_value <- 1234 + i * 100  # Change this multiplier to space seeds
  #   set.seed(seed_value)
  #   
  #   # Generate random numbers (e.g., 5 random numbers from a normal distribution)
  #   random_numbers <- rnorm(5)
  #   
  #   # Print or use the random numbers
  #   print(paste("Iteration", i, "with seed", seed_value, "generated:", paste(random_numbers, collapse = ", ")))
  # }
}