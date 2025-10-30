source("dataModels/regressionModel1.R")
source("simulation.R")
source("evaluation.R")
source("models/regsubsetSel.R")
source("models/LASSO.R")

m=getModel1()
nSamples=10000
sizeOOS=100000

# result
model1=list()

# load result if available
model1=readRDS("model1.rds")


set.seed(10071977)
inSamples=getSamples(m,n=m$sizeInSample, nSample=nSamples,getSample=getSampleLM)
oos=getSampleLM(m,n=sizeOOS)

# calibrate reference model
v=simulatePartial(getCalibration = getReferenceLM, 
                  getPrediction = getPredictionLM, 
                  inSamples, oos)
v=v$V1


# calibrate backward model

simRes=simulatePartial(getCalibration = backward.calibrate, 
                       getPrediction = getPredictionLM, 
                       inSamples, oos)
df=getRelMSE(v,simRes)
model1$backward=df

# calibrate forward model

simRes=simulatePartial(getCalibration = forward.calibrate, 
                       getPrediction = getPredictionLM, 
                       inSamples, oos)

df=getRelMSE(v,simRes)
model1$forward=df

# calibrate lasso

simRes=simulatePartial(getCalibration = LASSO.calibrate, 
                       getPrediction = LASSO.predict, 
                       inSamples, oos)

df=getRelMSE(v,simRes)
model1$lasso=df

# save result 
saveRDS(model1,"model1.rds")


# post-processing of the simulation results

longRes=eval.lsDF2long(model1)
sum=eval.Summary(longRes)

write_ods(sum, "summary _model1.ods")

#  filter summary for plot
sum=sum[sum$size>2,]
sum=sum[sum$size<15,]

plt=p_facet_meanSD(sum, free_y = FALSE)
plt
plt=p_facet_prop(sum)
plt
plt=p_overlay_mean(sum)
plt
plt=p_overlay_median(sum)
plt
plt=p_overlay_prop(sum)
plt

# distribution of minimal model sizes across repeats, which rel MSE is below 0 first time

freqMinSizes=freq_min_sizes(longRes)
plt=p_overlay_freq(freqMinSizes)
plt





# calibrate relaxed lasso with fixed relaxation parameter

# gamma=1 yields lasso, gamma=0 is completely relaxed, also SSE refit

getCalibration=function(data){
  RelaxedLasso.calibrate(data, gamma=0.75)
}

simRes=simulatePartial(getCalibration = getCalibration, 
                       getPrediction = RelaxedLasso.predict, 
                       inSamples, oos)
df=eval.transformToDF(simRes)


meanQtSSE$rlasso_075=eval.meanQtSSE(v,df)
QtOfMeansSSE$rlasso_075=eval.QtMeanSSE(v,df)

# calibrate  relaxed lasso using best CV relaxation parameter for every model size

simRes=simulatePartial(getCalibration = relaxedLassoCV.calibrate, 
                       getPrediction = relaxedLassoCV.predict, 
                       inSamples, oos)
df=eval.transformToDF(simRes)


meanQtSSE$rlassoCV=eval.meanQtSSE(v,df)
QtOfMeansSSE$rlassoCV=eval.QtMeanSSE(v,df)

# calibrate relaxed elastic net with fixed parameter
# gamma=1 yields is not relaxed; gamma=0 is completely relaxed, also SSE refit
# alpha=1 yields lasso, alpha =0 yields ridge regression

getCalibration=function(data){
  elastic.net.fixed.calibrate(data, gamma=0, alpha=0.75)
}

simRes=simulatePartial(getCalibration = getCalibration, 
                       getPrediction = elastic.net.fixed.predict, 
                       inSamples, oos)
df=eval.transformToDF(simRes)


meanQtSSE$elasticNet_gamma0_alpha075=eval.meanQtSSE(v,df)
QtOfMeansSSE$elasticNet_gamma0_alpha075=eval.QtMeanSSE(v,df)

# calibrate relaxed elastic net using CV for optimal gamma and alpha

simRes=simulatePartial(getCalibration = elasticNet.CV.calibrate, 
                       getPrediction = elasticNet.CV.predict, 
                       inSamples, oos)
df=eval.transformToDF(simRes)

# calibrate using knockout techniques
simRes=simulatePartial(getCalibration = knockoff.calibrate, 
                       getPrediction = getPredictionLM,
                       inSamples, oos)
df=eval.transformToDF(simRes)


meanQtSSE$elasticNetCV=eval.meanQtSSE(v,df)
QtOfMeansSSE$elasticNetCV=eval.QtMeanSSE(v,df)



