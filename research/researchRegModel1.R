source("dataModels/regressionModel1.R")
source("simulation.R")
source("evaluation.R")
source("models/regsubsetSel.R")

m=getModel1()
nSamples=10000
sizeOOS=100000

# result
meanQtSSE=list()
QtOfMeansSSE=list()

# load result if available
meanQtSSE=readRDS("meanQtSSE.rds")
QtOfMeansSSE=readRDS("QtOfMeansSSE.rds")

set.seed(10071977)
inSamples=getSamples(m,n=m$sizeInSample, nSample=nSamples,getSample=getSampleLM)
oos=getSampleLM(m,n=sizeOOS)

# calibrate reference model
simRes=simulatePartial(getCalibration = getReferenceLM, 
                       getPrediction = getPredictionLM, 
                       inSamples, oos)

resRefMod=eval.refModRes(simRes)
v=resRefMod$predSSE

# calibrate backward model

simRes=simulatePartial(getCalibration = backward.calibrate, 
                       getPrediction = getPredictionLM, 
                       inSamples, oos)
df=eval.transformToDF(simRes)

meanQtSSE$backward=eval.meanQtSSE(v,df)
QtOfMeansSSE$backward=eval.QtMeanSSE(v,df)

# calibrate forward model

simRes=simulatePartial(getCalibration = forward.calibrate, 
                       getPrediction = getPredictionLM, 
                       inSamples, oos)
df=eval.transformToDF(simRes)

meanQtSSE$forward=eval.meanQtSSE(v,df)
QtOfMeansSSE$forward=eval.QtMeanSSE(v,df)


# save results
meanQtSSE=as.data.frame(meanQtSSE)
QtOfMeansSSE=as.data.frame(QtOfMeansSSE)

saveRDS(meanQtSSE,"meanQtSSE.rds")
saveRDS(QtOfMeansSSE,"QtOfMeansSSE.rds")

write.csv(meanQtSSE,"meanQtSSE.csv")
write.csv(QtOfMeansSSE,"QtOfMeansSSE.csv")
