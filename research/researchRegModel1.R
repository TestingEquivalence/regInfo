source("dataModels/regressionModel1.R")
source("simulation.R")
source("evaluation.R")

m=getModel1()
nSamples=2 #10000
sizeOOS=100000

set.seed(10071977)
inSamples=getSamples(m,n=m$sizeInSample, nSample=nSamples,getSample=getSampleLM)
oos=getSampleLM(m,n=sizeOOS)

# calibrate reference model

simRes=simulatePartial(getCalibration = getReferenceLM, 
                       getPrediction = getPredictionLM, 
                       inSamples, oos)

tSimRes=eval.transformToDF(simRes)






print(result)