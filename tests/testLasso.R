library(leaps)

source("regressionModel1.R")
source("simulation.R")
source("regsubsetSel.R")

scenarioNr=1
regMod1=getLinearModel1()
nSample=1

set.seed(10071977)
inSampleSet=regMod1$getSampleScenario(regMod1,scenarioNr,nSample)
df=inSampleSet[[1]]

source("LASSO.R")
m=LASSO.calibrate(df,10)
v=LASSO.getCoef(m)

