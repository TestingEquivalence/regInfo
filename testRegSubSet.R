source("regressionModel1.R")
source("simulation.R")
source("stepwiseSelection.R")

scenarioNr=1
regMod1=getLinearModel1()
nSample=1

set.seed(10071977)
inSampleSet=regMod1$getSampleScenario(regMod1,scenarioNr,nSample)
df=inSampleSet[[1]]

# try all three variants "exhaustive","backward", "forward", "seqrep"

m=step.calibrate(df=df, direction="backQward")
