library(olsrr)

source("models/regressionModel1.R")
source("simulation.R")

scenarioNr=1
regMod1=getLinearModel1()
nSample=1

set.seed(10071977)
inSampleSet=regMod1$getSampleScenario(regMod1,scenarioNr,nSample)
df=inSampleSet[[1]]

# stepwise forward regression
m=lm("y~.",df)
sm=ols_step_backward_p(m, p_val = 0.05)



