source("models/regressionModel1.R")
source("simulation.R")
source("selection/LASSO.R")

scenarioNr=1
regMod1=getModel1()
nSample=1

set.seed(10071977)
inSampleSet=regMod1$getSampleScenario(regMod1,scenarioNr,nSample)
df=inSampleSet[[1]]

set.seed(18032024)
sizeOOS=100000
outOfSample=regMod1$getSample(regMod1,sizeOOS,regMod1$scenario[[scenarioNr]]$errVariance)
outOfSampleResponce=outOfSample$y
outOfSample$y=NULL


mLasso=LASSO.calibrate(df,10)
vLasso=LASSO.predict(mLasso,outOfSample)

mRelaxed=LASSO.relaxed.calibrate(df,10)
mRelaxed$lambda.min
mRelaxed$lambda.min=mLasso$lambda.min
vRelaxed=LASSO.predict(mRelaxed,outOfSample)

mAdoptiv=LASSO.adaptive.calibrate(df,10)
mAdoptiv$lambda.min=mLasso$lambda.min
vAdoptiv=LASSO.predict(mAdoptiv,outOfSample)

LASSO.getCoef(mLasso)
LASSO.getCoef(mRelaxed)
LASSO.getCoef(mAdoptiv)

vDiff=vRelaxed-vLasso
min(vDiff)
max(vDiff)

vDiff=vAdoptiv-vLasso
min(vDiff)
max(vDiff)

