source("models/regressionModel1.R")
source("simulation.R")
source("selection/LASSO.R")

scenarioNr=1
regMod1=getLinearModel1()
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
mLasso$bestLambda
vLasso=LASSO.predict(mLasso,outOfSample)

mRelaxed=LASSO.relaxed.calibrate(df,10)
mRelaxed$bestLambda
mRelaxed$bestLambda=mLasso$bestLambda
vRelaxed=LASSO.predict(mRelaxed,outOfSample)

LASSO.getCoef(mLasso)
LASSO.getCoef(mRelaxed)
vDiff=vRelaxed-vLasso
min(vDiff)
max(vDiff)


## Perform ridge regression using  CV
x=df
x$y=NULL
x=as.matrix(x)
y=df$y
rcv = cv.glmnet(x, y, type.measure = "mse", nfold = 10,
                       #  ‘alpha = 0’ the ridge penalty.
                       alpha = 0)
# cv optimal ridge coefficients, without intercept
cvRidgeCoef = as.numeric(coef(rcv, s = rcv$lambda.min))[-1]

# Perform adaptive LASSO using cv and ridge coefficients
fit = cv.glmnet(x, y, type.measure = "mse",
                        nfold = 10,
                        # ‘alpha = 1’ is the lasso penalty
                        alpha = 1,
                        penalty.factor = 1 / abs(cvRidgeCoef))


