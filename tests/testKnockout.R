library(knockoff)
#library(doParallel)

source("regressionModel1.R")
source("simulation.R")
source("regsubsetSel.R")

scenarioNr=1
regMod1=getLinearModel1()
nSample=1

set.seed(10071977)
inSampleSet=regMod1$getSampleScenario(regMod1,scenarioNr,nSample)
df=inSampleSet[[1]]

set.seed(18032024)
outOfSample=regMod1$getSample(regMod1,sizeOOS,regMod1$scenario[[scenarioNr]]$errVariance)
outOfSampleResponce=outOfSample$y
outOfSample$y=NULL

originalKnockoffStat = function(X, X_k, y) {
  abs(t(X) %*% y) - abs(t(X_k) %*% y)
}

defaultStatistic=stat.glmnet_coefdiff

X=df
X$y=NULL
y=df$y

result = knockoff.filter(X, y, fdr = 0.20, statistic = defaultStatistic)

print(result)
summary(result)

if (length(result$selected)>0) {
  selectedPredictors=names(result$selected)
  frm= paste0("y~",paste(selectedPredictors, collapse = " + "))
} else
{
  frm="y~1" 
  selectedPredictors=c()
}

frm
m=lm(frm,df)
summary(m)

predicted=predict(m, outOfSample)
mse=mean((outOfSampleResponce - predicted)^2)
mse


allCoef=colnames(outOfSample)


# Generate a vector of 1s and 0s
allCoef01= ifelse(allCoef %in% selectedPredictors, 1, 0)
allCoef
selectedPredictors
allCoef01



v=getCoef(m)
trueCoef=regMod1$beta
boolTrueCoef=(trueCoef!=0)
bv=(v!=0)
nCorrectNonZero[i]=sum(bv & boolTrueCoef)
nWrongNonZero[i]=sum(!boolTrueCoef & bv)