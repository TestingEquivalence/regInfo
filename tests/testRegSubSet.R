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

# try all three variants "exhaustive","backward", "forward", "seqrep"
method="backward"
# it is possible to use "cp", "bic", "adjR2" , see code below
selector="adjR2"

nvmax=ncol(df)-1
regFit=regsubsets(y ~ ., data = df, nvmax=nvmax, method = method)
regFitSum=summary(regFit)
minCpIndex=which.min(regFitSum$cp)
minBICIndex=which.min(regFitSum$bic)
minAdjR2Index=which.max(regFitSum$adjr2)

if (selector=="cp"){
  ind=minCpIndex
} else if (selector=="bic") {
  ind=minBICIndex
} else if (selector=="adjR2"){
  ind=minAdjR2Index
}

selectedCoefs = regFitSum$which[ind,]
selectedCoefs=selectedCoefs[-1]
allCoef=as.numeric(selectedCoefs)
selectedPredictors=names(selectedCoefs[selectedCoefs==TRUE])
frm= paste0("y~",paste(selectedPredictors, collapse = " + "))
m=lm(frm,df)
