library(leaps)

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
method="backward"
selector="cp" # "cp", "bic"

nvmax=ncol(df)-1
regFit=regsubsets(y ~ ., data = df, nvmax=nvmax, method = method)
regFitSum=summary(regFit)
minCpIndex=which.min(regFitSum$cp)
minBICIndex=which.min(regFitSum$bic)

if (selector=="cp"){
  ind=minCpIndex
} else if (selector=="bic") {
  ind=minBICIndex
}

selectedCoefs = regFitSum$which[ind,]
selectedCoefs=selectedCoefs[-1]
allCoef=as.numeric(selectedCoefs)
selectedPredictors=names(selectedCoefs[selectedCoefs==TRUE])
frm= paste0("y~",paste(selectedPredictors, collapse = " + "))
m=lm(frm,df)
