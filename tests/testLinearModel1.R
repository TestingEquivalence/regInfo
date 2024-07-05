source("regressionModel1.R")

mod1=getLinearModel1()
x=mod1$getSampleCovariates(mod1,100000)
covMat=cov(x)

covMat[1,5] # 0.7
covMat[1,10] # 0.5
covMat[2, 6]
covMat[4,8]
covMat[7,8]
covMat[7,14]
covMat[9,13]
covMat[11,12]

df=mod1$getSample(mod1,10000,3)
sc=mod1$getSampleScenario(mod1,1,10000)

v=mod1$beta
bv=(v!=0)

rv=bv
rv[4]=FALSE
rv[5]=FALSE
rv[1]=TRUE

nCorrect=sum(rv & bv)
nFalsePositive=sum(!bv & rv)
sum(rv)  

df=inSampleSet[[1]]
x=df
x$y=NULL
x=as.matrix(x)
y=df$y
fit = glmnet(x, y, alpha = 1)

# perform cross-validation
cvfit = cv.glmnet(x, y, nfolds=10,alpha = 1)
bestLambda = cvfit$lambda.min
finalCoef = predict.glmnet(m, s = m$bestLambda, type = "coefficients")

# test knockoff
df=inSampleSet[[1]]
m=knockoff.calibrate(df, fdr=0.2)
