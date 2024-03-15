mod1=getLinearModel1()
x=mod1$getSampleCovariates(mod1,100000)
covMat=cov(x)

covMat[1,5] # 0.7
covMat[5,1] # 0.7
