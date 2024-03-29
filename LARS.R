library(glmnet)

df=inSampleSet[[1]]

x=df
x$y=NULL
x=as.matrix(x)
y=df$y

# fit LARS model
# alpha=1 is the lasso penalty
fit = glmnet(x, y, alpha = 1)

# perform cross-validation
cvfit = cv.glmnet(x, y, alpha = 1) # alpha=1 is the lasso penalty 

# Plot cross-validation results (optional)
plot(cvfit)

# get the best lambda value
bestLambda = cvfit$lambda.min

# Get coefficients corresponding to the best lambda
finalCoef = predict(fit, s = bestLambda, type = "coefficients")
vFinalCoef= as.matrix(finalCoef)
vFinalCoef=as.vector(vFinalCoef)
names(vFinalCoef)=finalCoef@Dimnames[[1]]
c=vFinalCoef[1]
vFinalCoef=vFinalCoef[-1]
