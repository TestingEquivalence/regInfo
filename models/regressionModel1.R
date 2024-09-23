# This regression model is originally from the paper:
# W. Sauerbrei, N. Holländer and A. Buchholz, “Investigation about a Screening Step in Model Selection,” 
# Statistics and Computing, Vol. 18, No. 2, 2008, pp. 195-208. doi:10.1007/s11222-007-9048-5

# This model is also used in the paper:
# Open Journal of Statistics, 2013, 3, 79-102
# http://dx.doi.org/10.4236/ojs.2013.32011 Published Online April 2013 (http://www.scirp.org/journal/ojs)
# Hans C. van Houwelingen, Willi Sauerbrei
# "Cross-Validation, Shrinkage and Variable Selection in Linear Regression Revisited"

library(mvtnorm)

getLinearModel1<-function(){
  m=list()
  m$nOfCovariates=15
  
  # Create the covariance matrix
  sigma=diag(1, m$nOfCovariates)  # Create a diagonal matrix with diagonal entries as 1
  sigma[1,5]=0.7
  sigma[5,1]=sigma[1,5]
  sigma[1,10]=0.5
  sigma[10,1]=sigma[1,10]
  sigma[2,6]=0.5
  sigma[6,2]=sigma[2,6]
  sigma[4,8]=-0.7
  sigma[8,4]=sigma[4,8]
  sigma[7,8]=0.3
  sigma[8,7]=sigma[7,8]
  sigma[7,14]=0.5
  sigma[14,7]=sigma[7,14]
  sigma[9,13]=0.5
  sigma[13,9]=sigma[9,13]
  sigma[11,12]=0.7
  sigma[12,11]=sigma[11,12]
  m$sigma=sigma
  
  beta=rep(0, m$nOfCovariates)
  beta[4]=-0.5
  beta[5]=0.5
  beta[6]=0.5
  beta[7]=0.5
  beta[8]=1
  beta[9]=1
  beta[10]=1.5
  m$beta=beta
  
  m$oracle="y~x4+x5+x6+x7+x8+x9+x10"
  
  scenario=list()
  scenario[[1]]=list(n=100, errVariance=6.25)
  scenario[[2]]=list(n=100, errVariance=2.50)
  scenario[[3]]=list(n=400, errVariance=6.25)
  scenario[[4]]=list(n=400, errVariance=2.50)
  
  m$scenario=scenario
  
  m$getSampleCovariates<-function(m,n){
    return(rmvnorm(n,sigma=m$sigma))
  }
  
  m$getSample<-function(m,n, errVariance){
    x=m$getSampleCovariates(m,n)
    y=x %*% m$beta
    y=y[,1]
    err=rnorm(n,0,sqrt(errVariance))
    y=y+err
    x=as.data.frame(x)
    colnames(x)=paste0("x",c(1:m$nOfCovariates))
    x$y=y
    return(x)
  }
  
  m$getSampleScenario<-function(m,scenarioNr, nSample){
    errVariance=m$scenario[[scenarioNr]]$errVariance
    n=m$scenario[[scenarioNr]]$n
    
    res=list()
    for (i in c(1:nSample)){
      res[[i]]=m$getSample(m,n,errVariance)
    }
    return(res)
  }
  
  return(m)
}