##' @title assess.params
##' @name  assess.params
##' @author Michael Dietze and Ann Raiho \email{dietze@@bu.edu}
##' 
##' @param dat           MCMC output
##' @param Xt            ensemble output matrix
##' @param mu_f_TRUE     muf before tobit2space
##' @param P_f_TRUE      Pf before tobit2space
##' 
##' @description Assessing parameter estimations after mapping model output to tobit space
##' 
##' @return make plots
##' @export
##' 


assessParams <- function(dat, Xt, mu_f_TRUE = NULL, P_f_TRUE = NULL){  
  #mu_f_TRUE and P_f_TRUE used for simulation

  
  #* page 6 looks more like I expected, but I’m not sure how we’re getting negative variances
  
  #* In general, the first 3 pages of pairs plots doesn’t seem to be producing anything too absurd — there’s no estimates going off to large negative values and nothing TOO far from the sample mean. That said, I do find some of the estimates to be surprising (e.g. why is the posterior for mu12 at t=2 lower than the sample mean when the ensemble shouldn’t include any zeros)
  
  imuf   <- grep("muf", colnames(dat))
  muf <- colMeans(dat[, imuf])
  mufT <- apply(Xt,2,mean)
  PfT <- cov(Xt)
  
  mufCI <- apply(dat[,imuf],2,quantile,c(0.025,0.975))
  mufTCI <- apply(Xt,2,quantile,c(0.025,0.975))
  
  par(mfrow=c(1,1))
  plot(mufT,muf,pch=19,ylim=range(mufCI),xlim=range(mufTCI))
  abline(a=0,b=1,lty=2)
  for(i in 1:length(muf)){
    lines(mufTCI[,i],rep(as.vector(muf)[i],2),col=i,lwd=2)
    lines(rep(as.vector(mufT)[i],2),mufCI[,i],col=i,lwd=2)
  }
  
  #muf mufT scatter plot
  par(mfrow=c(2,2))
  for(i in 1:(length(imuf)-1)){
    plot(dat[,i],dat[,i+1],xlab=paste('mu', i),ylab=paste('mu', i+1))
    #points(mu_f_TRUE[i],mu_f_TRUE[i+1],cex=3,col=2,pch=18)
    points(muf[i],muf[i+1],cex=3,col=3,pch=19)
    points(mufT[i],mufT[i+1],cex=3,col=4,pch=20)
  }
  plot.new()
  legend("topleft",legend=c("post","sampT"),col=3:4,pch = 19:20)
  #legend("topleft",legend=c("TRUE","post","sampT"),col=2:4,pch = 18:20)
  
  boxplot(Xt,xlab='State Variables',ylab='X')
  points(muf,col='red',pch=19)
  legend("topleft",legend=c("muf"),col='red',pch = 19)
  
  #cor(dat[,1:6])
  
  iPf   <- grep("pf", colnames(dat))
  Pf <- matrix(colMeans(dat[, iPf]),ncol(X),ncol(X))
  
  PfCI <- apply(dat[,iPf],2,quantile,c(0.025,0.975))

  diag.stopper <- diag(length(muf))
  
  par(mfrow=c(1,1))
  plot(PfT,Pf,ylim=range(PfCI),pch=19,xlab='Pf Ensemble (True)',ylab='Pf Estimated (tobit2space)')
  abline(0,1,lty=2)
  for(i in 1:length(Pf)){
    lines(rep(as.vector(PfT)[i],2),PfCI[,i],col=i,lwd=2)
    if(diag.stopper[i]==1){
      points(PfT[i],Pf[i],cex=2,pch = 7)
    }
  }
  legend('topleft','variance',pch = 7,cex=2)
  
  diag.stopper2 <- diag.stopper+1
  diag(diag.stopper2) <- 0
  
  plot(cov2cor(PfT)[which(diag.stopper2==1)],
       cov2cor(Pf)[which(diag.stopper2==1)],pch=19,
       ylab = 'Pf', xlab = 'Pft', main = 'Correlations')
  abline(a=0,b=1,lty=2)
  
  corrCI <- apply(dat[,iPf[which(diag.stopper2!=0)]],2,quantile,c(0.025,0.975))
  
  par(mfrow=c(1,1))
  plot(PfT[which(diag.stopper2!=0)],Pf[which(diag.stopper2!=0)],
       ylim=range(corrCI),pch=19,xlab='Pf Ensemble (True)',
       ylab='Pf Estimated (tobit2space)',
       main='Non-Diagonal Covariance')
  abline(a=0,b=1,lty=2)
  for(i in 1:length(Pf)){
    if(diag.stopper2[i]==1){
      lines(rep(as.vector(PfT)[i],2),PfCI[,i],col=i,lwd=2)
    }
  }
  
  par(mfrow=c(1,1))
  plot(diag(PfT)-diag(Pf),xlab='State Variable',pch=19,
       cex=2,main='Which variance changed the most?')
  
  
  #var.change <- data.frame(mufT = signif(colMeans(Xt),digits=2),muf=signif(muf,digits=2),abs.change.var = abs(diag(PfT)-diag(Pf)))
  #var.change[order(var.change$abs.change.var),]
  
  # sort(diag(Pf)-diag(PfT),decreasing = T)
  # 
  # par(mfrow=c(3,3))
  # for(i in 1:length(Pf)) {
  #   if(diag.stopper[i]==1){
  #   plot(dat[,i+14],ylim=c(0,10)); abline(h=as.vector(PfT)[i],col='red',lwd=2)
  #   }
  # }
  #scatterplots
  #var 
  #corr #pull diags out ==1 
  #check mu v var to make sure variance is only changing near 0# shifts in Xt v X on same plot
  
  #PfT <- cov(Xt)
  #points(P_f_TRUE,PfT,col=1:14,pch="-",cex=2)
}
