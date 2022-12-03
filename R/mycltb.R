#' Central Limit Theorem from a Binomial Distribution
#'
#' @param n the sample
#' @param iter the number of iterations
#' @param p the probability
#'
#' @return a histogram of the density versus the sample mean
#' @export
#'
#' @examples
#' \dontrun{mycltb(4,10000,.3)}
mycltb=function(n,iter,p){
  y=rbinom(n*iter,size=n,prob=p)
  data=matrix(y,nr=n,nc=iter,byrow=TRUE)
  w=apply(data,2,mean)
  param=hist(w,plot=FALSE)
  ymax=max(param$density)
  ymax=1.1*ymax
  hist(w,freq=FALSE,  ylim=c(0,ymax),
       main=paste("Histogram of sample mean","\n", "sample size= ",n,sep=""),
       xlab="Sample mean")
  curve(dnorm(x,mean=n*p,sd=sqrt(p*(1-p))),add=TRUE,col="Red",lty=2,lwd=3)
}
