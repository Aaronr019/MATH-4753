#' Normal Function Graph
#'
#' @param mu the mean
#' @param sigma the standard deviation
#' @param a the upper bound of area
#'
#' @return a normal function graph with the given parameters and probability of the desired area
#' @export
#'
#' @examples
#' \dontrun{myncurve(10,5,6)}
myncurve = function(mu, sigma, a){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))

  xcurve <- seq(-1000,a,length=1000)
  ycurve <- dnorm(xcurve,mu,sigma)
  polygon(c(-1000,xcurve,a),c(0,ycurve,0),col="Red")

  prob=pnorm(a,mu,sigma)-pnorm(-1000,mu,sigma)
  prob=round(prob,4)
  mtext(c(prob),side=1,line=2,at=c(7,7))
}
