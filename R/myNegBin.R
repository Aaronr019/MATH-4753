#' Negative Binomial Probability Distribution Function
#'
#' @param y quantile to find as a vector
#' @param r size as a vector
#' @param p probability as a vector
#'
#' @return a negative binomial probability distribution
#' @export
#'
#' @examples
#' \dontrun{myNegBin(10,3,0.4)}
myNegBin=function(y,r,p){
  choose(y-1,r-1)*p^r*(1-p)^(y-r)
}
