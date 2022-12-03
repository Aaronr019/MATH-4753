#' Tickets to be Sold
#'
#' @param N the number of seats in a flight
#' @param gamma the probability a plane will be truly overbooked
#' @param p the probability of a "show"
#'
#' @return The number of tickets to be sold for a plane
#' @export
#'
#' @examples
#' \dontrun{ntickets(200,0.02,0.95)}
ntickets <- function(N, gamma, p){
  n <- seq(N, floor(N+N/10), by = 1)
  tmp <- 1-gamma-pbinom(q = N, size = n, prob = p)
  ind <- which.min(abs(tmp))
  tmp2 <- 1 - gamma - pnorm(N, p*n, sqrt((n*p)*(1-p)))
  ind2 <- which.min(abs(tmp2))
  nd <- N+(ind-1)
  nc <- N+(ind2-1)

  cat("nd =", nd,"\n")
  cat("nc =", nc,"\n")
  cat("N =", N, "\n")
  cat("p =", p, "\n")
  cat("gamma =", gamma, "\n")

  layout(matrix(1:2, nrow=2, ncol=1))
  layout.show(2)

  plot(x = n,y = 1-gamma-pbinom(q = N, size = n, prob = p), type = "b", col = "blue", xlab = "n", ylab = "Objective Function", main = "Discrete Case")
  abline(v = nd)
  plot(x = n, y = 1 - gamma - pnorm(N, p*n, sqrt((n*p)*(1-p))), type = "b", col = "red", xlab = "n", ylab = "Objective Function", main = "Continuous Case")
  abline(v=nc)
}
