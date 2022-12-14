% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mycltb.R
\name{mycltb}
\alias{mycltb}
\title{Central Limit Theorem from a Binomial Distribution}
\usage{
mycltb(n, iter, p)
}
\arguments{
\item{n}{the sample}

\item{iter}{the number of iterations}

\item{p}{the probability}
}
\value{
a histogram of the density versus the sample mean
}
\description{
Central Limit Theorem from a Binomial Distribution
}
\examples{
\dontrun{mycltb(4,10000,.3)}
}
