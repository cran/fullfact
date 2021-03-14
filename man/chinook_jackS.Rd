\name{chinook_jackS}
\alias{chinook_jackS}
\docType{data}
\title{Chinook salmon survival, jackknife data}
\description{
Jackknife resampled Chinook salmon survival with the amount of additive genetic,
non-additive genetic, and maternal variance calculations. Jackknife resampling was leave-out-30.
}
\usage{data("chinook_jackS")}
\format{
  A data frame with 1210 observations on the following 9 variables.
  \describe{
    \item{\code{dam.sire},}{a numeric vector.}
    \item{\code{sire},}{a numeric vector.}
    \item{\code{dam},}{a numeric vector.}
    \item{\code{Residual},}{a numeric vector.}
    \item{\code{Total},}{a numeric vector.}
    \item{\code{additive},}{a numeric vector.}
    \item{\code{nonadd},}{a numeric vector.}
    \item{\code{maternal},}{a numeric vector.}
  }
}
\details{
Also includes the calculations for the amount of variance explained by dam by sire,
sire, dam, residual, and total.
}
\source{
http://link.springer.com.proxy1.lib.uwo.ca/article/10.1007%2Fs10592-006-9204-z
}
\references{
Pitcher TE, Neff BD. 2007. Genetic quality and offspring performance in Chinook salmon:
implications for supportive breeding. Conservation Genetics 8(3):607-616.
DOI: 10.1007/s10592-006-9204-z
}
\examples{
data(chinook_jackS)
## Extract jackknifed confidence interval
ciJack(comp=chinook_jackS,full=c(0.6655,0.6692,0.6266,4.4166))
}
\keyword{datasets}
