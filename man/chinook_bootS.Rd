\name{chinook_bootS}
\alias{chinook_bootS}
\docType{data}
\title{Chinook salmon survival, bootstrap data}
\description{
Bootstrap resampled Chinook salmon binary survival to hatch (1 is alive, 0 is dead)
with the amount of additive genetic, non-additive genetic, and maternal variance calculations.
}
\usage{data("chinook_bootS")}
\format{
  A data frame with 1000 observations on the following 8 variables.
  \describe{
    \item{\code{dam.sire},}{a numeric vector.}
    \item{\code{sire},}{a numeric vector.}
    \item{\code{dam},}{a numeric vector.}
    \item{\code{Residual},}{a numeric vector.}
    \item{\code{Total},}{a numeric vector.}
    \item{\code{additive},}{a numeric vector.}
    \item{\code{maternal},}{a numeric vector.}
    \item{\code{nonadd},}{a numeric vector.}
  }
}
\details{
Also includes the calculations for the amount of variance explained by dam by sire, sire, dam,
residual, and total.
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
data(chinook_bootS)
## Extract bootstrap confidence interval
ciMANA(comp=chinook_bootS)
}
\keyword{datasets}
