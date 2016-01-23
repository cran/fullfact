\name{chinook_jackL}
\alias{chinook_jackL}
\docType{data}
\title{Chinook salmon length, jackknife data}
\description{
Jackknife resampled Chinook salmon fork length (mm) at hatch with the amount of additive genetic,
non-additive genetic, and maternal variance calculations. Jackknife resampling was leave-out-one.
}
\usage{data("chinook_jackL")}
\format{
  A data frame with 1210 observations on the following 9 variables.
  \describe{
    \item{\code{dam.sire},}{a numeric vector.}
    \item{\code{tray},}{a numeric vector.}
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
Also includes the calculations for the amount of variance explained by position (tray), dam by sire,
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
data(chinook_jackL)
## Extract jackknifed confidence interval
ciJack(comp=chinook_jackL,full=c(0.0000000,0.7192253,0.2029684,1.0404425))
}
\keyword{datasets}
