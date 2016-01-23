\name{chinook_resampL}
\alias{chinook_resampL}
\docType{data}
\title{Chinook salmon length, bootstrap resampled}
\description{
Bootstrap resampled Chinook salmon fork length (mm) at hatch. Number of iterations was 5.
}
\usage{data("chinook_resampL")}
\format{
  A data frame with 1210 observations on the following 30 variables.
  \describe{
    \item{\code{dam1},}{a numeric vector}
    \item{\code{sire1},}{a numeric vector}
    \item{\code{tray1},}{a numeric vector}
    \item{\code{cell1},}{a numeric vector}
    \item{\code{length1},}{a numeric vector}
    \item{\code{egg_size1},}{a numeric vector}
    \item{\code{dam2},}{a numeric vector}
    \item{\code{sire2},}{a numeric vector}
    \item{\code{tray2},}{a numeric vector}
    \item{\code{cell2},}{a numeric vector}
    \item{\code{length2},}{a numeric vector}
    \item{\code{egg_size2},}{a numeric vector}
    \item{\code{dam3},}{a numeric vector}
    \item{\code{sire3},}{a numeric vector}
    \item{\code{tray3},}{a numeric vector}
    \item{\code{cell3},}{a numeric vector}
    \item{\code{length3},}{a numeric vector}
    \item{\code{egg_size3},}{a numeric vector}
    \item{\code{dam4},}{a numeric vector}
    \item{\code{sire4},}{a numeric vector}
    \item{\code{tray4},}{a numeric vector}
    \item{\code{cell4},}{a numeric vector}
    \item{\code{length4},}{a numeric vector}
    \item{\code{egg_size4},}{a numeric vector}
    \item{\code{dam5},}{a numeric vector}
    \item{\code{sire5},}{a numeric vector}
    \item{\code{tray5},}{a numeric vector}
    \item{\code{cell5},}{a numeric vector}
    \item{\code{length5},}{a numeric vector}
    \item{\code{egg_size5},}{a numeric vector}
  }
}
\source{
Pitcher TE, Neff BD. 2007. Genetic quality and offspring performance in Chinook salmon:
implications for supportive breeding. Conservation Genetics 8(3):607-616.
DOI: 10.1007/s10592-006-9204-z
}
\examples{
data(chinook_resampL)
#the five models
length_rcomp1<- resampLmer(resamp=chinook_resampL,dam="dam",sire="sire",response="length",
start=1,end=5)  #full analysis should use 1,000 models
}
\keyword{datasets}