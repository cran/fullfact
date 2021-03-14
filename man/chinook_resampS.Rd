\name{chinook_resampS}
\alias{chinook_resampS}
\docType{data}
\title{Chinook salmon survival, bootstrap resampled}
\description{
Bootstrap resampled Chinook salmon binary survival to hatch (1 is alive, 0 is dead).
Number of iterations was 5.
}
\usage{data("chinook_resampS")}
\format{
  A data frame with 36300 observations on the following 30 variables.
  \describe{
    \item{\code{status1},}{a numeric vector}
    \item{\code{dam1},}{a numeric vector}
    \item{\code{sire1},}{a numeric vector}
    \item{\code{tray1},}{a numeric vector}
    \item{\code{cell1},}{a numeric vector}
    \item{\code{egg_size1},}{a numeric vector}
    \item{\code{status2},}{a numeric vector}
    \item{\code{dam2},}{a numeric vector}
    \item{\code{sire2},}{a numeric vector}
    \item{\code{tray2},}{a numeric vector}
    \item{\code{cell2},}{a numeric vector}
    \item{\code{egg_size2},}{a numeric vector}
    \item{\code{status3},}{a numeric vector}
    \item{\code{dam3},}{a numeric vector}
    \item{\code{sire3},}{a numeric vector}
    \item{\code{tray3},}{a numeric vector}
    \item{\code{cell3},}{a numeric vector}
    \item{\code{egg_size3},}{a numeric vector}
    \item{\code{status4},}{a numeric vector}
    \item{\code{dam4},}{a numeric vector}
    \item{\code{sire4},}{a numeric vector}
    \item{\code{tray4},}{a numeric vector}
    \item{\code{cell4},}{a numeric vector}
    \item{\code{egg_size4},}{a numeric vector}
    \item{\code{status5},}{a numeric vector}
    \item{\code{dam5},}{a numeric vector}
    \item{\code{sire5},}{a numeric vector}
    \item{\code{tray5},}{a numeric vector}
    \item{\code{cell5},}{a numeric vector}
    \item{\code{egg_size5},}{a numeric vector}
  }
}
\source{
Pitcher TE, Neff BD. 2007. Genetic quality and offspring performance in Chinook salmon:
implications for supportive breeding. Conservation Genetics 8(3):607-616.
DOI: 10.1007/s10592-006-9204-z
}
\examples{
data(chinook_resampS)
\dontrun{survival_rcomp<- resampGlmer(resamp=chinook_resampS,dam="dam",sire="sire",
response="status",fam_link=binomial(link="logit"),start=1,end=1000)}
}
\keyword{datasets}
