\name{chinook_survival}
\alias{chinook_survival}
\docType{data}
\title{Chinook salmon survival, raw data}
\description{
Raw Chinook salmon numbers alive and dead to hatching of offspring produced using
an 11 x 11 full factorial breeding design.
}
\usage{data("chinook_survival")}
\format{
  A data frame with 242 observations on the following 9 variables.
  \describe{
    \item{\code{family},}{a factor with levels: \code{f1} \code{f10} \code{f100} \code{f101} \code{f102} \code{f103} \code{f104} \code{f105} \code{f106} \code{f107} \code{f108} \code{f109} \code{f11} \code{f110} \code{f111} \code{f112} \code{f113} \code{f114} \code{f115} \code{f116} \code{f117} \code{f118} \code{f119} \code{f12} \code{f120} \code{f121} \code{f13} \code{f14} \code{f15} \code{f16} \code{f17} \code{f18} \code{f19} \code{f2} \code{f20} \code{f21} \code{f22} \code{f23} \code{f24} \code{f25} \code{f26} \code{f27} \code{f28} \code{f29} \code{f3} \code{f30} \code{f31} \code{f32} \code{f33} \code{f34} \code{f35} \code{f36} \code{f37} \code{f38} \code{f39} \code{f4} \code{f40} \code{f41} \code{f42} \code{f43} \code{f44} \code{f45} \code{f46} \code{f47} \code{f48} \code{f49} \code{f5} \code{f50} \code{f51} \code{f52} \code{f53} \code{f54} \code{f55} \code{f56} \code{f57} \code{f58} \code{f59} \code{f6} \code{f60} \code{f61} \code{f62} \code{f63} \code{f64} \code{f65} \code{f66} \code{f67} \code{f68} \code{f69} \code{f7} \code{f70} \code{f71} \code{f72} \code{f73} \code{f74} \code{f75} \code{f76} \code{f77} \code{f78} \code{f79} \code{f8} \code{f80} \code{f81} \code{f82} \code{f83} \code{f84} \code{f85} \code{f86} \code{f87} \code{f88} \code{f89} \code{f9} \code{f90} \code{f91} \code{f92} \code{f93} \code{f94} \code{f95} \code{f96} \code{f97} \code{f98} \code{f99}}
    \item{\code{repli},}{a factor with levels: \code{r1} \code{r2}}
    \item{\code{dam},}{a factor with levels: \code{d1} \code{d10} \code{d11} \code{d2} \code{d3} \code{d4} \code{d5} \code{d6} \code{d7} \code{d8} \code{d9}}
    \item{\code{sire},}{a factor with levels: \code{s1} \code{s10} \code{s11} \code{s2} \code{s3} \code{s4} \code{s5} \code{s6} \code{s7} \code{s8} \code{s9}}
    \item{\code{tray},}{a factor with levels: \code{t1} \code{t10} \code{t11} \code{t12} \code{t13} \code{t14} \code{t15} \code{t16} \code{t2} \code{t3} \code{t4} \code{t5} \code{t6} \code{t7} \code{t8} \code{t9}}
    \item{\code{cell},}{a factor with levels: \code{1A} \code{1B} \code{1C} \code{1D} \code{2A} \code{2B} \code{2C} \code{2D} \code{3A} \code{3B} \code{3C} \code{3D} \code{4A} \code{4B} \code{4C} \code{4D}}
    \item{\code{alive},}{a numeric vector.}
    \item{\code{dead},}{a numeric vector.}
    \item{\code{egg_size},}{a numeric vector.}
  }
}
\details{
Also includes family identity, family replicate, incubator position (tray and cell),
and average female egg size (mm) information.
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
data(chinook_survival)
## Convert replicate-level recorded data to individual-level (binary) data
chinook_survival2<- buildBinary(dat=chinook_survival,copy=c(2:6,9),one="alive",zero="dead")

## Standard additive genetic, non-additive genetic, and maternal variance analysis
\dontrun{survival_mod1<- observGlmer(observ=chinook_survival2,dam="dam",sire="sire",
response="status",fam_link=binomial(logit))
survival_mod1}
}
\keyword{datasets}
