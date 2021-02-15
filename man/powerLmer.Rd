\name{powerLmer}
\alias{powerLmer}
\title{Power analysis for normal data}
\description{
Extracts the power values of dam, sire, and dam by sire variance components from a
linear mixed-effect model using the \emph{lmer} function of the \emph{lme4} package.
}
\usage{
powerLmer(varcomp, nval, alpha = 0.05, nsim = 100, ml = F)
}
\arguments{
  \item{varcomp}{Vector of known dam, sire, dam by sire, and residual variance components, i.e. c(dam,sire,ds,res).}
  \item{nval}{Vector of known dam, sire, and offspring per family sample sizes, i.e. c(dam,sire,
  offspring).}
  \item{alpha}{Statistical significance value. Default is 0.05.}
  \item{nsim}{Number of simulations. Default is 100.}
  \item{ml}{Default is FALSE for restricted maximum likelihood. Change to TRUE for maximum likelihood.}
}
\details{
Extracts the dam, sire, dam, and dam by sire power values.
Power values are calculated by stochastically simulation data and then calculating the proportion of
significance values less than \emph{alpha} for each component (Bolker 2008).
Significance values for the random effects are determined using likelihood ratio tests (Bolker et al. 2009).
}
\value{
Prints a data frame with the sample sizes, variance component inputs, variance component outputs, and power values.
}
\references{
Bolker BM. 2008. Ecological models and data in R. Princeton University Press, New Jersey.

Bolker BM, Brooks ME, Clark CJ, Geange SW, Poulsen JR, Stevens MHH, White J-SS. 2009.
Generalized linear mixed models: a practical guide for ecology and evolution.
Trends in Ecology and Evolution 24(3): 127-135. DOI: 10.1016/j.tree.2008.10.008

Lynch M, Walsh B. 1998. Genetics and Analysis of Quantitative Traits. Sinauer Associates, Massachusetts.
}
\note{
Maximum likelihood (ML) estimates the parameters that maximize the likelihood of the observed data
and has the advantage of using all the data and accounting for non-independence (Lynch and Walsh 1998,
p. 779; Bolker et al. 2009). On the other hand, ML has the disadvantage of assuming that all fixed
effects are known without error, producing a downward bias in the estimation of the residual variance
component. This bias can be large if there are lots of fixed effects, especially if sample sizes are
small. Restricted maximum likelihood (REML) has the advantage of not assuming the fixed effects are
known and averages over the uncertainty, so there can be less bias in the estimation of the residual
variance component. However, REML only maximizes a portion of the likelihood to estimate the effect
parameters, but is the preferred method for analyzing large data sets with complex structure.
}
\seealso{ \code{\link{powerLmer2}}, \code{\link{powerLmer3}} }
\examples{
#100 simulations
#powerLmer(varcomp=c(0.19,0.03,0.02,0.76),nval=c(10,10,20))
#
#5 simulations
powerLmer(varcomp=c(0.19,0.03,0.02,0.76),nval=c(10,10,20),nsim=5)
}