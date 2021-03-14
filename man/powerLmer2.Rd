\name{powerLmer2}
\alias{powerLmer2}
\title{Power analysis for normal data 2}
\description{
Extracts the power values of dam, sire, and dam by sire variance components from a
linear mixed-effect model using the \emph{lmer} function of the \emph{lme4} package.
Options to include one random position and/or one random block effect(s).
}
\usage{
powerLmer2(varcomp, nval, alpha = 0.05, nsim = 100, position = NULL, block = NULL,
ml = F)
}
\arguments{
  \item{varcomp}{Vector of known dam, sire, dam by sire, residual, and position and/or
  block variance components, i.e. c(dam, sire, dam x sire, residual, position/block). If there is a
  position and a block c(..., residual, position, block).}
  \item{nval}{Vector of known dam, sire, offspring per family, and offspring per position or number of blocks
  sample sizes, i.e. c(dam, sire, offspring, position/block). If there is a position and a
  block c(..., offspring, position, block).}
  \item{alpha}{Statistical significance value. Default is 0.05.}
  \item{nsim}{Number of simulations. Default is 100.}
  \item{position}{Optional number of positions.}
  \item{block}{Optional vector of dams and sires per block, e.g. c(2,2).}
  \item{ml}{Default is FALSE for restricted maximum likelihood. Change to TRUE for maximum likelihood.}
}
\details{
Extracts the dam, sire, dam, dam by sire, and position and/or block power values.
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
\seealso{ \code{\link{powerLmer}}, \code{\link{powerLmer3}} }
\examples{
#100 simulations
#position only, e.g. 8 tanks
\dontrun{powerLmer2(varcomp=c(0.2030,0,0.1798,0.5499,0.1077),nval=c(8,8,20,40),position=8)}
#block only, e.g. four 2 x 2
\dontrun{powerLmer2(varcomp=c(0.2030,0,0.1798,0.5499,0.1077),nval=c(8,8,20,4),block=c(2,2))}
#position and block
\dontrun{powerLmer2(varcomp=c(0.2030,0,0.1798,0.5499,0.1077,0.1077),nval=c(8,8,20,40,4),
position=8,block=c(2,2))}
}