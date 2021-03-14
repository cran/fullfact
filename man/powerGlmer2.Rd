\name{powerGlmer2}
\alias{powerGlmer2}
\title{Power analysis for non-normal data 2}
\description{
Extracts the power values of dam, sire, and dam by sire variance components from a
generalized linear mixed-effect model using the \emph{glmer} function of the \emph{lme4}
package. Options to include one random position and/or one random block effect(s).
}
\usage{
powerGlmer2(varcomp, nval, fam_link, alpha = 0.05, nsim = 100, position = NULL,
block = NULL, poisLog = NULL)
}
\arguments{
  \item{varcomp}{Vector of known dam, sire, dam by sire, and position and/or
  block variance components, i.e. c(dam, sire, dam x sire, position/block). If there is a
  position and a block c(..., dam x sire, position, block).}
  \item{nval}{Vector of known dam, sire, offspring per family, and offspring per position or number of blocks
  sample sizes, i.e. c(dam, sire, offspring, position/block). If there is a position and a
  block c(..., offspring, position, block).}
  \item{fam_link}{The family and link in family(link) format. Supported options are binomial(link="logit"), binomial(link="probit"), poisson(link="log"), and poisson(link="sqrt").}
  \item{alpha}{Statistical significance value. Default is 0.05.}
  \item{nsim}{Number of simulations. Default is 100.}
  \item{position}{Optional number of positions.}
  \item{block}{Optional vector of dams and sires per block, e.g. c(2,2).}
  \item{poisLog}{The residual variance component value if using poisson(link="log").}
}
\details{
Extracts the dam, sire, dam, dam by sire, and position and/or block power values.
The residual variance component for the \emph{fam_links} are described by Nakagawa and Schielzeth (2010, 2013).
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

Nakagawa S, Schielzeth H. 2010. Repeatability for Gaussian and non-Gaussian data: a practical guide for biologists.
Biological Reviews 85(4): 935-956. DOI: 10.1111/j.1469-185X.2010.00141.x

Nakagawa S, Schielzeth H. 2013. A general and simple method for obtaining R2 from generalized linear mixed-effects models.
Methods in Ecology and Evolution 4(2): 133-142. DOI: 10.1111/j.2041-210x.2012.00261.x
}
\note{
The Laplace approximation is used because there were fewer disadvantages relative to penalized
quasi-likelihood and Gauss-Hermite quadrature parameter estimation (Bolker et al. 2009).
That is, penalized quasi-likelihood is not recommended for count responses with means less than 5 and
binary responses with less than 5 successes per group. Gauss-Hermite quadrature is not recommended for
more than two or three random effects because of the rapidly declining analytical speed with the
increasing number of random effects.
}
\seealso{ \code{\link{powerGlmer}}, \code{\link{powerGlmer3}} }
\examples{
#100 simulations
\dontrun{powerGlmer2(varcomp=c(0.7880,0.1667,0.1671,0.0037),nval=c(11,11,300,3300),
position=11,fam_link=binomial(link="logit")) }
}