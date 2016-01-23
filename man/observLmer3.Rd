\name{observLmer3}
\alias{observLmer3}
\title{Variance components for normal data 3}
\description{
Extracts additive genetic, non-additive genetic, and maternal variance components
from a linear mixed-effect model using the \emph{lmer} function of the \emph{lme4} package.
Model random effects are dam, sire, dam by sire, and any additional fixed and/or random effects.
}
\usage{
observLmer3(observ, dam, sire, response, remain, ml = F, iter = 1000)
}
\arguments{
  \item{observ}{Data frame of observed data.}
  \item{dam}{Column name containing dam(female) parent identity information.}
  \item{sire}{Column name containing sire(male) parent identity information.}
  \item{response}{Column name containing the offspring (response) phenotype values.}
  \item{remain}{Remaining formula using \emph{lme4} package format.}
  \item{ml}{Default is FALSE for restricted maximum likelihood. Change to TRUE for maximum likelihood.}
  \item{iter}{Number of iterations for computing the parametric bootstrap significance value
  for any fixed effects.}
}
\details{
Extracts the dam, sire, dam, dam by sire, and residual variance components.
Extracts any additional fixed effect and random effect variance components. The fixed-effect variance
component is as a single group using the method described by Nakagawa and Schielzeth (2013).
Calculates the total variance component. Calculates the additive genetic, non-additive genetic, and
maternal variance components (see Lynch and Walsh 1998, p. 603).
Significance values for the random effects are determined using likelihood ratio tests (Bolker et al. 2009).
Significance values for any fixed effects are determined using likelihood ratio tests and a parametric
bootstrap method (Bolker et al. 2009) from the \emph{mixed} function of the \emph{afex} package.
}
\value{
A list object containing the raw variance components, the variance components as a percentage
of the total variance component. Contains the difference in AIC and BIC, likelihood ratio
test Chi-square and p-value for both random and fixed effects. Also contains the parametric
bootstrap p-value for fixed effects.
}
\references{
Bolker BM, Brooks ME, Clark CJ, Geange SW, Poulsen JR, Stevens MHH, White J-SS. 2009.
Generalized linear mixed models: a practical guide for ecology and evolution.
Trends in Ecology and Evolution 24(3): 127-135. DOI: 10.1016/j.tree.2008.10.008

Lynch M, Walsh B. 1998. Genetics and Analysis of Quantitative Traits. Sinauer Associates, Massachusetts.

Nakagawa S, Schielzeth H. 2013. A general and simple method for obtaining R2 from generalized linear
mixed-effects models. Methods in Ecology and Evolution 4(2): 133-142.
DOI: 10.1111/j.2041-210x.2012.00261.x
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
\seealso{ \code{\link{observLmer}}, \code{\link{observLmer2}} }

\examples{
data(chinook_length) #Chinook salmon offspring length
#just a few iterations for the p-value of fixed effect
length_mod3<- observLmer3(observ=chinook_length,dam="dam",sire="sire",response="length",
remain="egg_size + (1|tray)",iter=5)
length_mod3
}