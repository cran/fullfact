\name{powerLmer3}
\alias{powerLmer3}
\title{Power analysis for normal data 3}
\description{
Extracts the power values of dam, sire, and dam by sire variance components from a
linear mixed-effect model using the \emph{lmer} function of the \emph{lme4} package.
Model can include additional fixed and/or random effects.
}
\usage{
powerLmer3(var_rand, n_rand, design, remain, var_fix = NULL, n_fix = NULL,
alpha = 0.05, nsim = 100, ml = F, ftest = "LR", iter = NULL)
}
\arguments{
  \item{var_rand}{Vector of known dam, sire, dam by sire, residual, and remaining random
  variance components, i.e. c(dam, sire, dam x sire, residual, rand1, rand2, etc.).}
  \item{n_rand}{Vector of known dam, sire, family, and remaining random sample sizes,
  i.e. c(dam, sire, family, rand1, rand2, etc.).}
  \item{design}{A data frame of the experimental design, using only integers. First
  three columns must contain and be named "dam", "sire", "family". Remaining columns are
  the random effects followed by the fixed effects. Continuous fixed effects are a column
  containing the values 1:nrow(design).}
  \item{remain}{Remaining formula using \emph{lme4} package format. Must be random effects
  followed by fixed effects. No interactions or random slopes; formulate as intercepts in design.}
  \item{var_fix}{Vector of known fixed variance components, i.e. c(fix1, fix2, etc.).
  Continous fixed random values are sorted to match column values.}
  \item{n_fix}{Vector of known fixed sample sizes, i.e. c(fix1, fix2, etc.). Continuous fixed
  effects must have a sample size of 1.}
  \item{alpha}{Statistical significance value. Default is 0.05.}
  \item{nsim}{Number of simulations. Default is 100.}
  \item{ml}{Default is FALSE for restricted maximum likelihood. Change to TRUE for maximum likelihood.}
  \item{ftest}{Default is "LR" for likelihood ratio test for fixed effects. Option "PB" is for parametric
  bootstrap.}
  \item{iter}{Number of iterations for computing the parametric bootstrap significance value
  for any fixed effects.}
}
\details{
Extracts the dam, sire, dam, dam by sire, and any remaining random and fixed effects power values.
Power values are calculated by stochastically simulation data and then calculating the proportion of
significance values less than \emph{alpha} for each component (Bolker 2008).
Significance values for the random effects are determined using likelihood ratio tests (Bolker et al. 2009).
Significance values for any fixed effects are determined using likelihood ratio tests or parametric
bootstrap method (Bolker et al. 2009) from the \emph{mixed} function of the \emph{afex} package.
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
\seealso{ \code{\link{powerLmer}}, \code{\link{powerLmer2}} }
\examples{
##design object: 2 remaining random effects and 1 continous fixed effect
block=c(2,2); blocN=4; position=16; posN=20; offN=20
dam0<- stack(as.data.frame(matrix(1:(block[1]*blocN),ncol=blocN,nrow=block[1])))
sire0<- stack(as.data.frame(matrix(1:(block[2]*blocN),ncol=blocN,nrow=block[2])))
observ0<- merge(dam0,sire0, by="ind")
levels(observ0[,1])<- 1:blocN; colnames(observ0)<- c("block","dam","sire")
observ0$family<- 1:nrow(observ0)  #add family
#expand for offspring, observ0 x offN
observ1<- do.call("rbind", replicate(offN,observ0,simplify=FALSE))
observ1$position<- rep(1:position,each=posN)
observ1$position<- sample(observ1$position,nrow(observ1)) #shuffle
desn<- observ1[,c(2,3,4,5,1)];rm(observ0,observ1) #dam,sire,family,position,block
desn$egg_size<- 1:nrow(desn)

#100 simulations
\dontrun{powerLmer3(var_rand=c(0.19,0.03,0.02,0.51,0.1,0.05),n_rand=c(8,8,16,16,4),
var_fix=0.1,n_fix=1,design=desn,remain="(1|position)+ (1|block)+ egg_size") }
}