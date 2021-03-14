\name{powerGlmer3}
\alias{powerGlmer3}
\title{Power analysis for non-normal data 3}
\description{
Extracts the power values of dam, sire, and dam by sire variance components from a
generalized linear mixed-effect model using the \emph{glmer} function of the
\emph{lme4} package. Model can include additional fixed and/or random effects.
}
\usage{
powerGlmer3(var_rand, n_rand, design, remain, fam_link, var_fix = NULL, n_fix = NULL,
 alpha = 0.05, nsim = 100, poisLog = NULL, ftest = "LR", iter = NULL)
}
\arguments{
  \item{var_rand}{Vector of known dam, sire, dam by sire, and remaining random
  variance components, i.e. c(dam,sire, dam by sire, rand1, rand2, etc.).}
  \item{n_rand}{Vector of known dam, sire, family, and remaining random sample sizes,
  i.e. c(dam, sire, family, rand1, rand2,etc.).}
  \item{design}{A data frame of the experimental design, using only integers. First
  three columns must contain and be named "dam", "sire", "family". Remaining columns are
  the random effects followed by the fixed effects. Continuous fixed effects are a column
  containing the values 1:nrow(design).}
  \item{remain}{Remaining formula using \emph{lme4} package format. Must be random effects
  followed by fixed effects. No interactions or random slopes; formulate as intercepts in design.}
  \item{fam_link}{The family and link in family(link) format. Supported options are binomial(link="logit"), binomial(link="probit"), poisson(link="log"), and poisson(link="sqrt").}
  \item{var_fix}{Vector of known fixed variance components, i.e. c(fix1, fix2, etc.).
  Continous fixed random values are sorted to match column values.}
  \item{n_fix}{Vector of known fixed sample sizes, i.e. c(fix1, fix2, etc.). Continuous fixed
  effects must have a sample size of 1.}
  \item{alpha}{Statistical significance value. Default is 0.05.}
  \item{nsim}{Number of simulations. Default is 100.}
  \item{poisLog}{The residual variance component value if using poisson(link="log").}
  \item{ftest}{Default is "LR" for likelihood ratio test for fixed effects. Option "PB" is for parametric
  bootstrap.}
  \item{iter}{Number of iterations for computing the parametric bootstrap significance value
  for any fixed effects.}
}
\details{
Extracts the dam, sire, dam, dam by sire, and any remaining random and fixed effects power values.
The residual variance component for the \emph{fam_links} are described by Nakagawa and Schielzeth (2010, 2013).
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
\seealso{ \code{\link{powerGlmer}}, \code{\link{powerGlmer2}} }
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
\dontrun{powerGlmer3(var_rand=c(1,0.15,0.11,0.5,0.3),n_rand=c(8,8,16,16,4),
fam_link=binomial(link="logit"),var_fix=0.1,n_fix=1,design=desn,
remain="(1|position)+(1|block)+egg_size") }
}