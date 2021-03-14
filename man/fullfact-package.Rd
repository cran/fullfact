\name{fullfact-package}
\alias{fullfact-package}
\alias{fullfact}
\docType{package}
\title{
\packageTitle{fullfact}
}
\description{

Full factorial breeding designs are useful for quantifying the amount of additive
genetic, nonadditive genetic, and maternal variance that explain phenotypic
traits. Such variance estimates are important for examining evolutionary potential.
Traditionally, full factorial mating designs have been analyzed using a two-
way analysis of variance, which may produce negative variance values and is
not suited for unbalanced designs. Mixed-effects models do not produce negative
variance values and are suited for unbalanced designs. However, extracting
the variance components, calculating significance values, and estimating confidence
intervals and/or power values for the components are not straightforward
using traditional analytic methods.

In this package we address these issues and facilitate the analysis of full factorial mating
designs with mixed-effects models. The observed data functions extract the variance explained
by random and fixed effects and provide their significance. We then calculate
the additive genetic, nonadditive genetic, and maternal variance components
explaining the phenotype. In particular, we integrate nonnormal error structures for estimating
these components for nonnormal data types. The resampled data functions are used to produce
bootstrap confidence intervals, which can then be plotted using a simple function.
This package will facilitate the analyses of full factorial mating designs in R, especially
for the analysis of binary, proportion, and/or count data types and for the ability to incorporate
additional random and fixed effects and power analyses.

There are now six vignettes containing detailed examples: browseVignettes(package="fullfact"). 

The paper associated with the package including worked examples is:
Houde ALS, Pitcher TE. 2016. fullfact: an R package for the analysis of genetic and maternal variance components
from full factorial mating designs. Ecology and evolution 6 (6), 1656-1665. doi: 10.1002/ece3.1943.

}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{fullfact}
\packageIndices{fullfact}
}
\author{
\packageAuthor{fullfact}

Maintainer: \packageMaintainer{fullfact}
}
\references{

Traditional full factorial breeding design analysis:

Lynch M, Walsh B. 1998. Genetics and Analysis of Quantitative Traits. Sinauer Associates, Massachusetts.

Residual variance component values for generalized linear mixed-effects models:

Nakagawa S, Schielzeth H. 2010. Repeatability for Gaussian and non-Gaussian data: a practical guide for biologists.
Biological Reviews 85(4): 935-956. DOI: 10.1111/j.1469-185X.2010.00141.x

Fixed effect variance component values for mixed-effects models:

Nakagawa S, Schielzeth H. 2013. A general and simple method for obtaining R2 from generalized linear mixed-effects models.
Methods in Ecology and Evolution 4(2): 133-142. DOI: 10.1111/j.2041-210x.2012.00261.x

Confidence intervals (bootstrap resampling, bias and acceleration correction, jackknife resampling):

Efron B, Tibshirani R. 1993. An introduction to the Bootstrap. Chapman and Hall, New York.

Martin, H., Westad, F. & Martens, H. (2004). Imporved Jackknife Variance Estimates of Bilinear
Model Parameters. COMPSTAT 2004 -- Proceedings in Computational Statistics 16th Symposium Held
in Prague, Czech Republic, 2004 (ed J. Antoch), pp. 261-275. Physica-Verlag HD, Heidelberg.

Data sources:

Pitcher TE, Neff BD. 2007. Genetic quality and offspring performance in Chinook salmon: implications for supportive breeding.
Conservation Genetics 8(3):607-616. DOI: 10.1007/s10592-006-9204-z
}
\keyword{ package }

\examples{
data(chinook_length) #Chinook salmon offspring length

## Standard additive genetic, non-additive genetic, and maternal variance analysis

length_mod1<- observLmer(observ=chinook_length,dam="dam",sire="sire",response="length")
length_mod1

## Confidence intervals

##Bootstrap resampling of data: replicates within family
\dontrun{resampRepli(dat=chinook_length,copy=c(3:8),family="family",replicate="repli",
iter=1000)}
#saves the files in working directory: one for each replicate and
#one final (combined) file "resamp_datR.csv"

##Import file
#length_datR<- read.csv("resamp_datR.csv")
data(chinook_resampL) #same as length_datR, 5 iterations

##Models for the resampled data: standard analysis
\dontrun{length_rcomp<- resampLmer(resamp=length_datR,dam="dam",sire="sire",
response="length",start=1,end=1000)}

## 1. Uncorrected Bootstrap 95% confidence interval

#ciMANA(comp=length_rcomp)
data(chinook_bootL)  #similar to length_rcomp, but 1,000 models
ciMANA(comp=chinook_bootL)

## 2. Bias and accelerated corrected Bootstrap 95% confidence interval

##Jackknife resampling of data, delete-one: for acceleration estimate
\dontrun{length_jack<- JackLmer(observ=chinook_length,dam="dam",sire="sire",
response="length")}

#ciMANA(comp=length_rcomp,bias=c(0,0.7192,0.2030),accel=length_jack)
data(chinook_jackL)  #similar to length_jack, but all observations
ciMANA(comp=chinook_bootL,bias=c(0,0.7192,0.2030),accel=chinook_jackL)

##3. Jackknife 95% confidence interval

#ciJack(comp=length_jack,full=c(0,0.7192,0.2030,1.0404))
ciJack(comp=chinook_jackL,full=c(0,0.7192,0.2030,1.0404))
}