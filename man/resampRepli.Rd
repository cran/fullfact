\name{resampRepli}
\alias{resampRepli}
\title{Bootstrap resample within replicates}
\description{
Bootstrap resample observations grouped by replicate identities within family identities for a specified number
of iterations to create a resampled data set.
}
\usage{
resampRepli(dat, copy, family, replicate, iter)
}
\arguments{
  \item{dat}{Data frame observed data to resample.}
  \item{copy}{Column numbers to copy.}
  \item{family}{Column name containing family identity information.}
  \item{replicate}{Column name containing replicate identity information.}
  \item{iter}{Number of iterations for resampling.}
}
\details{
The resampled data can be used for producing bootstrap confidence intervals.
}
\value{
Because of the large file sizes that can be produced, the resampling of each replicate Y per family X is
saved separately as a common separated (X_Y_resampR.csv) file in the working directory. These files
are merged to create the final resampled data set (resamp_datR.csv).
}
\seealso{ \code{\link{resampFamily}} }
\examples{
data(chinook_length) #Chinook salmon offspring length
#resampRepli(dat=chinook_length,copy=c(3:8),family="family",replicate="repli",iter=1000)
#example with a couple iterations
#resampRepli(dat=chinook_length,copy=c(3:8),family="family",replicate="repli",iter=2)
}