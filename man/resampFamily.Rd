\name{resampFamily}
\alias{resampFamily}
\title{Bootstrap resample within families}
\description{
Bootstrap resample observations grouped by family identities for a specified number of iterations to create
a resampled data set.
}
\usage{
resampFamily(dat, copy, family, iter)
}
\arguments{
  \item{dat}{Data frame observed data to resample.}
  \item{copy}{Column numbers to copy.}
  \item{family}{Column name containing family identity information.}
  \item{iter}{Number of iterations for resampling.}
}
\details{
The resampled data can be used for producing bootstrap confidence intervals.
}
\value{
Because of the large file sizes that can be produced, the resampling of each family X is saved separately
as a common separated (X_resampF.csv) file in the working directory. These files are merged to create the
final resampled data set (resamp_datF.csv).
}
\seealso{ \code{\link{resampRepli}} }
\examples{
data(chinook_length) #Chinook salmon offspring length
#resampFamily(dat=chinook_length,copy=c(3:8),family="family",iter=1000)
#resampFamily(dat=chinook_length,copy=c(3:8),family="family",iter=2)
#example with a couple iterations
}