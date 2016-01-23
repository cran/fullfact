\name{buildBinary}
\alias{buildBinary}
\title{Convert to a binary data frame}
\description{
Assign a binary number (i.e. '0' or '1') to two columns containing the number of offspring.
Copy information by the number of times equal to the number of offspring.
}
\usage{buildBinary(dat, copy, one, zero)}
\arguments{
  \item{dat}{Data frame to convert.}
  \item{copy}{Column numbers to copy.}
  \item{one}{Column name of counts to assign a '1' value.}
  \item{zero}{Column name of counts to assign a '0' value.}
}
\details{
Replicate-level data should be converted to the individual-level to not underestimate phenotypic variance,
which can influence genetic and maternal estimates (see Puurtinen et al. 2009).
}
\value{
A converted data frame with a number of row matching the total number of individuals.
}
\references{
Puurtinen M, Ketola T, Kotiaho JS. 2009. The good-genes and compatible-genes benefits of mate choice.
The American Naturalist 174(5): 741-752. DOI: 10.1086/606024
}
\seealso{\code{\link{buildMulti}}}
\examples{
data(chinook_survival)
chinook_survival2<- buildBinary(dat=chinook_survival,copy=c(1:6,9),one="alive",zero="dead")
}