\name{buildMulti}
\alias{buildMulti}
\title{Convert to a multinomial frame}
\description{
Assign multiple numbers to multiple columns containing the number of offspring.
Copy information by the number of times equal to the number of offspring.
}
\usage{
buildMulti(dat, copy, multi)
}
\arguments{
  \item{dat}{Data frame to convert.}
  \item{copy}{Column numbers to copy.}
  \item{multi}{A list containing the numbers to assign and matching column names,
  e.g. list(c(2,0,1),c("two","zero","one")).
}
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
\seealso{\code{\link{buildBinary}}}
\examples{
data(chinook_survival)
chinook_survival$total<- chinook_survival$alive + chinook_survival$dead #create total column
chinook_survival3<- buildMulti(dat=chinook_survival,copy=c(1:6,9),multi=list(c(2,1,0),
c("total","alive","dead")))
}