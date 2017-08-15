\name{prev}
\alias{prev}
\title{
Preview of the Semigroup Construction
}
\description{
A function to preview the partial right multiplication table of the semigroup to assess the size of the complete semigroup.
}
\usage{
prev(x)
}
\arguments{
  \item{x}{
   an array; usually with three dimensions of stacked matrices where the multiple relations are placed.
}
}
\details{
When the input data is large, i.e. having a dozen or more elements and/or more than five dimensions, it is recommended to perform this function before the semigroup construction in order to get the partial right multiplication table. 

That is because the amount of undefined data in such table gives an idea of how much time may take to get the complete semigroup. However the performance depends mainly on whether the generator matrices are sparse and/or have a relative large number of elements for a semigroup construction of course.
}
\value{
  \item{'2stpT'}{
  a partial right multiplication table at two-step.
}
  \item{'PcU2stpT'}{
  the proportion of undefined elements at two-step.
}
  \item{ordr}{
  the dimension of the right multiplication table so far.
}
  \item{Note}{
  a conditional warning message.
}
}
%\references{
%No hay...
%}
\author{
Antonio Rivero Ostoic
}
\note{
The warning message is given only if the percentage of undefined elements and the dimension of the input data are relative high. The semigroup construction can however still take long time without such message; cf. `Details' for this. 
}
\seealso{
\code{\link{semigroup}}, \code{\link{edgeT}}.
}
\examples{
## Create the data: 2 binary relations among 3 elements
arr <- round( replace( array(runif(18), c(3,3,2)), array(runif(18),
       c(3,3,2))>.5, 1 ) )

## preview it
prev(arr)

}
\keyword{ algebra }
\keyword{ math }
%