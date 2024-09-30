\name{bundle.census}
\alias{bundle.census}
\title{
Bundle Census
}
\description{
A function to perform the bundle census in multiple networks.
}
\usage{
bundle.census(x, loops = FALSE)
}
\arguments{
  \item{x}{
   an array; usually with three dimensions of stacked matrices where the multiple relations are placed
}
  \item{loops}{
   (logical) whether or not the loops should be considered
}
}
\details{
This function calculates the number of occurrences for each bundle class pattern in multiple networks. 
A bundle is a particular type of pattern made of relations at different levels that is binding a pair of nodes or actors. 
Depending on the direction and occurrence of each possible tie, then it is possible to count with seven dyadic configuration classes in the census. 
}
\value{
A table with the occurrences in the distinct bundle class patterns. 
The first column in the output gives the number of bundles in the network, excluding the null pattern, and then the totals 
for each bundle class pattern are specified in the following columns. 
The last column of the table hosts loops in case these are activated in the input. 
}
\references{
Ostoic, J. A. R. \dQuote{Dyadic Patterns in Multiple Networks,} \emph{Advances in Social Networks Analysis and Mining, International Conference on}, 475-481. 2011.
}
\author{
Antonio Rivero Ostoic
}
\note{
Functions \code{\link{bundles}} and \code{\link{summaryBundles}} provide bundle class occurrences in the network with a more detailed information. 
}
\seealso{
\code{\link{bundles}}, \code{\link{summaryBundles}}
}
\examples{
# create the data: two binary relations among three elements
arr <- round( replace( array(runif(18), c(3,3,2)), array(runif(18),
       c(3,3,2))>.8, 3 ) )

# compute the bundle census
bundle.census(arr)

}
\keyword{data}

