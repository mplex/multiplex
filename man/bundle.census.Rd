\name{bundle.census}
\alias{bundle.census}
\title{
Bundle Census
}
\description{
A function to perform the Bundle Census in a given multiple network.
}
\usage{
bundle.census(x, loops = FALSE)
}
\arguments{
  \item{x}{
   an array; usually with three dimensions of stacked matrices where the multiple relations are placed.
}
  \item{loops}{
   (logical) whether or not the loops should be considered
}
}
\details{
This function calculates the number of occurrences for each bundle class pattern in the multiple network. A bundle is a special type of pattern made of relations at different levels that is binding a pair of nodes or actors. Depending on the direction and occurrence of each possible tie, then it is possible to count with seven dyadic configuration classes, which are included in the census.
}
\value{
A table with the occurrences in the distinct bundle class patterns. The first column in the output gives the number of bundles in the network excluding the null pattern, and then the totals for each bundle class pattern are specified in the following columns. If loops were considered, these are at the end of the table.

More detailed information about the bundle class occurrences is given by the function \code{\link{bundles}}.

}
\references{
Ostoic, J. A. R. `Dyadic Patterns in Multiple Networks,' \emph{Advances in Social Networks Analysis and Mining, International Conference on}, 475-481. 2011.
}
\author{
Antonio Rivero Ostoic
}
\note{
Neither loops or null dyads are regarded as properly bundle classes.
}

\seealso{
\code{\link{bundles}}, \code{\link{summaryBundles}}
}
\examples{
## Create the data: two binary relations among three elements
arr <- round( replace( array(runif(18), c(3,3,2)), array(runif(18),
       c(3,3,2))>.8, 3 ) )

## Calculate the Bundle Census
bundle.census(arr)

}
\keyword{ data }

