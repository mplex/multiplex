\name{summaryBundles}
\alias{summaryBundles}
\title{
Summary of Bundle Classes
}
\description{
Pretty printing of the bundle class patterns results.
}
\usage{
summaryBundles(x, file = NULL, latex = FALSE, byties)
}
\arguments{
  \item{x}{
   an object of the `\code{Rel.Bundles}' class
}
  \item{file}{
   (optional) the path where the output file is to be placed
}
  \item{latex}{
   (logical) whether the output should be in latex format or not
}
  \item{byties}{
   (optional and logical) expand tie patterns and collapse tie labels?
}
%  \item{sep}{
%   (optional) the pair separator for the pairwise relations
%}
}
\details{
This function prints the bundle census patterns existing in the network with an option to export such information in a friendly format. 
The dyadic bundle patterns are provided by the function \code{\link{bundles}}; however, the outcome of this function provides a list of pair lists for 
each bundle with the involved types of relations and nodes in the network. 
This form for presentation, although is convenient for further computation, it is not always easy to read for the human eye. 
The pair separator used to print the bundle occurrences is taken from the output of the \code{\link{bundles}} function.


If \code{latex} is set to \code{TRUE}, then the path file is activated to obtain a \code{tex} file with the different bundle class patterns. 
Finally, the optional argument \code{byties} provide more precise information about the patterned ties disregarding the relational content.
}
\value{
The distinct bundle class patterns with a user friendly format.
}
\references{
Ostoic, J. A. R. `Dyadic Patterns in Multiple Networks,' \emph{Advances in Social Networks Analysis and Mining, International Conference on}, 475-481. 2011.
}
\author{
Antonio Rivero Ostoic
}
\note{
If a file with the same name already exists in the pointed directory, then this file will be overwritten.
}

\seealso{
\code{\link{bundles}}, \code{\link{bundle.census}}
}
\examples{
## Create the data: 2 binary relations among 3 elements
arr <- round( replace( array( runif(18), c(3 ,3, 2) ), array( runif(18),
c(3, 3, 2) ) > .8, 3 ) )

## Establish the different bundles
bd <- bundles(arr)

## Print the different relational bundles
summaryBundles(bd)

}
\keyword{ print }
\keyword{ IO }
