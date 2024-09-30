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
   an object of the \sQuote{\code{Rel.Bundles}} class
}
  \item{file}{
   (optional) a path where the output file is to be placed
}
  \item{latex}{
   (logical) whether or not the output should be in latex format
}
  \item{byties}{
   (optional and logical) expand tie patterns and collapse tie labels?
}
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
Ostoic, J. A. R. \dQuote{Dyadic Patterns in Multiple Networks,} \emph{Advances in Social Networks Analysis and Mining, International Conference on}, 475-481. 2011.
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
# create the data: two binary relations among three elements
arr <- round( replace( array( runif(18), c(3 ,3, 2) ), array( runif(18),
c(3, 3, 2) ) > .8, 3 ) )

# print different relational bundles in 'arr'
arr |> 
  bundles() |> 
  summaryBundles()
}
\keyword{print }
\keyword{IO}
