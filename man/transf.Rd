\name{transf}
\alias{transf}
\title{
Transform Data from/to Matrix/List Formats
}
\description{
Function to transform data from/to matrix/list formats representing a network.
}
\usage{
transf(x, type = c("tolist", "toarray"), lb2lb = FALSE, 
       labels = NULL, ord, prsep)
}
\arguments{
  \item{x}{
   a matrix or a list of pair relations
  }
  \item{type}{
   whether the transformation is from a matrix to a list of pair relations, or from a list of pair relations to an array format
  }
  \item{lb2lb}{
   (logical) whether the transformation is label-to-label
  }
  \item{labels}{
   (optional) the labels in the transformation
  }
  \item{ord}{
   (optional) the order of the resulted structure (`\code{toarray}' option, otherwise ignored)
  }
  \item{prsep}{
   (optional) the pair separator for the pairwise relations
  }
}
\details{
`\code{tolist}' is the option to transform a matrix to a list of pair elements. In case that the \code{lb2lb} is enabled in this type of transformation, then \code{labels} must be provided, whereas the pair separator is optional. On the other hand `\code{toarray}' will produce a matrix from a list of pair elements, and in this case is advisable to specify the order of the structure.

For high dimensional arrays use the \code{\link{rel.sys}} function in order to get the list of pair relations of the entire structure.
}
\value{
Depending on the input data, the result is either a list of pair relations or a matrix of relations.
}
%\references{
%
%}
\author{
Antonio Rivero Ostoic
}
\note{
Data frames are also accepted for the `\code{tolist}' option, but in case that this information is given as a list of pair relations the output will be a square matrix.
}
\seealso{
\code{\link{read.srt}}, \code{\link{bundles}}, \code{\link{reduc}}, \code{\link{rel.sys}}
}
\examples{
## scan the multiplication table data
s <- matrix(data=c(1, 1, 1, 3, 3, 3, 3, 3, 3), nrow=3, ncol=3, byrow=TRUE)

## transform the matrix to a list format
transf(s, lb2lb = TRUE, labels = c('n','m','ñ'))
}
\keyword{ manip }
\keyword{ array }

