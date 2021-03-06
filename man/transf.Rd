\name{transf}
\alias{transf}
\title{
Transform Data from/to Matrix/List Formats
}
\description{
Function to transform data from/to matrix/list formats representing a network.
}
\usage{
transf(x, type = c("toarray", "tolist", "toarray2"), lbs = NULL, lb2lb, 
    sep, ord, sort, sym, add, adc)
}
\arguments{
  \item{x}{
   an array or a list of pair relations
  }
  \item{type}{
   whether the transformation is from a list of pair relations to an array format, or from a matrix to a list of pair relations, or else from a list of pair relations to a square array 
  }
  \item{lbs}{
   (optional) the labels in the transformation (disabled for `\code{toarray}')
  }
  \item{lb2lb}{
   (optional and logical) whether the transformation is label-to-label. Default TRUE for "toarray" and FALSE for "tolist"
  }
  \item{sep}{
   (optional) the pair separator for the pairwise relations
  }
  \item{ord}{
   (optional) the order of the resulted structure (`\code{toarray}' option, otherwise ignored)
  }
  \item{sort}{
   (optional and logical) sort the arrays in the output?
  }
  \item{sym}{
   (optional and logical) symmetrize the arrays? (`\code{toarray}' option, otherwise ignored)
  }
  \item{add}{
   (optional) add elements in the array's `domain'
  }
  \item{adc}{
   (optional) add elements in the array's `codomain'
  }
}
\details{
`\code{tolist}' is the option to transform a matrix or an array to a list of pair elements. In case that the \code{lb2lb} is enabled in this type of transformation, then \code{lbs} must be provided, whereas the pair separator is optional. On the other hand, `\code{toarray}' will produce a matrix from a list of pair elements, and in this case is advisable to specify the order of the structure. Three-dimensional structures are now supported.

Data frames are also accepted for the `\code{tolist}' option, , but in case that such information is given as a list of pair relations, the output will be a square matrix.

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
For high dimensional arrays, the \code{\link{rel.sys}} function provides additional information other than the list of pair relations of the entire structure.
}
\seealso{
\code{\link{read.srt}}, \code{\link{bundles}}, \code{\link{reduc}}, \code{\link{rel.sys}}
}
\examples{
## scan the multiplication table data
s <- matrix(data=c(1, 1, 1, 3, 3, 3, 3, 3, 3), nrow=3, ncol=3, byrow=TRUE)

## transform the matrix to a list format
transf(s, lb2lb = TRUE, lbs = c('n','m','�'))
}
\keyword{ manip }
\keyword{ array }

