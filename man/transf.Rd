\name{transf}
\alias{transf}
\title{
Transform Data from/to Matrix/List Formats
}
\description{
Function to transform data from/to matrix/list formats or edge list representing a network.
}
\usage{
transf(x, type = c("toarray", "tolist", "toarray2", "toedgel"), lbs = NULL, lb2lb, 
    sep, ord, sort, sym, add, adc, na.rm)
}
\arguments{
  \item{x}{
   an array or a list of pair relations
  }
  \item{type}{
   type of transformation:
  \itemize{ 
   \item \code{toarray} from a list of pair relations to an array format
   \item \code{tolist} from a matrix to a list of pair relations
   \item \code{toarray2} from a list of pair relations to a square array
   \item \code{toedgel} from arrays to edge list
  }
  }
  \item{lbs}{
   (optional) the labels in the transformation
  }
  \item{lb2lb}{
   (optional and logical) whether the transformation is \emph{label-to-label}. Default \code{TRUE} for \code{"toarray"} and \code{FALSE} for \code{"tolist"}
  }
  \item{sep}{
   (optional) pair separator used for the pairwise relations
  }
  \item{ord}{
   (optional) for \code{"toarray"}, the order of the resulted structure
  }
  \item{sort}{
   (optional and logical) sort the arrays in the output?
  }
  \item{sym}{
   (optional and logical) for \code{"toarray"}, symmetrize the arrays?
  }
  \item{add}{
   (optional) added elements in the array's \emph{domain}
  }
  \item{adc}{
   (optional) added elements in the array's \emph{codomain}
  }
  \item{na.rm}{
   (optional) remove missing data in \code{NA}?
  }
}
\details{
Option \code{"tolist"} is for transforming a matrix or an array to a list of pair elements. 
In case that the \code{lb2lb} is enabled in this type of transformation, then \code{lbs} 
must be provided, whereas the pair separator is optional. On the other hand, \code{"toarray"} 
will produce a matrix from a list of pair elements, and in this case is advisable to specify the order of the structure. 
Three dimensional structures are supported in the transformations with all options.

Data frames are also accepted for the \code{"tolist"} option; however, in case that this information 
is given as a list of pair relations the output will be a square matrix.

When the transformation option is \code{"edgel"}, the output is a data frame with the first two 
columns for the sending and receiving ties. For simple networks, these two columns are enough and for 
multiplex networks additional columns are for the types of tie, one for each (cf. function \code{\link{edgel}}).
}
\value{
Depending on the input data, the result is either a list of pair relations or a matrix of relations.
}
%\references{
%}
\author{
Antonio Rivero Ostoic
}
\note{
For high dimensional arrays, the \code{\link{rel.sys}} function provides additional information other than the list of pair relations of the entire structure.
}
\seealso{
\code{\link{edgel}}, \code{\link{bundles}}, \code{\link{reduc}}, \code{\link{rel.sys}}
}
\examples{
# scan the multiplication table data
s <- matrix(data=c(1, 1, 1, 3, 3, 3, 3, 3, 3), nrow=3, ncol=3, byrow=TRUE)

# transform the matrix to a list format
s |> transf(lb2lb = TRUE, lbs = c("n","m","u"))
}
\keyword{manip}
\keyword{array}

