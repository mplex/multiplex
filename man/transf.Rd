\name{transf}
\alias{transf}
\title{
Transform Data from-to Matrix-List Formats
}
\description{
Function to transform data from/to matrix/list formats or edge list representing a network.
}
\usage{
transf(x, type = c("toarray", "tolist", "toarray2", "toedgel"), lbs = NULL, lb2lb, valued, 
       sep, ord, sort, sym, add, adc, rm.isol, na.rm)
}
\arguments{
  \item{x}{An array or a list of pair relations.}
  \item{type}{Type of transformation:
   \itemize{ 
    \item \code{toarray} from a list of pair relations to an array format
    \item \code{tolist} from a matrix to a list of pair relations
    \item \code{toarray2} from a list of pair relations to a square array
    \item \code{toedgel} from arrays to edge list
   }}
  \item{lbs}{(optional) Labels in the transformation.}
  \item{lb2lb}{(optional and logical) Is the transformation \dQuote{label-to-label}?}
  \item{valued}{(optional and logical) Treat \code{x} as valued?}
  \item{sep}{(optional) Pair separator used for the pairwise relations.}
  \item{ord}{(optional, for \code{"toarray"}) Order of the transformed array structure.}
  \item{sort}{(optional and logical) Sort the arrays in the output?}
  \item{sym}{(optional and logical, for \code{"toarray"}) Symmetrize the arrays?}
  \item{add}{(optional) Added elements in the array's \emph{domain}.}
  \item{adc}{(optional) Added elements in the array's \emph{codomain}.}
  \item{rm.isol}{(optional and logical) Remove isolates in \code{x}?}
  \item{na.rm}{(optional and logical) Remove missing data or \code{NA}s?}
}
\details{
Option \code{"tolist"} is for transforming a matrix or an array to a list of pair elements. 
In case that the \code{lb2lb} is enabled in this type of transformation, then \code{lbs} 
must be provided, whereas the pair separator is optional. 
Notice that the default of \code{lb2lb} is \code{TRUE} for \code{"toarray"}, while is \code{FALSE} for \code{"tolist"}.

Option type \code{"toarray"} will produce a matrix from a list of pair elements, and in this case is advisable to specify the order of the structure. 
Three dimensional structures are supported in the transformations with all options.

Data frames are also accepted for the \code{"tolist"} option; however, in case that this information 
is given as a list of pair relations the output will be a square matrix.

When the transformation option is \code{"edgel"}, the output is a data frame with the first two 
columns for the sending and receiving ties. For simple networks, these two columns are enough and for 
multiplex networks additional columns are for the types of tie, one for each (cf. function \code{\link{edgel}}).

Notice that for high dimensional arrays, the \code{\link{rel.sys}} function provides additional information 
other than the list of pair relations of the entire structure.
}
\value{
Depending on the input data, the result is either a list of pair relations or a matrix of relations.
}
%\references{} ~
%\author{} ~
%\note{} ~
%\section{Warning }{....} ~
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

