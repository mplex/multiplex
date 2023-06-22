\name{diagram.levels}
\alias{diagram.levels}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Levels in the Lattice Diagram
}
\description{
This is a function that reads the different levels in the lattice diagram of the partial order structure among actors and ties in the network
}
\usage{
diagram.levels(x, perm = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
   A matrix representing the partial order
}
  \item{perm}{
   (optional) whether or not to return the permuted structure
   }
}
\details{
When it comes to reduce the structure of a multiple network, many times the partial order structure provides different classes of elements depending in the inclusions these elements have. In this sense, the illustration given by the \code{\link{diagram}} function provides us typically with different levels of the ordered relations, which are read by this routine.
}
\value{
A named list with components of the ``levels'' in the concept diagram produced by \code{\link{diagram}}.

If the permutation is specified, a data frame with the elements of the partial order structure with the column names indicating the element class plus a vector with the levels and a matrix with the permuted structure are given as well.
}
%\references{
%No hay...
%}
\author{
Antonio Rivero Ostoic
}
\note{
This function requires that the \emph{Rgraphviz} package is available. Besides, since the \code{pictex} function from \code{grDevices} is inside this routine, which may imply in some cases running with administrator privileges.
}

%% ~Make other sections like Warning with \section{Warning }{...} ~

\seealso{
\code{\link{partial.order}}, \code{\link{diagram}}, \code{\link{perm}}
}
\examples{
## load the data
data("incubA")

## given e.g. a partial order table in the object 'po'
po <- partial.order(as.strings(incubA$IM))

## find the levels in the lattice diagram
\dontrun{diagram.levels(po)}
}
\keyword{ algebra }
\keyword{ manip }
