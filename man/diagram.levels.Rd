\name{diagram.levels}
\alias{diagram.levels}
%
\title{
Levels in Lattice Diagram
}
\description{
A function that reads the different levels in the lattice diagram of the partial order structure among actors and ties in the network
}
\usage{
diagram.levels(x, perm = FALSE)
}
%
\arguments{
  \item{x}{
   A matrix representing the partial order
}
  \item{perm}{
   (optional) whether or not to return the permuted structure
   }
}
\details{
When it comes to reduce the structure of a multiple network, many times the partial order structure provides different 
classes of elements depending in the inclusions these elements have. 
In this sense, the illustration given by the \code{\link{diagram}} function provides us typically with different levels 
of the ordered relations, which are read by this routine.
}
\value{
A named list with components of the \dQuote{levels} in the concept diagram produced by \code{\link{diagram}}.

If the permutation is specified in \code{perm}, a data frame with the elements of the partial order structure with 
the column names indicating the element class plus a vector with the levels and a matrix with the permuted structure are given as well.
}
%
\author{
Antonio Rivero Ostoic
}
\note{
This function requires that the \emph{Rgraphviz} package is installed. 
Since function \code{grDevices::pictex()} inside this routine is for historical interest only since \strong{R} 4.4.0, 
the warning message has been suppressed before its future replacement.
}
%
\seealso{
\code{\link{partial.order}}, \code{\link{diagram}}, \code{\link{perm}}
}
\examples{
# load the data
data("incubA")

# given e.g. a partial order table in the object 'po'
po <- as.strings(incubA$IM) |> 
  partial.order()

# find the levels in the lattice diagram
\dontrun{
diagram.levels(po)
}
}
\keyword{algebra}
\keyword{manip}
