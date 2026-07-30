\name{diagram.levels}
\alias{diagram.levels}
\title{
Levels in Lattice Diagram
}
\description{
A function to represent a textual interpretation of the levels in a given lattice diagram.
}
\usage{
diagram.levels(x, perm = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A matrix representing partial order relations.}
  \item{perm}{optional and logical) Return automorphism of a permuted structure?}
}
\details{
This function provides a textual interpretation of the different levels in the lattice diagram of the partial order structure among actors and ties in the network.

When reducing a multiple network, the partial order structure may define different classes of elements based on their inclusion relations. 
The illustration produced by the \code{\link{diagram}} function typically shows these levels of ordered relations, which this routine then interprets.
}
\value{
A named list with components of the \dQuote{levels} in the concept diagram produced by \code{\link{diagram}}.

If \code{perm} is specified, the function returns a data frame containing the elements of the partial order structure, with column names indicating the element classes. 
It also returns a vector of levels and a matrix of the permuted structure.
}
%\references{} ~
%\author{} ~
\note{
This function requires that the \pkg{Rgraphviz} package is installed. 
Since function \code{grDevices::pictex()} inside this routine is for historical interest only since \proglang{R} \code{4.4.0}, 
the warning message has been suppressed before its future replacement.
}
%\section{Warning }{....} ~
\seealso{
\code{\link{partial.order}}, \code{\link{diagram}}, \code{\link{perm}}
}
\examples{
# load the data
data("incubA")

# given e.g. a partial order table in the object 'po'
po <- incubA |> getElement("IM") |> 
  as.strings() |> 
  partial.order(type="strings")

# find the levels in the lattice diagram
\dontrun{
diagram.levels(po)
}
}
\keyword{algebra}
\keyword{manip}
