\name{hasse}
\alias{hasse}
\title{
Hasse Diagram of Set of Ordered Relations
}
\description{
A function to plot the Hasse Diagram of partially ordered relations.
}
\usage{
hasse(x, attrs = NULL, main = NULL, incmp, cex.main, bg, mar, shape, col, 
      col0, fcol, ecol, lty, lbs, ffamily, fstyle, fsize, col.main, sep, ...)
}
\arguments{
  \item{x}{
   a matrix representing ordered relations
}
  \item{attrs}{
   (optional) attributes of the diagram
}
  \item{main}{
   (optional) title of the diagram
}
  \item{incmp}{
   (logical) whether or not incomparable elements should be included in the lattice diagram
}
  \item{cex.main}{
   (optional) font size of diagram's title
}
  \item{bg}{
   (optional) background color of diagram
}
  \item{mar}{
   (optional) margins of plot
}
  \item{shape}{
   (optional) shape of vertices
}
  \item{col}{
   (optional) color of vertices
}
  \item{col0}{
   (optional) color of vertices' contour
}
  \item{fcol}{
   (optional) color of text's vertices
}
  \item{ecol}{
   (optional) color of edges
}
  \item{lty}{
   (optional) shape of edges
}
  \item{lbs}{
   (optional) labels of elements in partially ordered set
}
  \item{ffamily}{
   (optional) font family of vertex labels
}
  \item{fstyle}{
   (optional) font style of vertex labels with options: 
   \itemize{ 
       \item \code{bold}
       \item \code{italic}
       \item \code{bolditalic}
   }
}
  \item{fsize}{
   (optional) font size of vertex labels
}
  \item{col.main}{
   (optional) font color of main title
}
  \item{sep}{
   (optional) pair separator for pairwise relations inside intents and extents
  }
  \item{...}{
   (optional) additional graphical items
}

}
\details{
A Hasse diagram is a pictorical device to represent systems of partially ordered relations where the \code{hasse} function provides arguments for visual manipulation of the diagram.
An example of a partially ordered system is the partial order table that is the outcome of the \sQuote{\code{strings}} option in the \code{\link{partial.order}} function. 
}
\value{
A plot of a Hasse diagram with specified settings for a partial or a linear order of relations. 
}
%
\author{
Antonio Rivero Ostoic
}
\note{
Roman numerals are given for elements when the partial order table have \code{NULL} dimnames.
}
\section{Warning}{
Requires \emph{Rgraphviz} package installed.
}

\seealso{
\code{\link{diagram}}, \code{\link{partial.order}}, \code{\link{strings}}, \code{\link{galois}}, 
\code{\link{green.rel}}, \code{\link{diagram.levels}}, \code{\link{as.strings}}.
}
\examples{
# load a dataset
data("incubA")

# given e.g. a partial order table in the object 'po'
po <- as.strings(incubA$IM) |> 
  partial.order(type="strings")

# plot the order relation as a Hasse diagram
\dontrun{if(require(Rgraphviz)) {
plot(hasse(po))
}}

}
\keyword{graphs}
