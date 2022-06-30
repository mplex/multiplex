\name{diagram}
\alias{diagram}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot a Hasse Diagram of a set of ordered relations
}
\description{
A function to plot a Hasse Diagram of partially ordered relations.
}
\usage{
diagram(x, attrs = NULL, main = NULL, incmp, cex.main, bg, mar, shape, 
        col, col0, fcol, ecol, lty, lbs, ffamily, fstyle, fsize, col.main, 
        sep,...)
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
   (optional and logical) whether or not the incomparable elements should be included in the lattice diagram
}
  \item{cex.main}{
   (optional) size of the diagram's title
}
  \item{bg}{
   (optional) the background color of the diagram
}
  \item{mar}{
   (optional) the margins of the plot
}
  \item{shape}{
   (optional) the shape of the vertices
}
  \item{col}{
   (optional) the color of the vertices
}
  \item{col0}{
   (optional) the color of the vertices' contour
}
  \item{fcol}{
   (optional) the color of the text's vertices
}
  \item{ecol}{
   (optional) the color of the edges
}
  \item{lty}{
   (optional) the shape of the edges
}
  \item{lbs}{
   (optional) labels of the elements in the partially ordered set
}
  \item{ffamily}{
   (optional) the font family of the vertex labels
}
  \item{fstyle}{
   (optional) the font style of the vertex labels with options: `bold', `italic', `bolditalic'
}
  \item{fsize}{
   (optional) the font size of the vertex labels
}
  \item{col.main}{
   (optional) the color of the diagram's title
}
  \item{sep}{
   (optional, only for `lbs') string separator for equations
}
  \item{...}{
   (optional) additional graphical items
}

}
\details{
An example of ordered relations is found in the partial order table of relations, which is a product of the `\code{strings}' option in the \code{\link{partial.order}} function. Another set of ordered relations comes from the table produced on Galois derivations in the mentioned function.
In either case, this function plot either the partial order or a linear order diagram, depending on the results as Hasse diagrams. 

When the partial order structure is from a decomposition process, string equations can be placed as labels with an equality separator. 
}
\value{
A Hasse diagram of the partial order relation.
}
%\references{
%No hay...
%}
\author{
Antonio Rivero Ostoic
}
\note{
Roman numerals are used if the elements of the partial order are not labelled. 
}
\section{Warning}{
This function requires that the \emph{Rgraphviz} package is available.
}

\seealso{
\code{\link{partial.order}}, \code{\link{as.strings}}, \code{\link{strings}}, \code{\link{diagram.levels}}, \code{\link{galois}}.
}
\examples{
## load the data
data("incubA")

## given e.g. a partial order table in the object 'po'
po <- partial.order(as.strings(incubA$IM), type="strings")


## plot the order relation as a Hasse diagram.
\dontrun{if(require(Rgraphviz)) {
plot(diagram(po))
}}
}

\keyword{graphs}
%\keyword{dplot}
%
