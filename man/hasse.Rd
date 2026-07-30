\name{hasse}
\alias{hasse}
\title{
Hasse Diagram of Set of Ordered Relations
}
\description{
A function to plot the Hasse Diagram of partially ordered relations.
}
\usage{
hasse(x, concept, attrs = NULL, main = NULL, incmp, shape = c("rectangle", 
    "circle", "box", "ellipse"), col, col0, fcol, ecol, lty, 
    lbs, ffamily, fstyle, fsize, cex.main, col.main, bg, mar, 
    sep, ...)
}
\arguments{
  \item{x}{An object of the \dQuote{\code{Partial.Order}} class or a matrix representing ordered relations.}
  \item{concept}{(optional and logical) Plot a concept diagram?}
  \item{attrs}{(optional) Attributes for diagram.}
  \item{main}{(optional) Title of diagram.}
  \item{incmp}{(optional and logical) Include incomparable elements Hasse diagram?}
  \item{shape}{Shape of diagram vertices, otherwise \dQuote{\code{rectangle}}:
   \itemize{ 
       \item \code{rectangle}
       \item \code{circle}
       \item \code{box}
       \item \code{ellipse}
   }
  }
  \item{col}{(optional) Colour of diagram vertices.}
  \item{col0}{(optional) Colour of vertices contour.}
  \item{fcol}{(optional) Font colour in vertices.}
  \item{ecol}{(optional) Edges colour in diagram.}
  \item{lty}{(optional) Edges shape in diagram.}
  \item{lbs}{(optional) Assign labels to elements in \code{x}.}
  \item{ffamily}{(optional) Font family of vertex labels.}
  \item{fstyle}{(optional) Font style of vertex labels: 
   \itemize{ 
       \item \code{bold}
       \item \code{italic}
       \item \code{bolditalic}
   }
  }
  \item{fsize}{(optional) Font size of vertex labels.}
  \item{cex.main}{(optional) Font size of main title.}
  \item{col.main}{(optional) Font colour of main title.}
  \item{bg}{(optional) Background colour.}
  \item{mar}{(optional) Margins of diagram plot.}
  \item{sep}{(optional, for concept lattice) Pair separator for pairwise relations inside intents and extents.}
  \item{\dots}{(optional) Additional graphical items.}
}
\details{
A Hasse diagram is a pictorical device to represent systems of partially ordered relations where the \code{hasse} function provides arguments for visual manipulation of the diagram.
An example of a partially ordered system is the partial order table that is the outcome of the \sQuote{\code{strings}} option in the \code{\link{partial.order}} function. 

Roman numerals are given for elements when the partial order table is not labelled.
}
\value{
A plot of a Hasse diagram with specified settings for a partial or a linear order of relations. 
}
%\references{} ~
%\author{} ~
\note{
This function requires \pkg{Rgraphviz} package installed.
}
%\section{Warning }{....} ~
\seealso{
\code{\link{diagram}}, \code{\link{partial.order}}, \code{\link{strings}}, \code{\link{galois}}, 
\code{\link{green.rel}}, \code{\link{diagram.levels}}, \code{\link{as.strings}}.
}
\examples{
# load a dataset
data("incubA")

# a partial order of strings from dataset image matrix
po <- incubA |> getElement("IM") |> 
  as.strings() |> 
  partial.order(type="strings")

# plot the order relation as a Hasse diagram
\dontrun{if(require(Rgraphviz)) {
plot(hasse(po))
}}
}
\keyword{graphs}
