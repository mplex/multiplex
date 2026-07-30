\name{diagram}
\alias{diagram}
\title{
Plot Diagrams of Ordered or Linked Relations
}
\description{
A function to plot and manipulate Hasse and Concept diagrams of
ordered relations, or the Egg-box of a semigroup structure.
}
\usage{
diagram(x, type = c("hasse", "concept", "egg-box"), attrs = NULL, main = NULL, 
        incmp, shape = c("rectangle", "circle", "box", "ellipse"), col, col0, 
        fcol, ecol, lty, lbs, ffamily, fstyle, fsize, cex.main, col.main, bg, 
        mar, sep, ...)
}
\arguments{
  \item{x}{Matrix representing ordered relations.}
  \item{type}{Type of diagram:
    \itemize{ 
       \item \code{hasse} Hasse diagram of partially ordered relations.
       \item \code{concept} Concept lattice of a formal context.
       \item \code{egg-box} Egg-box diagram of an abstract semigroup.
     }}
  For \code{"egg-box"} type diagram, the following arguments are ignored: 
  %\itemize{
    \item{attrs}{(optional) a list of lists with diagram attributes for \code{node}, \code{edge}, and \code{graph}.}
    \item{main}{(optional) Main title of the plot diagram.}
    \item{incmp}{(optional and logical) Should incomparable elements be included in the lattice diagram?}
    \item{shape}{(optional) Shape of diagram vertices: \code{"rectangle"}, \code{"circle"}, \code{"box"}, \code{"ellipse"}.}
    \item{col}{(optional) Colourr of diagram vertices.}
    \item{col0}{(optional) Colour of contour in diagram vertices.}
    \item{fcol}{(optional) Font colour of text in diagram vertices.}
    \item{ecol}{(optional) Edge colour of diagram edges.}
    \item{lty}{(optional) Integer for the shape of diagram edges.}
    \item{lbs}{(optional) Labels of elements in \code{x} partially ordered set.}
    \item{ffamily}{(optional) Font family of vertex labels in diagram.}
    \item{fstyle}{(optional) Font style of vertex labels in diagram with options: \code{bold}, \code{italic}, \code{bolditalic}.}
    \item{fsize}{(optional) Font size of vertex labels in diagram.}
    \item{cex.main}{(optional) Font size of title in plot diagram.}
    \item{col.main}{(optional) Font colour of title in plot diagram.}
    \item{bg}{(optional) Background colour of plot diagram.}
    \item{mar}{(optional) Margins of plot vindue.}
    \item{sep}{(optional only for \code{concept}) Pair separator for pairwise relations inside intents and extents.}
    \item{\dots}{(optional) Additional graphical items.}
  %}
}
\details{
\code{diagram} is a wrapper function for plotting and manipulating \dQuote{Hasse}, \dQuote{Concept}, and \dQuote{Egg-box} diagrams. 

The first two diagram types represent systems of ordered relations and are displayed as partial or linear order diagrams. 
For example, the partial order table produced by the \sQuote{\code{strings}} option in the \code{\link{partial.order}} function can be plotted as a Hasse diagram. 
Another example comes from Galois derivations in Formal Concept Analysis, where a Concept diagram shows the ordering of formal concepts defined by intents and extents.
Roman numerals are rendered for diagram elements when the partial order table in \code{x} is not labelled.

On the other hand, the Egg-box \dQuote{diagram} represents equivalence classes in an abstract semigroup without a partial order structure, 
and such representation is in the Console, not on the Graphic Device as the other representations.
}
\value{
Depending on the \code{type} diagram: 
  \item{hasse}{a Hasse diagram of partially ordered relations}
  \item{concept}{a Concept diagram of formal concepts in a formal context}
  \item{egg-box}{an Egg-box of an abstract semigroup}
}
%\references{} ~
%\author{} ~
\note{
This function requires \pkg{Rgraphviz} package installed.
}
%\section{Warning }{....} ~
\seealso{
\code{\link{hasse}}, \code{\link{partial.order}}, \code{\link{strings}}, \code{\link{galois}}, 
\code{\link{green.rel}}, \code{\link{diagram.levels}}, \code{\link{as.strings}}, \code{\link[multigraph:ccgraph]{ccgraph}}.
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
plot(diagram(po, type="hasse"))
}}
}
\keyword{graphs}
%\keyword{dplot}
%
