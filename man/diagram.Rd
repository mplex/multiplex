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
       incmp, cex.main, bg, mar, shape, col, col0, fcol, ecol, lty, lbs, 
       ffamily, fstyle, fsize, col.main, sep, ...)
}
\arguments{
  \item{x}{
   a matrix representing ordered relations
}
  \item{type}{
  type of diagram:
  \itemize{ 
       \item \code{hasse} Hasse diagram of partially ordered relations
       \item \code{concept} Concept lattice of a formal context
       \item \code{egg-box} Egg-box diagram of an abstract semigroup
       }
  }
For \code{egg-box}, the following arguments are ignored. 
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
\code{diagram} is a wrapper function to plot and manipulate \dQuote{Hasse}, \dQuote{Concept}, and \dQuote{Egg-box} types of diagrams. 

The first two diagrams are for systems of ordered relations, and the plotted deviced is either a partial order or a linear order diagram.
An example of ordered relations is found in the partial order table of relations product of the \sQuote{\code{strings}} option in the \code{\link{partial.order}} function, and which is plotted as a Hasse diagram. 
Another set of ordered relations comes from the table produced on Galois derivations within Formal Concept analysis where a Concept diagram represents the ordering relations among formal concepts made of intents and extents.

The Egg-box diagram is for equivalence classes in an abstract semigroup not associated to a partial order structure.
}
\value{
Depending on the \code{type}: 
  \item{hasse}{a Hasse diagram of partially ordered relations}
  \item{concept}{a Concept diagram of formal concepts in a formal context}
  \item{egg-box}{an Egg-box of an abstract semigroup}
}
%\references{
%
%}
\author{
Antonio Rivero Ostoic
}
\note{
Roman numerals are given for elements when the partial order table is not labelled.
}
\section{Warning}{
Requires \emph{Rgraphviz} package installed.
}

\seealso{
\code{\link{hasse}}, \code{\link{partial.order}}, \code{\link{strings}}, \code{\link{galois}}, 
\code{\link{green.rel}}, \code{\link{diagram.levels}}, \code{\link{as.strings}}, \code{\link[multigraph:ccgraph]{ccgraph}}.
}
\examples{
# load a dataset
data("incubA")

# given e.g. a partial order table in the object 'po'
po <- as.strings(incubA$IM) |> 
  partial.order(type="strings")

# plot the order relation as a Hasse diagram
\dontrun{if(require(Rgraphviz)) {
plot(diagram(po, type="hasse"))
}}
}

\keyword{graphs}
%\keyword{dplot}
%
