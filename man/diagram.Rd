\name{diagram}
\alias{diagram}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot the Hasse Diagram of a set of ordered relations
}
\description{
A function to plot the Hasse Diagram of partially ordered relations.
}
\usage{
diagram(x, unord = TRUE, attrs = NULL, main = NULL, cex.main = graphics::par()$cex.main, 
        bg = graphics::par()$bg, ...)
}
\arguments{
  \item{x}{
   a matrix representing ordered relations
}
  \item{unord}{
   (logical) whether or not the elements not belonging in the order should be included in the diagram
}
  \item{attrs}{
   (optional) attributes of the diagram
}
  \item{main}{
   (optional) title of the diagram
}
  \item{cex.main}{
   (optional) size of the diagram's title
}
  \item{bg}{
   (optional) the background color of the diagram
}
  \item{...}{
   (optional) other graphical items
}

}
\details{
An example of ordered relations is found in the partial order table of relations, which is product of the `\code{strings}' option in the \code{\link{partial.order}} function. Another set of ordered relations comes from the table produced on Galois derivations in the mentioned function.

In either case this function plot either the partial order or a linear order diagram, depending on the results as Hasse diagrams. 
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
Note that if the elements of the partial order are not labelled, Roman numerals will be given to each element.
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
