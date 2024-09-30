\name{read.gml}
\alias{read.gml}
\title{
Read \code{gml} Files
}
\description{
A function to read files with the \code{gml} format.
}
\usage{
read.gml(file, as = c("edgel", "array"), directed = TRUE, coords = FALSE)
}
\arguments{
  \item{file}{  
   character vector containing a file name or path
}
  \item{as}{
   should the data be given as 
  \itemize{ 
    \item \code{edgel} for edge list with send/receive/ties format
    \item \code{array} for two- or three-dimensional array
  }
}
  \item{directed}{
   (logical) whether the graph is directed or undirected.
}
  \item{coords}{
   (logical) whether the coordenates in the \code{gml} file should be included.
}
}
\details{
The \code{gml} format, an acronym for \emph{graph modelling language}, provides capabilities to represent multiple networks and 
add arguments both to the nodes and the edges for visualization purposes.

For the multiplexity in the ties the \code{gml} file distinguishes \dQuote{graphics} arguments inside \dQuote{edge}. 
Both \dQuote{style} and \dQuote{fill} are supported here and the former has priority over the latter in case the two are given; 
otherwise when these arguments are absent, the function separates up to a couple of relational levels when several pairwise ties are specified.
}
\value{
Depending the option chosen, the output is either a data frame or an array representing the multigraph. 
If the coordenates are chosen then these are part of the obejct structure, but they are not visible.
}
\references{
\bold{visone}: \emph{Software for the analysis and visualization of social networks}. \url{http://visone.info}
}
\author{
Antonio Rivero Ostoic
}
\note{
Node attributes can also be retrieved when the coordinates are chosen.
}
\seealso{
\code{\link{write.gml}}, \code{\link{edgel}}, \code{\link{read.dl}}
}
%\examples{
%}
\keyword{IO}
\keyword{data}
\keyword{file }
