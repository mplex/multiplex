\name{read.gml}
\alias{read.gml}
\title{
Read \code{gml} Files
}
\description{
A function to read files with the \code{gml} format.
}
\usage{
read.gml(file, as = c("srt", "array"), directed = TRUE, coords = FALSE)
}
\arguments{
  \item{file}{  
   character vector containing a file name or path
}
  \item{as}{
   should the data be given as a \code{srt} or with an \code{array} format?
}
  \item{directed}{
   (logical) whether the graph is directed or undirected.
}
  \item{coords}{
   (logical) whether the coordinates in the \code{gml} file should be included.
}
}
\details{
The \code{gml} format, an acronym for \emph{graph modelling language}, provides capabilities to represent multiple networks and add arguments to both the nodes and the edges for visualization purposes.

For the multiplexity in the ties, the \code{gml} file distinguishes ``graphics'' arguments inside ``edge''. 
Both ``style'' and ``fill'' are supported here and the former has priority over the latter in case the two are given; otherwise when these arguments are absent. 
The function separates up to a couple of relational levels when several pairwise ties are specified.
}
\value{
Depending on the option chosen, the output is either a data frame or an array representing the multigraph. If the coordinates are chosen then these are part of the object structure, but they are not visible.
}
\references{
\code{visone} Software for the analysis and visualization of social networks. \emph{http://visone.info}
}
\author{
Antonio Rivero Ostoic
}
\note{
If the coordinates are chosen, node attributes can also be retrieved.
}
\seealso{
\code{\link{write.gml}}, \code{\link{read.srt}}, \code{\link{read.dl}}
}
%\examples{
%
%}
\keyword{ IO }
\keyword{ data }
\keyword{ file }
