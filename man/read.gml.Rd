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
  \item{file}{Character vector containing a file name or path of the data representing the network.}
  \item{as}{Data format for output: 
  \itemize{ 
    \item \code{edgel} Edge list with send/receive/ties format
    \item \code{array} Two- or three-dimensional array
  }}
  \item{directed}{(logical) Is the graph directed?}
  \item{coords}{(optional and logical) Include coordenates in \code{gml} file?}
}
\details{
The \code{gml} format, an acronym for \emph{graph modelling language}, provides capabilities to represent multiple networks and 
add arguments both to the nodes and the edges for visualization purposes.

For the multiplexity in the ties the \code{gml} file distinguishes \dQuote{graphics} arguments inside \dQuote{edge}. 
Both \dQuote{style} and \dQuote{fill} are supported here and the former has priority over the latter in case the two are given; 
otherwise when these arguments are absent, the function separates up to a couple of relational levels when several pairwise ties are specified.

Node attributes can be retrieved as well when coordinates in \code{coords} are chosen.
}
\value{
Depending the option chosen, the output is either a data frame or an array representing the multigraph. 
If the coordenates are chosen then these are part of the obejct structure, but they are not visible.
}
\references{
\pkg{visone}: \emph{Software for the analysis and visualization of social networks}. \url{http://visone.info}
}
%\author{} ~
%\note{} ~
%\section{Warning }{....} ~
\seealso{
\code{\link{write.gml}}, \code{\link{edgel}}, \code{\link{read.dl}}
}
%\examples{
%}
\keyword{IO}
\keyword{data}
\keyword{file }
