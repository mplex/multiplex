\name{write.dl}
\alias{write.dl}
\title{
Write \code{dl} Format Files
}
\description{
A function to write \code{dl} files representing multiple networks.
}
\usage{
write.dl(x, file = NULL, type = c("nodelist", "fullmat"))
}
\arguments{
  \item{x}{Array or object representing a multiple network structure.}
  \item{file}{Path with filename for output.}
  \item{type}{
  Write data format type: 
  \itemize{ 
    \item \code{nodelist} for \emph{node-list} format
    \item \code{fullmat} for \emph{fullmat} format
  }}
}
\details{
Files with \code{dl} format serve to represent multiple networks, and it is one used in \pkg{NetDraw}, which is a component of the \pkg{Ucinet} program.
}
\value{
A file in path with the data with a \code{.dl} format.
}
\references{
Borgatti, S.P., \pkg{NetDraw} \emph{Software for Network Visualization}. Analytic Technologies. 2002. \cr
Borgatti, S.P., Everett, M.G. and Freeman, L.C. \pkg{Ucinet for Windows}: \emph{Software for Social Network Analysis}. Analytic Technologies. 2002.
}
%\author{} ~
%\note{} ~
%\section{Warning }{....} ~
\seealso{
\code{\link{read.dl}}, \code{\link{write.gml}}, \code{\link{write.edgel}}, \code{\link{write.dat}}
}
%\examples{
%}
\keyword{IO}
\keyword{data}
\keyword{file }
