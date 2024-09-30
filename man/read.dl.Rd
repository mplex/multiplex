\name{read.dl}
\alias{read.dl}
\title{
Read \code{dl} Files
}
\description{
A function to read files with the Ucinet \code{dl} format.
}
\usage{
read.dl(file)
}
\arguments{
  \item{file}{
   character vector containing a file name or path of the data representing the network
}
}
\details{
Files \code{dl} serve to represent multiple network structures, and it is one of the formats used in \bold{NetDraw}, which is a component of the \bold{Ucinet} program. 
Besides multiple networks, the function can read two-mode structures as well.
}
\value{
a data frame for two-mode networks, or an array representing the multiple networks with one set of actors.
}
\references{
Borgatti, S.P., \bold{NetDraw} \emph{Software for Network Visualization}. Analytic Technologies. 2002.

Borgatti, S.P., Everett, M.G. and Freeman, L.C. \bold{Ucinet for Windows}: \emph{Software for Social Network Analysis}. Analytic Technologies. 2002.
}
\author{
Antonio Rivero Ostoic
}
\note{
The \sQuote{\code{EDGELIST}} option in \code{DL} is not yet supported for reading.
}
%
\seealso{
\code{\link{write.dl}}, \code{\link{edgel}}, \code{\link{read.gml}}
}
%\examples{
%}
\keyword{IO}
\keyword{data}
\keyword{file }
