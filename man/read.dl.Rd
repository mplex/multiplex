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
  \item{file}{Character vector containing a file name or path of the data representing the network.}
}
\details{
Ucinet \code{dl} format is to represent multiple and two-mode network structures. 
It is used in \pkg{NetDraw}, which is a component of the \pkg{Ucinet} program. 
Besides multiple networks, this function can read two-mode structures as well but 
the \sQuote{\code{EDGELIST}} option in \code{DL} is not yet supported for reading.
}
\value{
A data frame for two-mode networks, or an array representing the multiple networks with one set of actors.
}
\references{
Borgatti, S.P., \pkg{NetDraw} \emph{Software for Network Visualization}. Analytic Technologies. 2002. \cr
Borgatti, S.P., Everett, M.G. and Freeman, L.C. \pkg{Ucinet for Windows}: \emph{Software for Social Network Analysis}. Analytic Technologies. 2002.
}
%\author{} ~
%\note{} ~
%\section{Warning }{....} ~
\seealso{
\code{\link{write.dl}}, \code{\link{edgel}}, \code{\link{read.gml}}
}
%\examples{
%}
\keyword{IO}
\keyword{data}
\keyword{file}
