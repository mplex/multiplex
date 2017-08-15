\name{write.dl}
\alias{write.dl}
\title{
Write \code{dl} Files
}
\description{
A function to write \code{dl} files representing multiple networks.
}
\usage{
write.dl(x, file = NULL, type = c("nodelist", "fullmat"))
}
\arguments{
  \item{x}{  an object representing the multiple network
}
  \item{file}{  path to the file
}
  \item{type}{  whether to write the data as a \code{nodelist} or as a \code{fullmat} format
}
}
\details{
\code{dl} files serve to represent multiple networks, and it is one of the formats used in Netdraw, which is a component of the Ucinet program.
}
\value{
A file with the data with a \code{.dl} format
%% ...
}
\references{
Borgatti, S.P., \code{NetDraw} Software for Network Visualization. Analytic Technologies. 2002.

Borgatti, S.P., Everett, M.G. and Freeman, L.C. \code{Ucinet for Windows}: Software for Social Network Analysis. Analytic Technologies. 2002.
}
\author{
Antonio Rivero Ostoic
}
%\note{
%
%}
\seealso{
\code{\link{read.dl}}, \code{\link{write.gml}}, \code{\link{write.srt}}, \code{\link{write.dat}}
}
%\examples{
%
%}
\keyword{ IO }
\keyword{ data }
\keyword{ file }
