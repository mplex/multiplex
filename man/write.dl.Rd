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
  \item{type}{  
  write data with format type: 
  \itemize{ 
    \item \code{nodelist} for \emph{node-list} format
    \item \code{fullmat} for \emph{fullmat} format
  }
}
}
\details{
\code{dl} files serve to represent multiple networks, and it is one of the formats used in \bold{NetDraw}, which is a component of the \bold{Ucinet} program.
}
\value{
A file with the data with a \code{.dl} format
%% ...
}
\references{
Borgatti, S.P., \bold{NetDraw} \emph{Software for Network Visualization}. Analytic Technologies. 2002.

Borgatti, S.P., Everett, M.G. and Freeman, L.C. \bold{Ucinet for Windows}: \emph{Software for Social Network Analysis}. Analytic Technologies. 2002.
}
\author{
Antonio Rivero Ostoic
}
%\note{
%}
\seealso{
\code{\link{read.dl}}, \code{\link{write.gml}}, \code{\link{write.srt}}, \code{\link{write.dat}}
}
%\examples{
%}
\keyword{IO}
\keyword{data}
\keyword{file }
