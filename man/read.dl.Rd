\name{read.dl}
\alias{read.dl}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read \code{dl} Files
}
\description{
A function to read files with the Ucinet \code{dl} format.
}
\usage{
read.dl(file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
   character vector containing a file name or path
}
}
\details{
\code{dl} files serve to represent multiple networks, and it is one of the formats used in Netdraw, which is a component of the Ucinet program.
}
\value{
a data frame or an array representing the multiple network.
}
\references{
Borgatti, S.P., \code{NetDraw} Software for Network Visualization. Analytic Technologies. 2002.

Borgatti, S.P., Everett, M.G. and Freeman, L.C. \code{Ucinet for Windows}: Software for Social Network Analysis. Analytic Technologies. 2002.
}
\author{
Antonio Rivero Ostoic
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{write.dl}}, \code{\link{read.srt}}, \code{\link{read.gml}}
}
%\examples{
%
%}
\keyword{ IO }
\keyword{ data }
\keyword{ file }
