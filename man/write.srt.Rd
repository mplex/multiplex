\name{write.srt}
\alias{write.srt}
\title{
Write \code{srt} Files
}
\description{
A function to write \code{srt} files
}
\usage{
write.srt(x, file = NULL, sep = "\t", header = TRUE)
}
\arguments{
  \item{x}{
  an object representing the multiple network
}
  \item{file}{
  path to the file
}
  \item{sep}{
  the separator used between the columns
}
  \item{header}{
  (logical) whether the header should be included in the file
}
}
\details{
\code{srt} stands for \emph{s}end, \emph{r}eceive, and \emph{t}ies, and it is a data frame with at least 3 columns for the sender, receiver, and the ties, one column for each type of relation.
}
\value{
A file with the data with a \code{.srt} format
}
%\references{
%
%}
\author{
Antonio Rivero Ostoic
}
%\note{
%
%}
\seealso{
\code{\link{read.srt}}, , \code{\link{write.dl}}
}

%\examples{
%
%}
\keyword{ IO }
\keyword{ data }
\keyword{ file }
