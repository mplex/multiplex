\name{read.srt}
\alias{read.srt}
\title{
Read edgelist (\code{srt}) files
}
\description{
A function to read edgelist files with a \emph{s}end, \emph{r}eceive, and \emph{t}ies format to make a three-dimensional array.
}
\usage{
read.srt(file, header = TRUE, sep = "\t", toarray = TRUE, dichot = FALSE, 
    attr = FALSE, rownames = FALSE, add = NULL) 
}
\arguments{
  \item{file}{
  path to the file
}
  \item{header}{
  (logical) does the file has a header?
}
  \item{sep}{
   the separator among the columns (default is horizontal tab)
}
  \item{toarray}{
  (logical) should the data frame be transformed to arrays?
}
  \item{dichot}{
  (logical) should the data be dichotomized?
}
  \item{attr}{
   (logical) whether or not the file corresponds to attribute-based data
}
  \item{rownames}{
  (logical) are rownames the labels of the nodes?
}
  \item{add}{
  (optional) isolates to be added to the network
}
}
\details{
\code{srt} stands for send, receive, and ties, and it is a data frame with at least 2 columns for the \emph{s}end, \emph{r}eceive, and the \emph{t}ies for a multivariate network, with one column for each type of relation. 
However, the \code{attr} option corresponds to an actor and self-ties data frame file with the option to transform it into a diagonal matrix. 
When option \code{toarray} is set to \code{FALSE}, options \code{attr} and \code{rownames} allow placing the first column of the data frame as the name of the table, which is the format of two-mode data, and compute for instance Galois transformations among the partite sets. 
If more than one isolate is added, then the data must be included as a vector.

It is also possible to treat the input data as data frame object and manipulate it via e.g. the \code{\link[base:subset]{subset}} function with the \code{toarray} option.
}
\value{
By default an array; usually with three dimensions of stacked matrices where the multiple relations are placed. If \code{toarray = FALSE}, then the data frame is given.
}
%\references{
%No hay references todavia
%}
\author{
Antonio Rivero Ostoic
}
\note{
The function supports valued networks as well.
}
\seealso{
\code{\link{write.srt}}, \code{\link{read.gml}}, \code{\link{read.dl}}, \code{\link{galois}}
}
%\examples{
%
%}
\keyword{ IO }
\keyword{ manip }
\keyword{ data }

