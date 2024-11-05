\name{edgel}
\alias{edgel}
\alias{read.srt}
\title{
Read Edge List Files
}
\description{
A function to read edge list files with \emph{s}end, \emph{r}eceive, and \emph{t}ies format 
for a multivariate network with the possibility to transform it into an three dimenasional array.
}
\usage{
edgel(file, header = TRUE, sep = "\t", toarray = TRUE, dichot = FALSE, 
      attr = FALSE, rownames = FALSE, add = NULL, na.rm)
}
%
\arguments{
  \item{file}{
  path to the file
}
  \item{header}{
  (logical) does the file has a header?
}
  \item{sep}{
   the separator among the columns (default is horizontal tab escaped as \code{"\t"})
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
  \item{na.rm}{
  (optional and logical) remove \code{NA}s in \code{file}?
}
}
\details{
\code{edgel} is a function to read edge list files with \emph{s}end, \emph{r}eceive, and \emph{t}ies format, 
which is a data frame with at least 2 columns for the sender, receiver and for multiplex networks also the ties, one column for each type of relation. 
However, the \code{attr} option correspond to a actor and self-ties data frame file with the option to transform it into a diagonal matrix. 
When \code{toarray} is set to \code{FALSE}, options \code{attr} and \code{rownames} allow placing the first column of the data frame as the name of the table, 
which is the format of two-mode data, and compute for instance Galois transformations among the partite sets. 
If more than one isolate is added, then the data must be included as a vector.

It is also possible to treat the input data as data frame object and manipulate it through e.g. the \code{\link[base:subset]{subset}} function with the \code{toarray} option. 
Valued networks are now supported as well.
}
\value{
By default an array; usually with three dimensions of stacked matrices where the multiple relations are placed. 
If \code{toarray = FALSE}, then the data frame is returned.
}
%
\author{
Antonio Rivero Ostoic
}
\note{
For backwards compatibility, an alias for \code{edgel} is \code{read.srt}.
}

\seealso{
\code{\link{write.edgel}}, \code{\link{read.gml}}, \code{\link{read.dl}}, \code{\link{galois}}
}
%\examples{
%}
\keyword{IO}
\keyword{manip}
\keyword{data}
