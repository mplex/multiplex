\name{read.srt}
\alias{read.srt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read \code{srt} Files
}
\description{
A function to read files with \emph{s}end, \emph{r}eceive, and \emph{t}ies format for a multivariate network with the possibility to transform it into an three dimenasional array.
}
\usage{
read.srt(file, header = TRUE, sep = "\t", attr = FALSE , 
         toarray = TRUE, dichot = FALSE, labels = NULL)
}
%- maybe also 'usage' for other objects documented here.
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
  \item{attr}{
   (logical) whether or not the file corresponds to attribute-based data
}
  \item{toarray}{
  (logical) should the data frame be transformed to arrays?
}
  \item{dichot}{
  (logical) should the data be dichotomized?
}
  \item{labels}{
  (optional) labels of the nodes
}
}
\details{
\code{srt} stands for \emph{s}end, \emph{r}eceive, and \emph{t}ies, and it is a data frame with at least 3 columns for the sender, receiver, and the ties, one column for each type of relation. However, the \code{attr} option correspond to a actor and self-ties data frame file with the option to transform it to an identical matrix.
It is also possible to treat the input data as data frame object and manipulate it via e.g. the \code{\link[base:subset]{subset}} function with the \code{toarray} option.

}
\value{
By default an array; usually with three dimensions of stacked matrices where the multiple relations are placed. If \code{toarray = FALSE}, then the data frame is given.
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
%\references{
%No hay references todavia
%}
\author{
Antonio Rivero Ostoic
}
%\note{
%
%}
\seealso{
\code{\link{write.srt}}, \code{\link{read.gml}}, \code{\link{read.dl}}
}
%\examples{
%
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ IO }
\keyword{ manip }
\keyword{ data }

