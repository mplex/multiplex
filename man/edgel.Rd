\name{edgel}
\alias{edgel}
\alias{read.srt}
\title{
Read and Transform Edge-Lists
}
\description{
A function to read edge lists as objects or files with a \sQuote{sender-receiver-tie} 
format representing multiplex networks and to transform edge-lists into three-dimensional arrays.
}
\usage{
edgel(x, toarray = TRUE, header = TRUE, sep = "\t", valued = FALSE, inc = FALSE, 
      rownames = TRUE, add = NULL, na.rm)
}
\arguments{
  \item{x}{Object or file representing network data.}
  \item{toarray}{(logical) Transform edge-list in \code{x} into an array?}
  \item{header}{(optional and logical) Does edge-list in file \code{x} has a header?}
  \item{sep}{(default horizontal tab) Separator between edge-list columns.}
  \item{valued}{(logical, for \code{toarray}) Return a transformation of \code{x} as a valued network?}
  \item{inc}{(optional and logical) Is \code{x} an incidence matrix?}
  \item{rownames}{(optional and logical, for \code{inc}) Treat \sQuote{rownames} in \code{x} as actors/objects names? See details.}
  \item{add}{(optional) Vector with isolate labels to add to the output.}
  \item{na.rm}{(optional and logical) Remove \code{NA}s from \code{x} in the output?}
}
\details{
The \code{edgel} function reads edge lists in \sQuote{sender-receiver-tie} format as a data frame 
with at least two columns for sender and receiver, plus one column for each relation type in a multiplex network. 
For simple networks, two columns for sender and receiver are sufficient. 
The input in \code{x} may also be a file containing tabular data, where the separator and header are specified with \code{sep} and \code{header}.

The \code{inc}  option is used for \emph{incidence} matrices with actors or objects and self-ties in data frames, 
which are relevant for two-mode data and analyses such as Galois transformations between partite sets. 
Incidence matrices can be converted into diagonal matrices for further analysis. 
When \code{toarray} is set to \code{FALSE}, argument \code{rownames} can be used to move labels between row names and the first column.

Additional options make it possible to remove missing values with \code{na.rm}, add isolated nodes with \code{add}, 
and choose between dichotomizing or preserving values in networks with \code{valued}.
}
\value{
By default the outcome is an array that for multiple networks is a three dimensions of stacked matrices where the relation types are placed. 
However, if \code{toarray} is set to \code{FALSE}, then the edge-list with a data frame format is returned.
\code{warning} messages for missing information, with the tranformation type, and when \sQuote{one type of relation assumed} in \code{x}.
}
%\references{} ~
%\author{} ~
\note{
For backwards compatibility, an alias for this fuction is \code{read.srt}.
}
%\section{Warning }{....} ~
\seealso{
\code{\link{write.edgel}}, \code{\link{galois}}, \code{\link{read.gml}}, \code{\link{read.dl}}
}
%\examples{
%}
\keyword{IO}
\keyword{manip}
\keyword{data}
