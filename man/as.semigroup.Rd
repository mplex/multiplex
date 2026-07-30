\name{as.semigroup}
\alias{as.semigroup}
\title{
Coerce into a Semigroup Object
}
\description{
A generic function for coercing a relational system into a \dQuote{\code{Semigroup}} class object.
}
\usage{
as.semigroup(x, gens, lbs, numerical, edgeT)
}
\arguments{
  \item{x}{An array representing the semigroup.}
  \item{gens}{Aarray or vector representing the semigroup generators.}
  \item{lbs}{(optional) Label strings for the semigroup.}
  \item{numerical}{(optional and logical) Should the semigroup have numerical format?}
  \item{edgeT}{(optional and logical) Is the input in \sQuote{\code{x}} an edge table?}
}
\details{
Use this function to convert an array representing a semigroup structure into an \R object of class \dQuote{\code{Semigroup}} 
as some routines in \pkg{multiplex} require.
}
\value{
A list object of class \dQuote{\code{Semigroup}} that is either \sQuote{\code{symbolic}} or \sQuote{\code{numerical}}.
  \item{ord}{an integer with the dimension of the semigroup}
  \item{st}{a vector of the semigroup unique relations or the string-relations}
  \item{gens}{a vector with the generators of the semigroup}
  \item{S}{multiplication table with the semigroup structure.}
  Class attribute, and a \code{warning} message when no generators are provided.
}
%\references{} ~
%\author{} ~
%\note{} ~
%\section{Warning }{....} ~
\seealso{
\code{\link{semigroup}}, \code{\link{green.rel}}
}
\examples{
# create a labeled multiplication table data
s <- matrix(data=c(1, 1, 1, 3, 3, 3, 3, 3, 3), nrow=3, ncol=3, byrow=TRUE)

# set attributes to object
attr(s, "dimnames") <- list(1:3, 1:3)

# make 's' a semigroup object
as.semigroup(s)
}
\keyword{manip}
\keyword{classes}
\keyword{misc}
