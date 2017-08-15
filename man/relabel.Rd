\name{relabel}
\alias{relabel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Relabel a Semigroup Object
}
\description{
Function to relabel the elements of a `\code{Semigroup}' class object.
}
\usage{
relabel(S, lbs = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{S}{
   an object from the `\code{Semigroup}' class
}
  \item{lbs}{
   the new labels of the semigroup
}
}
\details{
Basically this is a function to change the labels of the strings in the multiplication table represented by an object of the `\code{Semigroup}' class. Usually this is a consequence of the equations existing in the relational structure.
}
\value{
A `\code{Semigroup}' object with the semigroup with the new labels
}
%\references{
%
%}
\author{
Antonio Rivero Ostoic
}
\note{
For the set of equations in the relational structure see the \code{\link{strings}} function.
}
\seealso{
\code{\link{semigroup}}, \code{\link{as.semigroup}}, \code{\link{convert}}, \code{\link{strings}}
}
\examples{
## Create the multiplication table
s <- matrix(data=c(1, 1, 1, 3, 3, 3, 3, 3, 3), nrow=3, ncol=3, byrow=TRUE)

## make a semigroup object with labels
S <- as.semigroup(s, lbs=c('n', 'm', 'ñ'))

## convert to a symbolic form a semigroup with relabeled strings
convert(relabel(S, lbs=c('a', 'b', 'c')))

}
\keyword{ manip }

