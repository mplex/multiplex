\name{strings}
\alias{strings}
\title{
Strings of Relations
}
\description{
Function to get the labels of the unique relations of the semigroup, or the generators and compound relations that are the elements of the complete semigroup.
}
\usage{
strings(x, equat = FALSE, k = 2, smpl, valued)
}
\arguments{
  \item{x}{
   an array; usually with three dimensions of stacked matrices where the multiple relations are placed.
}
  \item{equat}{
   (logical) should the equations be included in the output?
}
  \item{k}{
   length of the strings in the equations
}
  \item{smpl}{
   (optional and logical) whether to simplify or not the string relations
}
  \item{valued}{
   (logical) whether the strings are with a valued format
  }
}
\details{
The strings are the unique relations, which constitute the elements of the complete semigroup. 
These are both the generators and the compound relations after applying the Axiom of Quality, which means that even some generators can be disregarded.

This function is especially useful to construct the partial order of relations and to establish the set of equations in the relational structure.
}
\value{
An object of \sQuote{\code{Strings}} class.
  \item{wt}{
  the generators and compound relations
  }
  \item{ord}{
  the order of the structure
  }
  \item{st}{
  the labels of the unique relations
  }
  \item{equat}{
  the equations among strings of relations
  }
}
\references{
Boorman, S.A. and H.C. White, \dQuote{Social Structure from Multiple Networks. II. Role Structures.} \emph{American Journal of Sociology}, 81 (6), 1384-1446. 1976.
}
\author{
Antonio Rivero Ostoic
}
\note{
The maximum length of the strings in the equations is currently \eqn{4}.
}
%
\seealso{
\code{\link{partial.order}}, \code{\link{semigroup}}.
}
\examples{
# create the data: two binary relations among three elements
arr <- round( replace( array(runif(18), c(3,3,2)), array(runif(18),
       c(3,3,2))>.5, 1 ) )

# get the strings
strings(arr)
}
\keyword{algebra}
\keyword{array}
