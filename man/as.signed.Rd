\name{as.signed}
\alias{as.signed}
\title{
Coerce into a Signed Object
}
\description{
A generic function for coercing a matrix into a \dQuote{\code{Signed}} class object.
}
\usage{
as.signed(x, lbs)
}
\arguments{
  \item{x}{A matrix representing the signed network.}
  \item{lbs}{(optional) Labels for the signed matrix.}
}
\details{
Use this function to convert an array representing a signed network into an \R object of 
class \dQuote{\code{Signed}}, as required by function \code{\link{semiring}}.

The array in \code{x} itself can be also a \dQuote{\code{Semiring}} class object represented as 
\sQuote{\code{Rel.Q}} attribute for the semiring relational structure. 
}
\value{
The input array in \code{x} as a \dQuote{\code{Signed}} class.
}
%\references{} ~
%\author{} ~
%\note{} ~
%\section{Warning }{....} ~
\seealso{
\code{\link{signed}}, \code{\link{semiring}}
}
\examples{
# load a network dataset
data("incubA")

# coerce parts of the signed matrix with two types of relations
signed(incubA$IM)$s[1:2,1:2] |> 
  as.signed()
}
\keyword{manip}
\keyword{classes}
\keyword{misc}
