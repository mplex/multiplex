\name{semiring}
\alias{semiring}
\title{
Semiring Structures for Balance Theory
}
\description{
A function to construct semiring structures for the analysis of Structural Balance theory.
}
\usage{
semiring(x, type = c("balance", "cluster"), symclos = TRUE,
         transclos = TRUE, k = 2, lbs)
}
\arguments{
  \item{x}{  an object of a \sQuote{\code{Signed}} class
}
  \item{type}{  
  \itemize{ 
    \item \code{balance} for a balance semiring (default)
    \item \code{cluster} for a cluster semiring
  }
}
  \item{symclos}{
  (logical) apply symmetric closure?
}
  \item{transclos}{
  (logical) apply transitive closure?
}
  \item{k}{
  length of the cycle or the semicycle
}
  \item{lbs}{
  (optional) labels for the semiring output
}
}
\details{
Semiring structures are based on signed networks, and this function provides the capabilities to handle either the balance semiring or the cluster semiring within the Structural Balance theory.

A semiring combines two different kinds of operations with a single underlying set, and it can be seen as an abstract semigroup with identity under multiplication and a commutative monoid under addition. 
Semirings are useful to determine whether a given signed network is balanced or clusterable. 
The symmetric closure evaluates this by looking at semicycles in the system; otherwise, the evaluation is through closed paths. 
}
\value{
An object of \sQuote{\code{Semiring}} class. 
The items included are:
  \item{val}{the valences in the semiring}
  \item{s }{the original semiring structure}
  \item{Q }{the resulted semiring structure}
  \item{k }{the number of cycles or semicycles}
}
\references{
Harary, F, Z. Norman, and D. Cartwright  \emph{Structural Models: An Introduction to the Theory of Directed Graphs}. Wiley. 1965.

Doreian, P., V. Batagelj and A. Ferligoj  \emph{Generalized Blockmodeling}. Cambridge University Press. 2004.

Ostoic, J.A.R. \dQuote{Creating context for social influence processes in multiplex networks.} \emph{Network Science}, 5(1), 1-29.
}
\author{
Antonio Rivero Ostoic
}
\note{
Disabling transitive closure should be made with good substantial reasons.
}
\seealso{
\code{\link{signed}}, \code{\link{as.signed}}
}
\examples{
# create the data: two sets with a pair of binary relations 
# among three elements
arr <- round( replace( array( runif(18), c(3 ,3, 2) ), array( runif(18),
       c(3, 3, 2) ) > .5, 3 ) )

# make the signed matrix with two types of relations
# and establish the semiring structure
signed(arr) |> 
  semiring()

}
\keyword{algebra}
\keyword{math}
\keyword{models}

