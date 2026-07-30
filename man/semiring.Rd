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
  \item{x}{A \sQuote{\code{Signed}} class object.}
  \item{type}{  
  \itemize{ 
   Semiring type: 
    \item \code{balance} semiring (default)
    \item \code{cluster} semiring
  }}
  \item{symclos}{(logical) Apply symmetric closure?}
  \item{transclos}{(logical) Apply transitive closure?}
  \item{k}{Length of cycle or semicycle.}
  \item{lbs}{(optional) Labels for semiring to return.}
}
\details{
Semiring structures are based on signed networks, and this function provides the capabilities to handle either the balance semiring or the cluster semiring within the Structural Balance theory.

A semiring combines two different kinds of operations with a single underlying set, and it can be seen as an abstract semigroup with identity under multiplication and a commutative monoid under addition. 
Semirings are useful to determine whether a given signed network is balanced or clusterable. 
The symmetric closure evaluates the assessment by looking at semicycles in the system; otherwise, the evaluation is through closed paths. 
}
\value{
A \sQuote{\code{Semiring}} class object with items:
  \item{val}{Valences in the semiring}
  \item{s}{Original semiring structure}
  \item{Q}{Resulted semiring structure}
  \item{k}{Number of cycles or semicycles}
}
\references{
Harary, F, Z. Norman, and D. Cartwright  \emph{Structural Models: An Introduction to the Theory of Directed Graphs}. Wiley. 1965. \cr
Doreian, P., V. Batagelj and A. Ferligoj  \emph{Generalized Blockmodeling}. Cambridge University Press. 2004. \cr
Ostoic, J.A.R. \dQuote{Creating context for social influence processes in multiplex networks.} \emph{Network Science}, 5(1), 1-29.
}
%\author{} ~
%\note{} ~
\section{Warning}{
Disabling transitive closure should be made with good substantial reasons.
}
\seealso{
\code{\link{signed}}, \code{\link{as.signed}}
}
\examples{
# create two binary relations among three elements
arr <- round( replace( array( runif(18), c(3 ,3, 2) ), array( runif(18),
       c(3, 3, 2) ) > .9, 3 ) )

# create signed matrix with the two types of relations
# and establish the semiring structure
signed(arr) |> 
  semiring()
}
\keyword{algebra}
\keyword{math}
\keyword{models}
