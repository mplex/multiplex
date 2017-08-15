\name{ltlw}
\alias{ltlw}
\title{
First- and Last Letter Laws
}
\description{
Find the First- and Last Letter Laws in the semigroup of relations.
}
\usage{
ltlw(x)
}
\arguments{
  \item{x}{  an object from the `\code{Semigroup}' class.
}
}
\details{
The First- and Last Letter Laws of the semigroup of relations correspond to the right and left zero of the semigroup, and it has some consequences in the interpretation of the results of the relational structure of the network. This function depends on the semigroup of a (recommended) labeled set of relationships.
}
\value{
  \item{S}{
the semigroup of relations.
}
  \item{strings}{
the strings of relations in the semigroup.
}
  \item{First.Letter}{
the First.Letters in rows and columns
}
  \item{Last.Letter}{
the Last.Letters in rows and columns
}

}
\references{
Lorrain, F. \emph{Réseaux sociaux et classifications sociales}. Hermann, Paris. 1975.
}
\author{
Antonio Rivero Ostoic
}
\note{
Since this function is based on the semigroup construction, see the Warning section and look at other details in the \code{\link{semigroup}} function section.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{semigroup}}, \code{\link{strings}}.
}
\examples{
## Create the data: 3 binary relations among 3 elements
arr <- round( replace( array(runif(27), c(3,3,3)), array(runif(27),
       c(3,3,3))>.75, 1 ) )

## Put labels
dimnames(arr)[[3]] <- list("n","m","ñ")

## Construct the semigroup
S <- semigroup(arr, type="symbolic")

## Find the Letter Laws
ltlw(S)
}
\keyword{ algebra }
\keyword{ math }

