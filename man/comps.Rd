\name{comps}
\alias{comps}
\title{
Find Components and Isolates in Multiple Networks
}
\description{
Function to find different components in the multiple network including isolated nodes.
}
\usage{
comps(x, bonds = c("entire", "strong", "weak"), sort)
}
\arguments{
  \item{x}{
  array representing a given network
}
  \item{bonds}{
   type of bonds to be used in the creation of the relational system  for the different components
}
  \item{sort}{
  (optional and logical) sort components by size? Default \code{FALSE}
}
}
\details{
The different components in the network are obtained through the transitive closure of the bundle ties. 
By default, the \emph{entire} system is chosen, but the option \code{bonds} allows choosing different types of relational bundles for the components. 
Argument \code{sort} is for output of components having increasing size; that is (if they exist) dyads, triads, and so on.
}
\value{
A list with two possible system \dQuote{components}
	\item{com}{network components}
	\item{isol}{network isolates}
}
%
\author{
Antonio Rivero Ostoic
}
%
\seealso{
\code{\link{bundles}}, \code{\link{rel.sys}}
}
\examples{
# create the data: two binary relations among three elements
arr <- round( replace( array( runif(18), c(3 ,3, 2) ), array( runif(18),
       c(3, 3, 2) ) > .9, 3 ) )

# find the components and isolates
comps(arr)
}
\keyword{models}
\keyword{cluster}
