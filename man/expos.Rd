\name{expos}
\alias{expos}
\title{
Network Exposure for Multiple Networks
}
\description{
Function to measure the network exposure of the nodes according to a chosen relational system representing the multiple network.
}
\usage{
expos(rs, classes = FALSE, allClasses = FALSE, allNodes = TRUE)
}
\arguments{
  \item{rs}{
   an object of \sQuote{\code{Rel.System}}, typically with node attributes.
}
  \item{classes}{
   (optional) whether or not should be included in the output the categories of adopters 
}
  \item{allClasses}{
   (optional) whether or not to include empty classes within the categories of adopters. Ignored if \code{classes} is \code{FALSE}
}
  \item{allNodes}{
   (optional) whether or not to include all actors in the network regardless they are in the chosen system. Ignored if \code{classes} is \code{FALSE}
}
}
\details{
This is a generalization of the network exposure measure for multiple networks with the characteristics chosen for the representative relational system. 
Such a system can be the entire network or configuration with strong or weak bonds among the actors. 
It is possible to specify different behaviors of the nodes representing social actors, which are indicated in the form of a relational system. 
The network exposure measure is computed according to the immediate neighbours to the reference actor. 
}
\value{
\item{Classes}{
        if option \code{classes} is set to \code{TRUE}, the adoption membership for the type of relational system chosen, including isolated actors in the system. 
}
\item{Bonds}{
        the type of bonds of the relational system (cf. \code{\link{rel.sys}})
}
\item{Exposure}{
        the exposure to the attribute(s) for acquisition through immediate neighbour relations 
}
}
\references{
Ostoic, J.A.R. \dQuote{Creating context for social influence processes in multiplex networks.} \emph{Network Science}, 5(1), 1-29.

Valente, T. W. \emph{Social networks and health}. Oxford University Press. 2010.

Rogers, E. \emph{The Diffusion of Innovations}. 5th ed. (1st ed. 1964) The Free Press. 2003.
}
\author{
Antonio Rivero Ostoic
}
%
\seealso{
\code{\link{rel.sys}}, \code{\link{neighb}}, \code{\link{bundles}}
}
\examples{
# create the data: two binary relations among three elements
arr <- round( replace( array( runif(18), c(3 ,3, 2) ), array( runif(18),
              c(3, 3, 2) ) > .9, 3 ) )

# first array in 'arr' is for attributes
rs <- rel.sys(arr, att = 1)

# compute the exposure measure for an attribute type with adopter categories
expos(rs, classes = TRUE)
}
\keyword{attribute}
\keyword{models}

