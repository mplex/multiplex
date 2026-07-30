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
  \item{rs}{An object of \dQuote{\code{Rel.System}}, typically with node attributes.}
  \item{classes}{(optional and logical) Should categories of adopters be included in the output?}
  \item{allClasses}{(optional and logical, only for \code{classes}) Include empty classes within the categories of adopters?}
  \item{allNodes}{(optional and logical, only for \code{classes}) Include all actors in the network regardless they are in the chosen system?}
}
\details{
This function generalises the measure of network exposure for multiple networks based on the characteristics of the selected relational system. 
Such a system may represent the entire network or a configuration of strong or weak ties among the actors. 
It is also possible to specify different behaviours of the nodes representing social actors, expressed through the relational system. 
The network exposure measure is calculated on the basis of the immediate neighbours of the reference actor.
}
\value{
\item{Classes}{when \code{classes} is set to \code{TRUE}, the adoption membership for the type of relational system chosen, including isolated actors in the system}
\item{Bonds}{type of bonds of the relational system (cf. \code{\link{rel.sys}})}
\item{Exposure}{the exposure to the attribute(s) for acquisition through immediate neighbour relations}
}
\references{
Ostoic, J.A.R. \dQuote{Creating context for social influence processes in multiplex networks.} \emph{Network Science}, 5(1), 1-29. \cr
Valente, T. W. \emph{Social networks and health}. Oxford University Press. 2010. \cr
Rogers, E. \emph{The Diffusion of Innovations}. 5th ed. (1st ed. 1964) The Free Press. 2003.
}
%\author{} ~
%\note{} ~
%\section{Warning }{....} ~
\seealso{
\code{\link{rel.sys}}, \code{\link{neighb}}, \code{\link{bundles}}
}
\examples{
# create two binary relations among three elements
arr <- round( replace( array(runif(18), c(3,3,2)), array(runif(18),
       c(3,3,2))>.5, 1 ) )

# compute the exposure measure first array in 'arr' is for attributes with adopter categories
arr |> 
  rel.sys(att = 1) |> 
  expos(classes = TRUE)
}
\keyword{attribute}
\keyword{models}
