\name{comps}
\alias{comps}
\title{
Find components in multiple networks
}
\description{
Function to find different components in the multiple network including isolates
}
\usage{
comps(x, bonds = c("entire", "strong", "weak"))
}
\arguments{
  \item{x}{
  array representing the network
}
  \item{bonds}{
   the type of bonds to be used in the creation of the relational system  for the different components
}
}
\details{
The network's different components are obtained through the transitive closure of the bundle ties. By default, the ``entire'' system is chosen, but the option \code{bonds} allow discriminating different types of relational bundles for the components. 
}
\value{
A list with two possible ``components''
\item{com}{a component}
\item{isol}{the isolates}
%% ...
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Antonio Rivero Ostoic
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{bundles}}, \code{\link{rel.sys}}
}
\examples{
## Create the data: two binary relations among three elements
arr <- round( replace( array( runif(18), c(3 ,3, 2) ), array( runif(18),
       c(3, 3, 2) ) > .9, 3 ) )

## Find components ans isolates
comps(arr)

}
\keyword{ models }
\keyword{cluster}
