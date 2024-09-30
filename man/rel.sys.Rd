\name{rel.sys}
\alias{rel.sys}
\title{
Relational System
}
\description{
Create the Relation System of a multiplex network.
}
\usage{
rel.sys(x, type = c("tolist", "toarray"), bonds = c("entire", "strong", "weak", 
        "asym", "recp", "txch", "tent", "mixd", "full"), loops = FALSE, 
        sel = NULL, att = NULL, sep)
}
\arguments{
  \item{x}{
   an array; usually with three dimensions of stacked matrices where the multiple relations are placed.
}
  \item{type}{
   if the transformation is from 
  \itemize{ 
    \item \code{tolist} for (array of) matrices into lists of pairwise relations
    \item \code{toarray} for lists of pairwise relations into (array of) matrices
  }
 }
  \item{bonds}{
   the type of bonds to be used in the creation of the relational system 
  \itemize{ 
    \item \code{entire} for the \dQuote{entire} network (default, same as \code{full})
    \item \code{strong} for strong bonds
    \item \code{weak} for weak bonds
    \item \code{asym} for asymmetric ties
    \item \code{recp} for reciprocal ties
    \item \code{txch} for tie exchange bundles
    \item \code{tent} for tie entrainment bundles
    \item \code{mixd} for mixed bundles
    \item \code{full} for the whole network (same as \code{entire})
  }
}
  \item{loops}{
   (logical) whether or not the loops should be considered in the relational system
}
  \item{sel}{
   (optional) a set of actors to be selected. For \code{"toarray"} \code{att} and \code{noatt} also supported (see details)
}
  \item{att}{
   (optional) arrays in \code{x} corresponding to attributes
}
  \item{sep}{
   (optional) pair separator used for the pairwise relations
  }
}
\details{
When the type of bonds chosen is \code{entire} then the nodes with ties are considered in the relational system without isolated nodes. 
\code{strong} bonds are relational bundles with a mutual character, whereas \code{weak} bonds are those patterns exclusively without mutual character.

When choosing from a list with actor attributes, it is also possible to select the network members having or \emph{not} having 
the attribute that is specified in the \code{Attrs} output by using in argument \code{sel}. %for the two options \code{att} or \code{noatt}.
}
\value{
For \code{type = "tolist"} (default) option, an object of \sQuote{\code{Rel.System}} class  
where items are:
   \item{ord}{order of network relational system}
   \item{nodes}{nodes in relational system}
   \item{sel}{selected set of actors}
   \item{sys.ord}{order of relational system with chosen bond type}
   \item{incl}{nodes included relational system with chosen bond type}
   \item{excl}{nodes excluded relational system with chosen bond type}
   \item{bond.type}{type of bonds used in relational system creation}
   \item{size}{number of ties in relational system}
   \item{Note}{(if needed) a note}
   \item{sep}{pairwise separator of relational system}
   \item{Ties}{ties in relational system}
   \item{Attrs.ord}{if \code{att} is not \code{NULL}, number of nodes with chosen attribute(s) }
   \item{Attrs}{if \code{att} is not \code{NULL}, actors with chosen attribute(s)}

For \code{type = "toarray"}, the output is a two or three dimensional dichotomous array recording the relations among the actors in the network.
%% ...
}
\references{
Ostoic, J.A.R. \dQuote{Creating context for social influence processes in multiplex networks.} \emph{Network Science}, 5(1), 1-29.
}
\author{
Antonio Rivero Ostoic
}
%\note{
%}

%

\seealso{
\code{\link{expos}}, \code{\link{bundles}}, \code{\link{neighb}}
}
\examples{
# create the data: two binary relations among three elements
arr <- round( replace( array( runif(18), c(3 ,3, 2) ), array( runif(18),
       c(3, 3, 2) ) > .9, 3 ) )

# establish the system of strong bonds
rel.sys(arr, bonds = "strong")

# first array is for attributes
rel.sys(arr, att = 1)

# select the first node
rel.sys(arr, sel = 1)
}

\keyword{models}
\keyword{attribute}

