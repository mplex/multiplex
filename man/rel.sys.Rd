\name{rel.sys}
\alias{rel.sys}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Relational System of Network
}
\description{
Function to create a Relational System of a multiple network.
}
\usage{
rel.sys(x, type = c("tolist", "toarray"), bonds = c("entire", "strong", "weak", 
        "asym", "recp", "txch", "tent", "mixd", "full"), loops = FALSE, 
        sel = NULL, att = NULL, sep)
}
\arguments{
  \item{x}{Array with three dimensions of stacked matrices with generating relations.}
  \item{type}{Is the transformation about: 
  \itemize{ 
    \item \code{tolist} (Array of) matrices into lists of pairwise relations.
    \item \code{toarray} Lists of pairwise relations into (array of) matrices.
  } }
  \item{bonds}{
   the type of bonds to be used in the creation of the relational system 
  \itemize{ 
    \item \code{entire} The \dQuote{entire} network (default, same as \code{full})
    \item \code{strong} Strong bonds
    \item \code{weak} Weak bonds
    \item \code{asym} Asymmetric ties
    \item \code{recp} Reciprocal ties
    \item \code{txch} Tie exchange bundles
    \item \code{tent} Tie entrainment bundles
    \item \code{mixd} Mixed bundles
    \item \code{full} Whole network (same as \code{entire})
  } }
  \item{loops}{(optional and logical) Include loops in relational system.}
  \item{sel}{(optional) Vector with set of actors to select.}
  \item{att}{(optional) Arrays in \code{x} corresponding to attributes.}
  \item{sep}{(optional) Pair separator for pairwise relations.}
}
\details{
A relational system of network \code{x} is a configuration with a particular pattern like the bond type among members. 
For instance, when the type of bonds chosen is \code{entire} then the nodes with ties are considered in the relational system without isolated nodes. 
Besides, \code{strong} bonds are relational bundles with a mutual character, whereas \code{weak} bonds are those patterns exclusively without mutual character. 

It is also possible to select the network members having or *not* having the attribute that is specified in the \code{Attrs} output by using in argument \code{sel}. 
This is possible when choosing from a list with actor attributes. 
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
}
\references{
Ostoic, J.A.R. \dQuote{Creating context for social influence processes in multiplex networks.} \emph{Network Science}, 5(1), 1-29.
}
%\author{} ~
%\note{} ~
%\section{Warning }{....} ~
\seealso{
\code{\link{expos}}, \code{\link{bundles}}, \code{\link{neighb}}
}
\examples{
# create two binary relations among three elements
arr <- round( replace( array( runif(18), c(3 ,3, 2) ), array( runif(18),
       c(3, 3, 2) ) > .9, 3 ) )

# establish system of strong bonds
arr |> rel.sys(bonds = "strong")

# first array is for attributes
arr |> rel.sys(att = 1)

# select first node
arr |> rel.sys(sel = 1)
}
\keyword{models}
\keyword{attribute}

