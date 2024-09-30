\name{multiplex-package}
\alias{multiplex-package}
%\alias{multiplex}
\docType{package}
\title{
Algebraic Tools for the Analysis of Multiple Social Networks
}
\description{
One of the aims of the \code{"multiplex"} package is to meet the necessity to count with an analytic tool specially designed for social networks with relations at different levels. 
In this sense, \code{"multiplex"} counts with functions that models the local role algebras of the network based on the simple and compound relations existing in the system.
\code{"multiplex"} has also a procedure for the construction and analysis of signed networks through the semiring structure. 
With \code{"multiplex"}, the different relational patterns at the dyadic level in the network can be obtained as well, which can serve for a further analysis with different types of structural theories.

It is also possible to take the attributes of the actors in the analysis of multiple networks with different forms to incorporate this kind of information to the existing relational structures. 
For instance, the network exposure of the actors can be taken in the context of multiple networks in this case, or else the attributes can be embedded in the resulted algebraic structures.
}
\details{
\tabular{ll}{
Package: \tab multiplex \cr
Type: \tab Package \cr
Version: \tab 3.7 \cr
Date: \tab 30 September 2024 \cr
License: \tab GPL-3 \cr
LazyLoad: \tab yes \cr
}
One way to work with this package is typically by starting with a specific algebraic structure like a semigroup that is a closed system made of a set of elements and an associative operation on it. 
This algebraic structure is constructed by the \code{semigroup} function, and it takes an array of (usually but not necessarily) multiple binary relations, which constitute the generator relations. 
The Word Table and the Edge Table serve to describe completely the semigroup, and they are constructed with the functions \code{wordT} and \code{edgeT} respectively. 
Unique relations of the complete semigroup are given by the \code{strings} function together with the set of equations with strings of length \eqn{k}. 
The \code{partial.order} function specifies the ordering of the string elements in the semigroup, and the function \code{hasse} (or function \code{diagram} with this type) 
produces the lattice of inclusions of a structure having ordered relations.

Semigroups can be analysed further by the \code{green.rel} function, and their found equivalence classes can be visualized as \dQuote{\code{egg-box}} type with the \code{diagram} function. 
Semigroups can be reduced as well with a decomposition process, which can be based on congruence or \eqn{\pi}-relations of the unique strings. 
In this case \code{pi.rels}, \code{cngr}, and \code{decomp} will make this job for you either for an abstract or a partially ordered structure.

In addition, it is possible to analyse structural balance in signed networks, which are built by \code{signed}, through the algebraic structure of the semiring. 
A semiring is an algebraic structure that combines an abstract semigroup with identity under multiplication and a commutative monoid under addition. 
The \code{semiring} function is capable to perform both balance and cluster semiring either with cycles or semicycles.

There are other capabilities in the package that are not strictly algebraic. 
For instance, the \code{dichot} serves to dichotomize the input data with a specified cut-off value, \code{rm.isol} removes isolated nodes, and the \code{perm} function performs an automorphism of the elements in the representative array. 
All these functions are built for multiple networks represented by high dimensional structures that can be constructed by function \code{zbind} to produce three-dimensional arrays. 

Furthermore, \code{"multiplex"} creates a Relation-Box with the \code{rbox} function, and it implements the Compositional Equivalence expressed in the cumulated person hierarchy of the network computed with the \code{cph} function.

Relational bundles are identified through the \code{bundles} function, which provides lists of pair relations. 
The \code{transf} function serves to transform pairwise list data into a matrix form and viceversa. 
The enumeration of the different bundle classes is given by \code{bundle.census}, while \code{summaryBundles} prints the bundle class patterns results. 
An advantage of counting with the bundle patterns is that the different types of bundles serve to establish a system inside the network, in which it is possible to measure the network exposure in multivariate relational systems. 
Such features can be realized via the \code{rel.sys} and \code{expos} functions, respectively. 
Several attributes can be derived by \code{galois}, which provides an algebraic approach for the analysis of two-mode networks.

Finally, multivariate network data can be created using a \emph{s}end \emph{r}eceive \emph{t}ies edge list format that can be loaded and transformed to arrays through the \code{edgel} function. 
Other formats for multiple network data like \bold{UCINET} \code{dl} or \bold{Visone} \code{gml} can be imported and exported as well with the \code{"multiplex"} package. 
Visualization of multiple network structures is possible with the \code{"multigraph"} package that depends on \code{"multiplex"}. 
}
%
\author{
J. Antonio Rivero Ostoic

Maintainer: Antonio Rivero Ostoic <multiplex@post.com>
}
\references{
Pattison, P.E. \emph{Algebraic Models for Social Networks}. Structural Analysis in the Social Sciences. Cambridge University Press. 1993.

Boyd, J.P. \emph{Social Semigroups. A unified theory of scaling and blockmodelling as applied to social networks}. George Mason University Press. 1991.

Lorrain, F. and H.C. White, \dQuote{Structural Equivalence of Individuals in Social Networks.} \emph{Journal of Mathematical Sociology}, 1, 49-80. 1971.

Boorman, S.A. and H.C. White, \dQuote{Social Structure from Multiple Networks. II. Role Structures.} \emph{American Journal of Sociology}, 81 (6), 1384-1446. 1976.

Ostoic, J.A.R. \emph{Algebraic Analysis of Social Networks}. Wiley Series in Computational and Quantitative Social Sciences. Wiley. 2021.
}

\seealso{
\code{\link[multigraph:multigraph]{multigraph}}, \code{\link[multigraph:bmgraph]{bmgraph}}, \code{\link[multigraph:ccgraph]{ccgraph}}.
}

\examples{
# create the data: two binary relations among three elements
arr <- round( replace( array(runif(18), c(3,3,2)), array(runif(18),
       c(3,3,2))>.5, 3 ) )

# dichotomize it with customized cutoff value
dichot(arr, c = 3)

# preview
prev(arr)

# create the semigroup and look at Green's relations
semigroup(arr) |> 
  green.rel()

# look at string relations
strings(arr)
}
\keyword{models}
\keyword{attribute}
\keyword{manip}
\keyword{IO}
\keyword{data}
\keyword{file }
