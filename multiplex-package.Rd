\name{multiplex-package}
\alias{multiplex-package}
%\alias{multiplex}
\docType{package}
\title{
Algebraic Tools for the Analysis of Multiple Social Networks
}
\description{
One of the aims of the \code{"multiplex"} package is to meet the necessity to count with an analytic tool specially designed for social networks with relations at different levels. In this sense, \code{"multiplex"} counts with functions that models the local role algebras of the network based on the simple and compound relations existing in the system, and also a procedure for the construction and analysis of signed networks through the semiring structure. The different relational patterns at the dyadic level in the network can be obtained as well, which can serve for a further analysis with different types of structural theories.

It is also possible to take the attributes of the actors in the analysis of multiple networks with different forms to incorporate this kind of information to the existing relational structures. In this case for example the network exposure of the actors can be taken in the context of multiple networks, or else the attributes can be embedded in the resulted algebraic structures.
}
\details{
\tabular{ll}{
Package: \tab multiplex \cr
Type: \tab Package \cr
Version: \tab 3.1.0-2 \cr
Date: \tab 10 July 2023 \cr
License: \tab GPL-3 \cr
LazyLoad: \tab yes \cr
}
To work with this package we typically start with a specific algebraic structure. A semigroup is a closed system made of a set of elements and an associative operation on it. This algebraic structure is constructed by the \code{semigroup} function, and it takes an array of (usually but not necessarily) multiple binary relations, which are the generator relations. The Word Table and the Edge Table serve to describe completely the semigroup, and they are constructed with the functions \code{wordT} and \code{edgeT} respectively. Unique relations of the complete semigroup are given by the \code{strings} function. The \code{partial.order} function specifies the ordering of the string elements in the semigroup. The function \code{diagram} produces the lattice of inclusions of a structure having ordered relations.

Semigroups can be analysed further by the \code{green.rel} function, and semigroups can also be reduced by a decomposition process. The decomposition is based on congruence or \eqn{\pi}-relations of the unique strings imported from Pacnet. In this case \code{pi.rels}, \code{cngr}, and \code{decomp} will make this job for you either for an abstract or a partially ordered structure.

In addition, it is possible to analyse structural balance in signed networks, which are built by \code{signed}, through the algebraic structure of the semiring. A semiring is an algebraic structure that combines an abstract semigroup with identity under multiplication and a commutative monoid under addition. The \code{semiring} function is capable to perform both balance and cluster semiring either with cycles or semicycles.

There are other capabilities in the package that are not strictly algebraic. For instance, the \code{dichot} serves to dichotomize the input data with a specified cut-off value, \code{rm.isol} removes isolated nodes, and the \code{perm} function performs an automorphism of the elements in the representative array. All these functions are built for multiple networks represented by high dimensional structures that can be constructed by the function \code{zbind}.

The \code{"multiplex"} package creates a Relation-Box with the \code{rbox} function, and it implements the Partial Structural Equivalence expressed in the cumulated person hierarchy of the network calculated via the \code{cph} function.

Relational bundles are identified through the \code{bundles} function, which provides lists of pair relations. The \code{transf} function serves to transform such data into a matrix form. The enumeration of the different bundle classes is given by \code{bundle.census}. An advantage of counting with the bundle patterns is that the different types of bundles serve to establish a system inside the network, in which it is possible to measure the network exposure in multivariate relational systems. Such features can be realized via the \code{rel.sys} and \code{expos} functions respectively. Several attributes can be derived by \code{galois}, which provides an algebraic approach for two-mode networks.

Finally, multivariate network data can be created through the (s)end (r)eceive (t)ies edge list format that can be loaded and transformed to arrays via the \code{edgel} function. Other formats for multiple network data like UCINET \code{dl} or Visone \code{gml} can be imported and exported as well with the \code{"multiplex"} package.
}
%
\author{
J. Antonio Rivero Ostoic

Maintainer: Antonio Rivero Ostoic <multiplex@post.com>
}
\references{
Pattison, P.E. \emph{Algebraic Models for Social Networks}. Cambridge University Press. 1993.

Boyd, J.P. \emph{Social Semigroups. A unified theory of scaling and blockmodelling as applied to social networks}. George Mason University Press. 1991.

Lorrain, F. and H.C. White, ``Structural Equivalence of Individuals in Social Networks.'' \emph{Journal of Mathematical Sociology}, 1, 49-80. 1971.

Boorman, S.A. and H.C. White, ``Social Structure from Multiple Networks. II. Role Structures.'' \emph{American Journal of Sociology}, 81 (6), 1384-1446. 1976.

Ostoic, J.A.R. \emph{Algebraic Analysis of Social Networks}. Wiley. 2021
}

\seealso{
\code{\link[multigraph:multigraph]{multigraph}}, \code{\link[multigraph:bmgraph]{bmgraph}}
}

\examples{
## Create the data: two binary relations among three elements
arr <- round( replace( array(runif(18), c(3,3,2)), array(runif(18),
       c(3,3,2))>.5, 3 ) )

## Dichotomize it with customized cutoff value
dichot(arr, c = 3)

## preview
prev(arr)

## create the semigroup and look at Green's relations
semigroup(arr) |> green.rel()

## and look at the strings
strings(arr)

}
\keyword{ models }
\keyword{ attribute }
\keyword{ manip }
\keyword{ IO }
\keyword{ data }
\keyword{ file }
