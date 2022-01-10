\name{multiplex-package}
\alias{multiplex-package}
%\alias{multiplex}
\docType{package}
\title{
Algebraic Tools for the Analysis of Multiple Social Networks
}
\description{
One of the aims of the \CRANpkg{multiplex} package is to meet the necessity to count with an analytic tool specially designed for social networks with relations at different levels. In this sense, \CRANpkg{multiplex} relies on functions to model the local role algebras of the network based on simple and compound relations existing in the system. It also provides a procedure for the construction and analysis of signed networks through the semiring structure, and it is possible to obtain the different relational patterns at the dyadic level in the system, which can serve for further analysis with diverse types of structural theories. 

In conjunction with the \CRANpkg{multigraph} package, it is possible to visualize multiplex, multimodal, and multilevel structures as graphs or valued graphs. 
}
\details{
\tabular{ll}{
Package: \tab multiplex \cr
Type: \tab Package \cr
Version: \tab 2.9.7 \cr
Date: \tab 10 January 2022 \cr
License: \tab GPL-3 \cr
LazyLoad: \tab yes \cr
}
To work with \CRANpkg{multiplex}, we typically start with a specific algebraic structure. A \emph{semigroup} is a closed system made of a set of elements and an associative operation on it. 
The \code{semigroup} function constructs this algebraic structure, and it takes an array of (usually but not necessarily) multiple binary relations, which are the generator relations. The Word Table and the Edge Table serve to describe the semigroup completely, and they are constructed with the functions \code{wordT} and \code{edgeT}, respectively. 
The \code{strings} function gives unique relations of the complete semigroup and the \code{partial.order} function specifies the ordering of the string elements in the semigroup. For the visualization of the partial order structure, the function \code{diagram} produces the lattice of inclusions of a structure having ordered relations.

Different forms of decomposition that allow reducing semigroups such as factorization or finding congruence classes by substitution and the decomposition is based on congruence with the function \code{cngr} or \eqn{\pi}-relations of the unique strings given by \code{fact} or imported from \emph{Pacnet}. In these two cases, \code{pi.rels}, and \code{decomp} will do this job for reducing either for an abstract or a partially ordered semigroup structure. 

It is also possible to take the attributes of the actors in the analysis of multiple networks with different forms to incorporate this kind of information to the existing relational structures. In this case, for example, the network exposure of the actors is in the context of multiple networks, or else the resulted algebraic structures can embed the actor attributes.  

In addition, it is possible to analyze structural balance in signed networks, which are built by function \code{signed}, through the algebraic structure of the semiring. A semiring is an algebraic structure that combines an abstract semigroup with identity under multiplication and a commutative monoid under addition. The \code{semiring} function is capable of performing both balance and cluster semiring either with cycles or with semicycles. 

Other capabilities of \CRANpkg{multiplex} are not strictly algebraic. For instance, the \code{dichot} function serves to dichotomize the input data with a specified cut-off value, \code{rm.isol} removes isolated nodes, and the \code{perm} function performs an automorphism of the elements in the representative array. All these functions are built for multiple networks represented by high dimensional structures that can be constructed by the function \code{zbind}.

The \CRANpkg{multiplex} package creates a Relation-Box with the \code{rbox} function to implement the Partial Structural or Compositional Equivalence expressed in the cumulated person hierarchy of the system calculated via the \code{cph} function. It is from this structure that the partition of multiple networks is possible by counting the multiplicity of their ties.

Relational bundles are identified through the \code{bundles} function, which provides lists of pair relations. The \code{transf} function serves to transform such data into a matrix form. The enumeration of the different bundle classes is given by \code{bundle.census}. An advantage of counting with the bundle patterns is that the different types of bundles serve to establish a system inside the network, in which it is possible to measure the network exposure in multivariate relational systems. Such features can be realized via the \code{rel.sys} and \code{expos} functions, respectively. Several attributes can be derived by \code{galois}, which provides an algebraic approach for two-mode networks.

Finally, multivariate network data can be created through the (s)end (r)eceive (t)ies format that can be loaded and transformed via the \code{read.srt} function. Other formats for multiple network data like \emph{Ucinet} \code{dl} or \emph{Visone} \code{gml} can be imported and exported as well with the \CRANpkg{multiplex} package.
}
%
\author{
J. Antonio Rivero Ostoic

Maintainer: Antonio Rivero Ostoic <multiplex@post.com>
}
\references{
Pattison, Philippa E. \emph{Algebraic Models for Social Networks}. Cambridge University Press. 1993.

Boyd, John P. \emph{Social Semigroups. A unified theory of scaling and blockmodelling as applied to social networks}. George Mason University Press. 1991.

Lorrain, François and Harrison C. White, `Structural Equivalence of Individuals in Social Networks.' \emph{Journal of Mathematical Sociology}, 1, 49-80. 1971.

Boorman, Scott A. and Harrison C. White, `Social Structure from Multiple Networks. II. Role Structures.' \emph{American Journal of Sociology}, 81 (6), 1384-1446. 1976.

Ostoic, J.A.R. `Algebraic Analysis of Multiple Social Networks with \CRANpkg{multiplex}.' \emph{Journal of Statistical Software}, 91(11), 1-41. <doi:10.18637/jss.v092.i11>
}

\seealso{
\code{\link[multigraph:multigraph]{multigraph}}
}

\examples{
## Create the data: two binary relations among three elements
arr <- round( replace( array(runif(18), c(3,3,2)), array(runif(18),
       c(3,3,2))>.5, 3 ) )

## Dichotomize it with customized cutoff value
dichot(arr, c = 3)

## preview
prev(arr)

## create the semigroup (elay...)
semigroup(arr)

## and look at the strings
strings(arr)

}
\keyword{ models }
\keyword{ attribute }
\keyword{ manip }
\keyword{ IO }
\keyword{ data }
\keyword{ file }
