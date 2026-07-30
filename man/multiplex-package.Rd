\name{multiplex-package}
\alias{multiplex-package}
\alias{multiplex}
\docType{package}
\title{
\packageTitle{multiplex}
}
\description{
\packageDescription{multiplex}
}
\details{
\tabular{ll}{
    Package: \tab multiplex \cr
    Type: \tab Package \cr
    Version: \tab 4.0 \cr
    Date: \tab 29 July 2026 \cr
    License: \tab GPL-3 \cr
}
One aim of the \pkg{multiplex} package is to provide an analytical tool specifically designed for social networks with relationships at multiple levels. 
It includes functions for modelling local role algebras based on the simple and compound relations present in the network. 
\pkg{multiplex} also supports the construction and analysis of signed networks through semiring structures. 
In addition, it can identify dyadic relational patterns that may support further analysis using a range of structural theories.
The package can also incorporate actor attributes into the analysis of multiple networks in different ways. 
For example, it can assess the network exposure of actors in a multiplex context or embed attribute information directly into the resulting algebraic structures.

A typical workflow starts with function \code{\link{semigroup}}, which constructs a semigroup from an array of multiple binary relations. 
The resulting structure can be described with functions \code{\link{wordT}} and \code{\link{edgeT}}. 
Unique relations and associated equations are returned by \code{\link{strings}} function together with the set of equations with strings of length \eqn{k}. 
Ordering among string elements is defined by function \code{\link{partial.order}}, and the corresponding lattice can be displayed with functions \code{\link{hasse}} or \code{\link{diagram}}. 

Further semigroup analysis is available through function \code{\link{green.rel}} for equivalence classes and \code{\link{diagram}} function again for \dQuote{egg-box} visualization. 
Structure reduction of semigroup objects is supported through decomposition based on congruence relations or \eqn{\pi}-relations using functions \code{\link{pi.rels}}, \code{\link{cngr}}, and \code{\link{decomp}}.

Signed networks can be created with function \code{\link{signed}} and analysed with function \code{\link{semiring}} for balance or cluster semiring structures. 

General network analysis utilities in \pkg{multiplex} include function \code{\link{dichot}} for dichotomization of matrices and arrays, 
\code{\link{rm.isol}} for removing isolates, \code{\link{perm}} for permutation operation, and function \code{\link{zbind}} for building three-dimensional arrays.

The package \pkg{multiplex} also provides function \code{\link{rbox}} for Relation-Box construction and function \code{\link{cph}} for Cumulated Person Hierarchy in the network. 
Bundle analysis is supported by functions \code{\link{bundles}}, \code{\link{transf}}, \code{\link{bundle.census}}, and \code{\link{summaryBundles}}. 
Network exposure and relational-system analysis are available through functions \code{\link{rel.sys}} and \code{\link{expos}}, while \code{\link{galois}} supports algebraic analysis of two-mode networks.
Multivariate network data can be imported from send-receive-ties edge lists with function \code{\link{edgel}} and from formats such as \pkg{UCINET} \code{dl} and \pkg{Visone} \code{gml}. 
Multiple network structures can be visualized further with the related package \pkg{multigraph}.
}
\author{
J. Antonio Rivero Ostoic

Maintainer: \packageMaintainer{multiplex}
}
\references{
Pattison, P.E. \emph{Algebraic Models for Social Networks}. Structural Analysis in the Social Sciences. Cambridge University Press. 1993. \cr 
Boyd, J.P. \emph{Social Semigroups. A unified theory of scaling and blockmodelling as applied to social networks}. George Mason University Press. 1991. \cr 
Lorrain, F. and H.C. White, \dQuote{Structural Equivalence of Individuals in Social Networks.} \emph{Journal of Mathematical Sociology}, 1, 49-80. 1971. \cr 
Boorman, S.A. and H.C. White, \dQuote{Social Structure from Multiple Networks. II. Role Structures.} \emph{American Journal of Sociology}, 81 (6), 1384-1446. 1976. \cr 
Ostoic, J.A.R. \emph{Algebraic Analysis of Social Networks}. Wiley Series in Computational and Quantitative Social Sciences. Wiley. 2021. \cr 
}

%% Uncomment below to imitate parts of library(help = multiplex)
%\section{The \file{DESCRIPTION} File}{\packageDESCRIPTION{multiplex}}
%\section{Documentation Index}{\packageIndices{multiplex}}

\seealso{
\code{\link[multigraph:multigraph]{multigraph}}, \code{\link[multigraph:bmgraph]{bmgraph}}, \code{\link[multigraph:ccgraph]{ccgraph}}.
}
\examples{
# create a three-dimensional array
arr <- round( replace( array(runif(18), c(3,3,2)), array(runif(18),
       c(3,3,2))>.5, 3 ) )

# dichotomize array with customized cutoff value
dichot(arr, c = 3)

# construct the semigroup structure and look at its Green relations
semigroup(arr) |> 
  green.rel()

# look at string relations
strings(arr)
}
\keyword{package}
\keyword{models}
\keyword{attribute}
\keyword{manip}
\keyword{IO}
\keyword{datasets}
\keyword{file}
\keyword{misc}
