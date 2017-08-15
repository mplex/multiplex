\name{incubs}
\docType{data}
\alias{incubs}
\alias{incubA}
\alias{incubB}
\alias{incubC}
\alias{incubD}
\title{
Business Incubators Data Sets
}
\description{
These are four data sets collected in year 2010 (see `source' for the details) of multiple relations between entrepreneurial firms working in business incubators in Denmark.

Each data set contains the adjacency matrices of the three social relations, coded as \code{C}, \code{F}, and \code{K} for working collaboration, informal friendship, and perceived competition among the firms. There are also two actor attributes corresponding to the adoption of a Web innovations at that time by the firms where \code{A} stands for Linkedin and \code{B} for Facebook.

In addition, there is a blockmodel attached to each data set that is product of Compositional Equivalence (cf. \code{\link{cph}}) with transposes for each type of social tie labelled with the following letter in the Latin alphabet; i.e. \code{D} for collaboration, etc.
}
\usage{
data("incubs")
data("incubA")
data("incubB")
data("incubC")
data("incubD")
}
\format{
Each data set is a list with a pair of three dimensioanl arrays.

For \code{incubA}, the dimensions of \code{net} are \eqn{26 \times 26 \times 5}, and of \code{IM} are \eqn{4 \times 4 \times 7} (the two attributes led to the identity matrix).

For \code{incubB}, the dimensions of \code{net} are \eqn{18 \times 18 \times 5}, and of \code{IM} are \eqn{4 \times 4 \times 8}.

For \code{incubC}, the dimensions of \code{net} are \eqn{22 \times 22 \times 5}, and of \code{IM} are \eqn{3 \times 3 \times 8}.

For \code{incubD}, the dimensions of \code{net} are \eqn{15 \times 15 \times 5}, and of \code{IM} are \eqn{4 \times 4 \times 6}.

All four networks are putted in together in \code{incubs}.

}
\source{
Ostoic, J.A.R.  `Algebraic methods for the analysis of multiple social networks and actors attributes'' PhD Thesis. University of Southern Denmark. 2013.
}
%\references{
%}
\keyword{datasets}
