\name{incubs}
\docType{data}
\alias{incubs}
\alias{incubA}
\alias{incubB}
\alias{incubC}
\alias{incubD}
\alias{incA}
\alias{incB}
\alias{incC}
\alias{incD}
\title{
Incubator Networks Datasets
}
\description{
Four data sets collected in year 2010 (see \sQuote{source} for the details) of multiple relations between entrepreneurial firms working in business incubators in Denmark.

Each data set contains the adjacency matrices of the three social relations, coded as \code{C}, \code{F}, and \code{K} for working collaboration, informal friendship, and perceived competition among the firms. 
There are also a pair of actor attributes corresponding to the adoption of two Web innovations in the year 2010 by the firms where \code{A} stands for Linkedin and \code{B} for Facebook websites.

In addition, there is a blockmodel attached to each data set that is a product of Compositional Equivalence (cf. \code{\link{cph}}) with transposes for each type of social tie labelled with the following letter in the Latin alphabet; 
i.e. \code{D} for collaboration, \code{G} for friendship, and \code{L} for perceived competition.
}
\usage{
data("incubs")
data("incubA")
data("incubB")
data("incubC")
data("incubD")
data("incA")
data("incB")
data("incC")
data("incD")
}
\format{
Each data set is a list with a pair of three-dimensional arrays.

For \code{incubA}, the dimensions of \code{net} are \eqn{26 \times 26 \times 5}, and of \code{IM} are \eqn{4 \times 4 \times 7}. 
In this case, the two attributes led to the identity matrix.

For \code{incubB}, the dimensions of \code{net} are \eqn{18 \times 18 \times 5}, and of \code{IM} are \eqn{4 \times 4 \times 8}.

For \code{incubC}, the dimensions of \code{net} are \eqn{22 \times 22 \times 5}, and of \code{IM} are \eqn{3 \times 3 \times 8}.

For \code{incubD}, the dimensions of \code{net} are \eqn{15 \times 15 \times 5}, and of \code{IM} are \eqn{4 \times 4 \times 6}.

All four network datasets are gather together in object \code{incubs}.

To plot automatically actor attributes in the graph with function \code{\link[multigraph:multigraph]{multigraph}}, 
another version of these data sets are given in \code{incA}, \code{incB}, \code{incC}, and \code{incD}, which are \dQuote{\code{Data.Set}} objects class having:
  \itemize{ 
    \item \code{net} for the network data
    \item \code{atnet} a vector that indicates whether or not the arrays in \sQuote{\code{net}} is attribute data
    \item \code{IM} for the Image Matrices of the reduced network data
    \item \code{atIM} a vector that indicates whether or not the array in \sQuote{\code{IM}} is attribute data
    \item \code{cite} relational content of the ties
  }
}
\source{
Ostoic, J.A.R. \dQuote{Algebraic methods for the analysis of multiple social networks and actors attributes.} PhD Thesis. University of Southern Denmark. 2013.
}
%\references{
%}
\keyword{datasets}
