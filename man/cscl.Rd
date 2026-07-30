\name{cscl}
\alias{cscl}
\title{
Conceptual Scaling for Many-valued Formal Contexts
}
\description{
A function to interpret a given many-valued formal context through a chosen scaling process. 
}
\usage{
cscl(x, cl, scale = c("nominal", "ordinal", "intord", "biord", "dichot"), sep)
}
\arguments{
  \item{x}{A table or array representing a many-valued formal context.}
  \item{cl}{A vector with the ordered conceptual scaling-levels in \code{x}.}
  \item{scale}{Type of \emph{scaling} in the interpretation of \code{x}: 
   \itemize{ 
    \item \code{nominal}.
    \item \code{ordinal}.
    \item \code{intord} for one-dimensional interordinal.
    \item \code{biord} for biordinal.
    \item \code{dichot} for dichotomic.
   }
  }
  \item{sep}{(optional, default dot) Pair separator among attributes and levels.}
}
\details{
\emph{Conceptual scaling} is a special form of formal context in which each attribute of a many-valued context is interpreted through a scaling procedure. 
More broadly, it converts many-valued data into dichotomous data, producing a derived context according to the selected type of scaling. 
Conceptual scaling is therefore an interpretive method where both the choice of scale and the transformation of values are form part of the analysis, 
rather than following from a mathematically determined procedure. 

The formal context represented in \code{x} must be associated to its conceptual scaling-levels recorded in \code{cl} as an ordered vector with all levels, 
and a type of scaling derives the context into a dichotomous data frame with attribute ordinal values. 
Once the partial order of the Galois derivation among the objects and attributes of the formal concept has been established, 
the resulting derived context with dichotomous data can be represented as a concept lattice.
}
\value{
A data frame with a \dQuote{\code{conceptual.scaling}} class representing a derived many-valued formal context with respect to the conceptual scaling-levels in \code{cl}. 
The ordered conceptual levels and the pair separator are given in the class attribute. 
}
\references{
Ganter, B. and R. Wille  \emph{Formal Concept Analysis -- Mathematical Foundations}. Springer. 1996.
}
%\author{} ~
%\note{}
%\section{Warning }{....} ~
\seealso{
\code{\link{galois}}, \code{\link{partial.order}}, \code{\link{diagram}}.
}
%\examples{
%
%}
\keyword{algebra}
\keyword{math}
