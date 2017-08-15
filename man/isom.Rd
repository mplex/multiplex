\name{isom}
\alias{isom}
\title{
Make Isomorphic...
}
\description{
Make isomorphic a vector or a data frame object.
}
\usage{
isom(x, uniq = FALSE)
}
\arguments{
  \item{x}{   a vector or a data frame
}
  \item{uniq}{  (logical) whether or not the unique class should be returned
}
}
\details{
This function serves to normalize otherwise arbitrary numbering of a vector or a data frame object data into a progressive ordinal recount of the data in the specified object. This is especially useful to specify more properly the corresponding clustering vector of a set of elements in a given system. 
}
\value{
The input data with isomorphic classes.
%%
In case that \code{uniq} is specified, then a list with the following items is given:
\item{ism}{the input data with isomorphic classes}
\item{uniq}{the unique isomorphic classes}
}
%\references{
%No hay...
%}
\author{
Antonio Rivero Ostoic
}
%\note{
%
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{cngr}}, \code{\link{reduc}}
}
\examples{
## Create a data frame (from 'Vectors')
df <- data.frame(x=1:3, y=5:7)

## Make isomorphic the observations
isom(df)
}
\keyword{ manip }

