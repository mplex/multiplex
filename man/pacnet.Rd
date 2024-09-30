\name{pacnet}
\alias{pacnet}
\title{
Import Output from Pacnet
}
\description{
A function to read output files and import from the Pacnet program with full factorization option.
}
\usage{
pacnet(file, toarray = FALSE, uniq = FALSE, transp = FALSE, sep)
}
\arguments{
  \item{file}{  
   character vector containing a file name or path
}
  \item{toarray}{  
   (logical) should the induced inclusions be transformed into arrays?
}
  \item{uniq}{  
   (logical) should only be considered the induced inclusions that are unique?
}
  \item{transp}{  
   (logical) should the partially ordered structures be transposed?
}
  \item{sep}{  
   (optional) pair separator used for the pairwise relations
}
}
\details{
This function is used to read the output file from the Pacnet program, which typically has the \code{.out} extension. 
By default the result is given in a list format, but it is possible to transform the pair lists into arrays. 
Note that the options in the Pacnet program should include the full factorization in the output; otherwise the object will be \code{NULL}.
}
\value{
An object of the \sQuote{\code{Pacnet}} class  with items:
  \item{ii }{induced inclusions}
  \item{at }{atoms}
  \item{mc }{meet complements}
}
\references{
Pattison, P., S. Wasserman, G. Robins and A.M. Kanfer  \dQuote{Statistical Evaluation of Algebraic Constraints for Social Networks,} \emph{Journal of Mathematical Psychology}, 44, 536-568. 2000
}
\author{
Antonio Rivero Ostoic
}
\note{
Currently only partial order structures of order 36 and less are supported.
}
\seealso{
\code{\link{pi.rels}}, \code{\link{cngr}}, \code{\link{decomp}}, \code{\link{write.dat}}
}
%\examples{
%}
\keyword{IO}
\keyword{data}
\keyword{file }
