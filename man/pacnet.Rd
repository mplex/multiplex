\name{pacnet}
\alias{pacnet}
\title{
Import Log-Report from Pacnet
}
\description{
A function to read output files and import from the \pkg{Pacnet} program with full factorization option.
}
\usage{
pacnet(file, toarray = FALSE, uniq = FALSE, transp = FALSE, sep)
}
\arguments{
  \item{file}{Path with filename of Log-Report.}
  \item{toarray}{(logical) Transform induced inclusions into arrays?}
  \item{uniq}{(logical) Consider only unique induced inclusions?}
  \item{transp}{(logical) Transpose partial order tables?}
  \item{sep}{(optional) Pair separator in pairwise relations.}
}
\details{
This function is used to read the output file from the \pkg{Pacnet} program, which typically has the \code{.out} extension. 
By default the result is given in a list format, but it is possible to transform the pair lists into arrays. 

Note that the options in the \pkg{Pacnet} program should include the full factorization in the output; otherwise the object will be \code{NULL}.
Also note that only partial order structures of order 36 and less are currently supported.
}
\value{
A \sQuote{\code{Pacnet}} class object with items:
  \item{ii}{: Induced inclusions}
  \item{at}{: Atoms}
  \item{mc}{: Meet complements}
}
\references{
Pattison, P, S Wasserman, G Robins and AM Kanfer \dQuote{Statistical Evaluation of Algebraic Constraints for Social Networks,} \emph{Journal of Mathematical Psychology}, 44, 536-568. 2000
}
%\author{} ~
%\note{} ~
%\section{Warning }{....} ~
\seealso{
\code{\link{pi.rels}}, \code{\link{cngr}}, \code{\link{decomp}}, \code{\link{write.dat}}
}
%\examples{
%}
\keyword{IO}
\keyword{data}
\keyword{file}
