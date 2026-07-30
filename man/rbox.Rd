\name{rbox}
\alias{rbox}
\title{
Construct the Relation-Box of Multiple Network
}
\description{
Function to construct the Relation-Box of a multiple network.
}
\usage{
rbox(w, transp = FALSE, smpl = FALSE, k = 3, tlbs)
}
\arguments{
  \item{w}{Array with three dimensions of stacked matrices with generating relations.}
  \item{transp}{(optional and logical) Include matrix transpose in \eqn{w}?
}
  \item{smpl}{(optional and logical) Simplify strings of relations?}
  \item{k}{Length of Relation-Box (in \eqn{z}).}
  \item{tlbs}{(optional) Vector with labels for transpose relations.}
}
\details{
If \code{transp = TRUE} the labels of the transpose are toggle case of the labels of the original matrices, and in such case, it is advised to simplify the strings of relations. 
To prevent a transposed structure for a certain array of \code{w}, use \code{NA} in the vector the transpose labels \code{tlbs} corresponding to the respective matrix.

Values of \code{k} until \eqn{9} are supported. 
}
\value{
An object of the \sQuote{\code{Rel.Box}} class.
  \item{w}{Primitive relations in Relation-Box}
  \item{W}{Structure of Relation-Box}
  \item{lbs}{Labels in relational system}
  \item{Note}{(optional) Notes indicating the particularities in the input}
  \item{Orels}{Original labels of relations}
  \item{Srels}{(optional) Simplified labels of relations}
  \item{Trels}{(optional) Transposed relation labels}
  \item{k}{Maximal length of the word}
  \item{z}{Length of the Relation-Box in the \eqn{z} dimension}
}
\references{
Winship, C. and M.J. Mandel \dQuote{Roles and positions: A critique and extension of the blockmodelling approach,} \emph{Sociological Methodology}, 314-344. 1983.
}
%\author{} ~
%\note{} ~
\section{Warning}{
May take a long time of computation with many types of relations and when the order of the multiplex network is high, and turning \code{k} to more than three.
}
\seealso{
 \code{\link{cph}}, \code{\link{semigroup}}, \code{\link{hierar}}
}
\examples{
# load the data
data("incubA")

# relation box of image matrices in dataset
\dontrun{
incubA |> getElement("IM") |> 
  rbox()
}
}
%
\keyword{algebra}
%\keyword{math}
\keyword{array}
