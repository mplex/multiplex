\name{rbox}
\alias{rbox}
\title{
Construct the Relation-Box
}
\description{
Function to construct the Relation-Box of a multiple network
}
\usage{
rbox(w, transp = FALSE, smpl = FALSE, k = 3, tlbs)
}
\arguments{
  \item{w}{
   an array with three dimensions of stacked matrices of generating relations.
}
  \item{transp}{
   (logical) whether or not the transpose of each matrix in \eqn{w} should be included.
}
  \item{smpl}{
   (logical) whether to simplify or not the strings of relations
}
  \item{k}{
   length of the Relation-Box in \eqn{z}
}
  \item{tlbs}{
   (optional) a vector with the labels for the transpose relations.
}
}
\details{
If \code{transp = TRUE} the labels of the transpose are toggle case of the labels of the original matrices, and in such case, it is advised to simplify the strings of relations. 
To prevent a transposed structure for a certain array of \code{w}, use \code{NA} in the vector the transpose labels \code{tlbs} corresponding to the respective matrix.
}
\value{
An object of the \sQuote{\code{Rel.Box}} class.
  \item{w}{the primitive relations in the Relation-Box}
  \item{W}{the structure of the Relation-Box}
  \item{lbs}{the labels in the relational system}
  \item{Note}{(optional) Notes indicating the particularities in the input}
  \item{Orels}{the original labels of the relations}
  \item{Srels}{(optional) the simplified labels of the relations}
  \item{Trels}{(optional) the labels of the transposed relations}
  \item{k}{the maximal length of the word}
  \item{z}{the length of the Relation-Box in the \eqn{z} dimension}
}
\references{
Winship, C. and M.J. Mandel \dQuote{Roles and positions: A critique and extension of the blockmodelling approach,} \emph{Sociological Methodology}, 314-344. 1983.
}
\author{
Antonio Rivero Ostoic
}
\note{
Values of \code{k} until \eqn{9} are supported. 
With many types of relations, and when the order of the multiplex network is high, turning \code{k} to more than three may take a long time of computation.
}
%\Warning{}
\seealso{
 \code{\link{cph}}, \code{\link{semigroup}}, \code{\link{hierar}}
}
\examples{
# load the data
data("incubA")

# relation box of image matrices in dataset
\dontrun{
rbox(incubA$IM)
}
}
%
\keyword{algebra}
%\keyword{math}
\keyword{array}
