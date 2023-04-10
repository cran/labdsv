\name{rnddist}
\alias{rnddist}
\title{Random Distance}
\usage{rnddist(size, method='metric', sat = 1.0, upper=FALSE, 
       diag=FALSE)}
\description{Calculates a random distance matrix for use in null
   model analysis.}
\arguments{
\item{size}{the number of items to calculate the distances for}
\item{method}{the desired properties of the matrix.  Must be either
\sQuote{metric} or \sQuote{euclidean}}
\item{sat}{a saturation coefficient to set an upper limit less than 1.0
that truncates maximum values to simulate a dissimilarity rather than a distance}
\item{upper}{logical: whether to print the upper triangle (default=FALSE)}
\item{diag}{logical: whether to print the diagonal (default=FALSE)}
}
\value{A dissimilarity object of class \sQuote{dist}}
\details{Generates a matrix of \eqn{size^2} uniform random numbers
   and passes the matrix to \code{\link[labdsv]{metrify}} or
   \code{\link[labdsv]{euclidify}} to ensure the metric or
   euclidean properties of the distances.  Values are normalized
   to a maximum of 1.0.
}
\author{
   David W. Roberts
   \email{droberts@montana.edu}
}
\seealso{\code{\link[labdsv]{metrify}}, \code{\link[labdsv]{euclidify}}}
\examples{
x <- rnddist(100)
pco.x <- pco(x)
plot(pco.x)
}
\keyword{datagen}
