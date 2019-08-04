\name{pca}
\alias{pca}
\alias{loadings.pca}
\alias{summary.pca}
\alias{scores.pca}
\alias{varplot.pca}
\title{Principal Components Analysis}
\description{Principal components analysis is a eigenanalysis of a
correlation or covariance matrix used to project a high-dimensional
system to fewer dimensions.
}
\usage{
pca(mat, cor = FALSE, dim = min(nrow(mat),ncol(mat)))
\method{summary}{pca}(object, dim = length(object$sdev), \dots)
\method{scores}{pca}(x, labels = NULL, dim = length(x$sdev))
\method{loadings}{pca}(x, dim = length(x$sdev), digits = 3, cutoff = 0.1)
varplot.pca(x, dim=length(x$sdev))
}
\arguments{
    \item{mat}{a matrix or data.frame of interest, samples as rows,
             attributes as columns}
    \item{cor}{logical: whether to use a correlation matrix (if TRUE),
               or covariance matrix (if FALSE)}
    \item{dim}{the number of dimensions to return}
    \item{object}{an object of class \sQuote{pca}}
    \item{x}{an object of class \sQuote{dsvord} and type='pca'}
    \item{labels}{an (optional) vector of labels to identify points}
    \item{digits}{number of digits to report}
    \item{cutoff}{threshold to suppress printing small values}
    \item{\dots}{arguments to pass to function summary}
}
\details{PCA is a common multivariate technique.  The version here is simply
a wrapper for the \code{prcomp} function to make its use and
plotting consistent with the other LabDSV functions.
}

\value{an object of class "pca", a list with components:
    \item{scores}{a matrix of the coordinates of the samples in the reduced space}
    \item{loadings}{a matrix of the contributions of the variables to the axes of
         the reduced space.}
   \item{sdev}{a vector of standard deviations for each dimension}
}
\references{\url{http://ecology.msu.montana.edu/labdsv/R/labs/lab7/lab7.html}}

\note{The current version of pca is based on the \code{prcomp}
    function, as opposed to the \code{princomp} function.  Nonetheless,
    it maintains the more conventional labels "scores" and "loadings",
    rather than x and rotation.  prcomp is based on a
    singular value decomposition algorithm, as has worked better in my
    experience.  In the rare cases where it fails, you may want to try
    \code{princomp}.
}
\author{
    David W. Roberts
    \email{droberts@montana.edu}
    \url{http://ecology.msu.montana.edu/droberts/droberts.html}
}
\seealso{
    \code{princomp}, \code{prcomp},
    \code{\link[labdsv]{pco}}, \code{\link[labdsv]{nmds}},
    \code{\link[fso]{fso}}, \code{\link[vegan]{cca}}

}
\examples{
data(bryceveg) # returns a vegetation data.frame
data(brycesite)
x <- pca(bryceveg,dim=10)  # returns the first 10 eigenvectors 
                           # and loadings
plot(x)
surf(x,brycesite$elev)
points(x,brycesite$depth=='deep')
}
\keyword{multivariate}
