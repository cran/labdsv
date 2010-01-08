\name{pco}
\alias{pco}
\title{Principal Coordinates Analysis}
\description{Principal coordinates analysis is an eigenanalysis of
    distance or metric dissimilarity matrices.
}
\usage{pco(dis, k=2)
}
\arguments{
    \item{dis}{the distance or dissimilarity matrix object of
         class "dist" returned from
        \code{dist}, \code{\link[vegan]{vegdist}}, or
        \code{\link[labdsv]{dsvdis}}}
    \item{k}{the number of dimensions to return}
}
\details{pco is simply a wrapper for the \code{cmdscale} function
     of Venebles and Ripley to make plotting of the function similar to
     other LabDSV functions
}

\value{an object of class \sQuote{pco} with components:
\item{points}{the coordinates of samples on eigenvectors}
}
\references{Gower, J.C. (1966)  Some distance properties of latent root
and vector methods used in multivariate analysis.  Biometrika 53:325-328.

\url{http://ecology.msu.montana.edu/labdsv/R/labdsv}}

\note{Principal Coordinates Analysis was pioneered by Gower (1966)
    as an alternative to PCA better suited to ecological datasets.}
\author{
of the \sQuote{cmdscale} function: Venebles and Ripley

of the wrapper function
    David W. Roberts
    \email{droberts@montana.edu}
}
\seealso{\code{cmdscale}, \code{\link[labdsv]{pca}},
          \code{\link[labdsv]{nmds}}, \code{\link[vegan]{cca}}
}
\examples{
        data(bryceveg) # returns a vegetation data.frame
        dis.bc <- dsvdis(bryceveg,'bray/curtis')
                  # returns an object of class \sQuote{dist}
        veg.pco <- pco(dis.bc,k=4) # returns first 4 dimensions
}
\keyword{multivariate}
