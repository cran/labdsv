\name{ordpart}
\alias{ordpart}
\title{Ordination Partitioning}
\description{Allows users to produce clusters by visually partitioning
an ordination with the mouse.
}
\usage{
\method{ordpart}{pca}ordpart(pca, ax = 1, ay = 2)
\method{ordpart}{pco}ordpart(pco, ax = 1, ay = 2)
\method{ordpart}{nmds}ordpart(nmds, ax = 1, ay = 2)
}
\arguments{
    \item{pca}{an object of class \sQuote{pca} produced by \code{\link[labdsv]{pca}}}
    \item{pco}{an object of class \sQuote{pco} produced by \code{\link[labdsv]{pco}}}
    \item{nmds}{an object of class \sQuote{nmds} produced by \code{\link[labdsv]{nmds}}}
    \item{ax}{dimension to use for X axis (must match plot)}
    \item{ay}{dimension to use for Y axis (must match plot)}
}
\details{Allows the user to draw an irregular polygon around a set of points
with the mouse to define a cluster.  As many clusters as desired can be defined,
and point membership in each cluster is color-coded as clusters are defined.  
Polygon vertices are drawn with first mouse button; the second mouse button closes
the polygon.  When all clusters are defined the second mouse button ends the routine.
}

\value{
a vector of class memberships with attribute \sQuote{clustering}
}
\note{
The program requires a point-in-polygon algorithm, supplied as a FORTRAN
subroutine
}
\author{
    David W. Roberts
    \email{droberts@montana.edu}
    \url{http://ecology.msu.montana.edu/labdsv}
}
\keyword{cluster}
