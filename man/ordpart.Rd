\name{ordpart}
\alias{ordpart}
\alias{ordpart.pca}
\alias{ordpart.pco}
\alias{ordpart.nmds}
\title{Ordination Partitioning}
\description{This function allows users to partition or classify the points
in an ordination by identifying clusters of points with a mouse}

\usage{ordpart(ord, ax = 1, ay = 2)
}
\arguments{
	\item{ord}{an ordination of class \sQuote{pca}, \sQuote{pco}, or \sQuote{nmds}}
	\item{ax}{the first axis number in the ordination plot}
        \item{ay}{the second axis number in the ordination plot}
}
\details{
Given a plot of an ordination, you assign plots to clusters by drawing
a polygon with the first mouse button to include all points in a given
cluster.  To end that cluster, click the right mouse button to close
the polygon.  Plots included in that cluster will be color-coded to
indicate membership.  Start the
next cluster by drawing another polygon.  To end, click the right
mouse button again after closing the last polygon.  Plots within more than
one polygon are assigned membership in the last polygon which includes them;
plots which are not within any polygon are assigned membership in cluster
zero.
}

\value{A integer vector of cluster membership values
}
\references{\url{http://ecology.msu.montana.edu/labdsv/R/labdsv}}
\note{Although the routine could easily be adapted for any scatter plot,
it is currently only designed for objects of class \sQuote{pca},
\sQuote{pco}, or \sQuote{nmds}}
\author{
	David W. Roberts
	\email{droberts@montana.edu}
}
\examples{
data(bryceveg)
data(brycesite)
dis.bc <- dsvdis(bryceveg,'bray/curtis')
nmds.1 <- nmds(dis.bc,5)
plot(nmds.1)
\dontrun{clustering <- ordpart(nmds.1)}
}
\keyword{hplot}
\keyword{aplot}
\keyword{iplot}
\keyword{cluster}

