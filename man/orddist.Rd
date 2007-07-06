\name{orddist}
\alias{orddist}
\title{Ordination Point Pair-Wise Distance Calculation}
\description{
Calculates the pair-wise distances of all points in an ordination.
The function is simply a
wrapper for the \sQuote{dist} function, but simplifies managing ordinations
that store their coordinates under different names, as well as managing
the desired dimensionality of the calculations.}

\usage{orddist(x,dim)}

\arguments{
    \item{x}{an ordination object from \code{\link[labdsv]{pca}},
    \code{\link[labdsv]{pco}}, \code{\link[labdsv]{nmds}},
    \code{\link[fso]{fso}}}
    \item{dim}{the desired dimensionality to be included in the calculations
               (must be <= number of dimensions of the ordinations)}
}

\value{an object of class \sQuote{dist} is produced
}
\references{\url{http://ecology.msu.montana.edu/labdsv/R/labdsv}}
\note{Ordinations are low dimensional representations of multidimensional
spaces.  This function produces data on the low-dimensional distances for
other analyses.}
\author{
    David W. Roberts
    \email{droberts@montana.edu}
}
\examples{
data(bryceveg) # produces a vegetation data.frame
dis.bc <- dsvdis(bryceveg,'bray/curtis') # creates a Bray/Curtis dissimilarity matrix
pco.bc <- pco(dis.bc,2) # produces a two-dimensional Principal Coordinates Ordination object
orddist(pco.bc,dim=2)
}
\keyword{multivariate}
\keyword{hplot}
