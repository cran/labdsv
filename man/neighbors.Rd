\name{neighbors}
\alias{neighbors}
\title{Neighbors}
\description{Calculates the nearest neighbors in a distance/dissimilarity
matrix}

\usage{neighbors(dis,numnbr)
}
\arguments{
    \item{dis}{an object of class \sQuote{dist} such as returned by
    \code{\link{dist}},
    \code{\link[vegan]{vegdist}} or \code{\link[labdsv]{dsvdis}}}
    \item{numnbr}{the number (order) of neighbors to return}
}
\details{For each sample unit in a dissimilarity matrix finds the 
\sQuote{numnbr} nearest neighbors and returns them in order.
}

\value{Returns a data.frame with sample units as rows and neighbors as columns,
listed in order of proximity to the sample unit.
}
\author{
    David W. Roberts
    \email{droberts@montana.edu}
}
\examples{
data(bryceveg) # returns a data.frame called veg
dis.bc <- dsvdis(bryceveg,'bray/curtis')
neighbors(dis.bc,5)
}
\keyword{multivariate}
