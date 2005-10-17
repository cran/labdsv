\name{metrify}
\alias{metrify}
\title{Nearest Metric Space Representation of a Dissimilarity Object}
\description{Calculates the nearest metric space representation of a dissimilarity object by iterating the transitive closure of the triangle inequality rule}
\usage{metrify(x,upper=FALSE,diag=FALSE)
}
\arguments{
    \item{x}{a distance or dissimilarity object returned from \code{\link{dist}}, 
    \code{\link[vegan]{vegdist}}, or \code{\link[labdsv]{dsvdis}}}
    \item{upper}{a logical switch to control whether to return the lower triangle
(upper=FALSE) or upper triangle (upper=TRUE) of the distance matrix} 
    \item{diag}{a logical switch to control whether to return the diagonal of
the distance matrix}
}
\details{Implements a constrained iteration of the transitive closure of
the triangle inequality, such that the distance between any two objects is
less than or equal to the sum of the distances from the two objects to a
third.
}

\value{a object of class \sQuote{dist}}
}
\references{\url{http://ecology.msu.montana.edu/labdsv/R}}
\note{Many multivariate statistical methods are designed for metric spaces,
and yet the direct calculation of distance is often inappropriate due
to problems with joint absences.  metrify takes any dissimilarity matrix and
converts it to the closest metric space representation, generally to avoid negative
eigenvalues in an eigenanalysis of the matrix.}
\author{
    David W. Roberts
    \email{droberts@montana.edu}
    \url{http://ecology.msu.montana.edu/droberts}
}
\seealso{\code{\link[labdsv]{euclidify}}}

}
\examples{
data(bryceveg) # returns a vegetation data.frame
dis.bc <- dsvdis(bryceveg,'bray/curtis') # calculate a Bray/Curtis
            #  dissimilarity matrix
dis.met <- metrify(dis.bc) # calculate the nearest euclidean
            #  representation 
}
\keyword{multivariate}
