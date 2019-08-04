\name{euclidify}
\alias{euclidify}
\alias{as.euclidean}
\title{Nearest Euclidean Space Representation of a Dissimilarity Object}
\description{Calculates the nearest Euclidean space representation of a 
dissimilarity object by iterating the transitive closure of the
triangle inequality}

\usage{
euclidify(dis,upper=FALSE,diag=FALSE)
as.euclidean(dis,upper=FALSE,diag=FALSE)
}

\arguments{
    \item{dis}{a distance or dissimilarity object returned from \code{\link{dist}},
    \code{\link[vegan]{vegdist}}, or \code{\link[labdsv]{dsvdis}}}
    \item{upper}{a logical switch to control whether to return the lower triangle
(upper=FALSE) or upper triangle (upper=TRUE) of the distance matrix}
    \item{diag}{a logical switch to control whether to return the diagonal of
the distance matrix}
}
\details{Implements a constrained iteration of the transitive closure of
Pythagoras' theorem, such that the squared distance between any two objects is
less than or equal to the sum of the squared distances from the two objects to all possible
third objects.
}

\value{An object of class \sQuote{dist}}

\note{Many multivariate statistical methods are designed for euclidean spaces,
and yet the direct calculation of euclidean distance is often inappropriate due
to problems with joint absences.  euclidify takes any dissimilarity matrix and
converts it to the closest euclidean representation, generally to avoid negative
eigenvalues in an eigenanalysis of the matrix.}
\author{
    David W. Roberts
    \email{droberts@montana.edu}
    \url{http://ecology.msu.montana.edu/droberts/droberts.html}
}
\seealso{\code{\link[labdsv]{metrify}}

}
\examples{
data(bryceveg) # returns a vegetation data.frame
dis.bc <- dsvdis(bryceveg,'bray/curtis') # calculate a Bray/Curtis
                                         # dissimilarity matrix
dis.euc <- euclidify(dis.bc) # calculate the nearest euclidean 
                             # representation 
\dontrun{plot(dis.bc,dis.euc)}
}
\keyword{multivariate}
