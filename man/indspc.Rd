\name{indspc}
\alias{indspc}
\title{Compositional Indicator Species Analysis}
\description{
Calculates the mean similarity of all plots in which a species occurs.
}
\usage{indspc(veg,dis)}
\arguments{
\item{veg}{a data frame of taxa, samples as rows, species as columns}
\item{dis}{an object of class \sQuote{dist} from 
    \code{\link[mva]{dist}},
    \code{\link[vegan]{vegist}}, or 
    \code{\link[labdsv]{dsvdis}}}
}
\value{a data frame with species name, mean similarity, and number of occurrences}
\note{
One measure of the habitat specificity of a species is the 
idegree to which a species only occurs in communities that are
similar to each other.  This function calculates the mean similarity of 
all samples in which each species occurs.  The mean similarity of species
which only occur once is set as 0, rather than NA.
}
\author{
        David W. Roberts
        \email{droberts@montana.edu}
        \url{http://ecology.msu.montana.edu/labdsv}
}
\seealso{
\code{\link[labdsv]{duleg}},\code{\link[labdsv]{duarm}}
}
\examples{
data(bryceveg) # returns a vegetation dataframe
dis.bc <- dsvdis(bryceveg,'bray/curtis')
    # returns a Bray/Curtis dissimilarity matrix
indspc(bryceveg,dis.bc)
}
\keyword{multivariate}
