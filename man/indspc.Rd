\name{indspc}
\alias{indspc}
\alias{plot.indspc}
\title{Compositional Indicator Species Analysis}
\description{
Calculates the mean similarity of all plots in which a species occurs.
}
\usage{indspc(taxa, dis, numitr=100)
\method{plot}{indspc}(x, \dots)
}
\arguments{
\item{taxa}{a data frame of taxa, samples as rows, species as columns}
\item{dis}{an object of class \sQuote{dist} from 
    \code{\link[mva]{dist}},
    \code{\link[vegan]{vegist}}, or 
    \code{\link[labdsv]{dsvdis}}}
\item{numitr}{the number of iterations to use to establish the quantiles 
of the distribution}
\item{x}{an object of class indspc}
\item{\dots}{additional arguments to the plot function}
}
\value{a list with two dataframes: vals with species name, mean similarity, and number of occurrences, and quantiles with the distribution of the quantiles}
\note{
One measure of the habitat specificity of a species is the 
degree to which a species only occurs in communities that are
similar to each other.  This function calculates the mean similarity of 
all samples in which each species occurs.  The mean similarity of species
which only occur once is set to 0, rather than NA.

The calculated values can be compared to random samples of plots with the 
same number of occurrences.

}
\references{\url{http://ecology.msu.montana.edu/labdsv/R}}
\author{
        David W. Roberts
        \email{droberts@montana.edu}
        \url{http://ecology.msu.montana.edu/droberts}
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
