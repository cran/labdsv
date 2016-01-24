\name{compspec}
\alias{compspec}
\alias{indspc}
\alias{plot.compspec}
\title{Compositional Specificity Analysis}
\description{
Calculates the mean similarity of all plots in which a species occurs
}
\usage{compspec(taxa, dis, numitr=100, drop=FALSE, progress=FALSE)
\method{plot}{compspec}(x,spc=NULL,\dots)
}
\arguments{
\item{taxa}{a data frame of taxa, samples as rows, species as columns}
\item{dis}{an object of class \sQuote{dist} from 
    \code{dist},
    \code{\link[vegan]{vegdist}}, or
    \code{\link[labdsv]{dsvdis}}}
\item{numitr}{the number of iterations to use to establish the quantiles
of the distribution}
\item{drop}{a switch to determine whether to drop species out when calculating
their compspec value}
\item{progress}{a switch to control printing out progress}
\item{x}{an object of class compspec}
\item{spc}{an integer code to specify exactly which species drop-out to plot}
\item{\dots}{additional arguments to the plot function}
}
\value{a list with several data.frames: \sQuote{vals} with species name, mean similarity, 
number of occurrences, and probaability of observing as high a mean similarity as observed,
and \sQuote{quantiles} with the distribution of the quantiles of mean similarity for 
given numbers of occurrences.
If drop=TRUE, results specific to dropping out each species in turn are added to the list
by taxon name.}


\note{
One measure of the habitat specificity of a species is the
degree to which a species only occurs in communities that are
similar to each other.  This function calculates the mean similarity of
all samples in which each species occurs, and compares that value to 
the distribution of mean similarities for randomly generated sets of the same size.  
The mean similarity of species which only occur once is set to 0, rather than NA.

If drop=TRUE each species is deleted in turn and a new dissimilarity matrix minus that
species is calculated for the analysis.  This eliminates the bias that part of the similarity 
of communities being anayzed is due to the known joint occurrence of the species being analyzed.


}
\references{\url{http://ecology.msu.montana.edu/labdsv/R}}
\author{
        David W. Roberts
        \email{droberts@montana.edu}
}
\seealso{
\code{indval},\code{isamic}
}
\examples{
data(bryceveg) # returns a vegetation data.frame
dis.bc <- dsvdis(bryceveg,'bray/curtis')
    # returns a Bray/Curtis dissimilarity matrix
compspec(bryceveg,dis.bc)
}
\keyword{multivariate}
