\name{compspec}
\alias{compspec}
\alias{indspc}
\alias{plot.compspec}
\title{Compositional Specificity Analysis}
\description{
Calculates the mean similarity of all plots in which each species occurs
}
\usage{compspec(comm, dis, numitr=100, drop=FALSE, progress=FALSE)
\method{plot}{compspec}(x,spc=NULL,pch=1,type='p',col=1,\dots)
}
\arguments{
\item{comm}{a data frame of community samples, samples as rows, species as columns}
\item{dis}{an object of class \sQuote{dist} from 
    \code{dist},
    \code{\link[labdsv]{dsvdis}} or
    \code{\link[vegan]{vegdist}}}
\item{numitr}{the number of iterations to use to establish the quantiles
of the distribution}
\item{drop}{a switch to determine whether to drop species out when calculating
their compspec value}
\item{progress}{a switch to control printing out a progress bar}
\item{x}{an object of class compspec}
\item{spc}{an integer code to specify exactly which species drop-out to plot}
\item{pch}{which glyph to plot for species}
\item{type}{which type of plot}
\item{col}{an integer or integer vector) to color the points}
\item{\dots}{additional arguments to the plot function}
}
\value{a list with several data.frames: \sQuote{vals} with species name, mean similarity, 
number of occurrences, and probability of observing as high a mean similarity as observed,
and \sQuote{quantiles} with the distribution of the quantiles of mean similarity for 
given numbers of occurrences.
If drop=TRUE, results specific to dropping out each species in turn are added to the list
by species name.}


\note{
One measure of the habitat specificity of a species is the
degree to which a species only occurs in communities that are
similar to each other.  This function calculates the mean similarity of
all samples in which each species occurs, and compares that value to 
the distribution of mean similarities for randomly generated sets of the same size.  
The mean similarity of species which only occur once is set to 0, rather than NA.

If drop=TRUE each species is deleted in turn and a new dissimilarity matrix minus that
species is calculated for the analysis.  This eliminates the bias that part of the similarity 
of communities being analyzed is due to the known joint occurrence of the species being analyzed.


}
\references{\url{http://ecology.msu.montana.edu/labdsv/R}}
\author{
        David W. Roberts
        \email{droberts@montana.edu}
        \url{http://ecology.msu.montana.edu/droberts/droberts.html}
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
