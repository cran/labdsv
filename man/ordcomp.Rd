\name{ordcomp}
\alias{ordcomp}
\title{Ordination to Dissimilarity Comparison}
\description{
Plots the distribution of pair-wise distances of all points in an ordination
over the distances in the dissimilarity or distance matrix the ordination was
calculated from.  Prints the correlation between the two on the graph.
}
\usage{ordcomp(x,dis,dim,xlab="Computed Distance",
        ylab="Ordination Distance",title="",pch=1)
}
\arguments{
    \item{x}{an ordination object of class \sQuote{dsvord} 
          from \code{\link[labdsv]{pca}}, \code{\link[labdsv]{pco}},
          \code{\link[labdsv]{nmds}}, \code{\link[fso]{fso}} or \cr
          \code{\link[vegan]{ordiplot}}}
    \item{dis}{an object of class \code{\link{dist}}}
    \item{dim}{the number of dimensions in the ordination to use (default=all)}
    \item{xlab}{the X axis label for the graph}
    \item{ylab}{the Y axis label for the graph}
    \item{title}{a title for the  plot}
    \item{pch}{the symbol to plot}
}

\value{A plot is created on the current graphics device.  Returns the (invisible)
   correlation.
}
\note{Ordinations are low dimensional representations of multidimensional spaces.
This function attempts to portray how well the low dimensional solution approximates the
full dimensional space.}
\references{\url{http://ecology.msu.montana.edu/labdsv/R/labs/lab9/lab9.html}}
\author{
    David W. Roberts
    \email{droberts@montana.edu}
    \url{http://ecology.msu.montana.edu/droberts/droberts.html}
}
\examples{
data(bryceveg) # produces a vegetation data.frame
dis.bc <- dsvdis(bryceveg,'bray/curtis') # creates a Bray/Curtis 
                                         # dissimilarity matrix
pco.bc <- pco(dis.bc,2) # produces a two-dimensional Principal 
                        # Coordinates Ordination object
ordcomp(pco.bc,dis.bc)
}
\keyword{multivariate}
\keyword{hplot}
