\name{ordneighbors}
\alias{ordneighbors}
\title{Nearest Neighbors Plotted in Ordination Space} 
\description{
For each sample unit in an ordination, for each of n nearest neighbors, draws
an arrow from the sample unit to its n neighbors.
}
\usage{ordneighbors(ord,dis,numnbr=1,ax=1,ay=2,digits=5,length=0.1)
}
\arguments{
    \item{ord}{an ordination object of class \sQuote{dsvord} 
          from \code{\link[labdsv]{pca}}, \code{\link[labdsv]{pco}},
          \code{\link[labdsv]{nmds}}, \code{\link[fso]{fso}}}
    \item{dis}{an object of class \code{\link{dist}}}
    \item{numnbr}{the number (order) of nearest neighbors to plot}
    \item{ax}{the dimension t plot on the X axis}
    \item{ay}{the dimension to plot on the y axis}
    \item{digits}{the number of digits to report}
    \item{length}{the length of the arrowhead}
}

\value{Additional information is plotted on an existing ordination and
summary information is printed.  Returns an (invisible) list of summary values.
}
\note{Ordinations are low dimensional representations of multidimensional spaces.
This function attempts to portray how well the low dimensional solution 
approximates the neighborhood relations of the 
full dimensional space.

If numnbr = 1 and there are ties the function plots arrows for all tied
values.  If n > 1 the function draws arrows for all values with rank <= n.

}



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
plot(pco.bc)
ordneighbors(pco.bc,dis.bc)
}
\keyword{multivariate}
\keyword{hplot}
