\name{ordcomp}
\alias{ordcomp}
\title{Ordination to Dissimilarity Comparison}
\description{
Plots the distribution of pair-wise distances of all points in an ordination
to the distances in the dissimilarity or distance matrix the ordination was 
calculated from.  Prints the corrleation betweeen the two on the graph.
}
\usage{ordcomp(x,y,dim=2,xlab="Computed Distance",ylab="Ordination Distance",
            title="",pch=1)
}
\arguments{
    \item{x}{an object of class \code{\link{dist}}}
    \item{y}{an ordination object from \code{\link[labdsv]{pca}}, \code{\link[labdsv]{pco}},
          \code{\link[labdsv]{nmds}}, \code{\link[labdsv]{ca}}, \code{\link[fso]{fso}}}
    \item{dim}{the number of dimensions in the ordination to use (default=2)}
    \item{xlab}{the X axis label for the graph}
    \item{ylab}{the Y axislabel for the graph}
    \item{title}{a title for the  plot}
    \item{pch}{the symbol to plot} 
}

\value{a plot is created on the currnt graphics device
}
\references{\url{http://ecology.montana.msu.edu/labdsv/}}
\note{Ordinations are low dimensional representations of multidimensional spaces.  
This function attempts to portray how well the low dimensional solution approximates the 
full dimesional space.}
\author{
    David W. Roberts
    \email{droberts@montana.edu}
    \url{http://ecology.msu.montana.edu/labdsv}
}
\examples{
data(bryceveg) # produces a vegetation dataframe
dis.bc <- dsvdis(bryceveg,'bray/curtis') # creates a Bray/Curtis dissimilarity matrix
pco.bc <- pco(dis.bc,2) # produces a two-dimensional Principal Coordinates Ordination object
ordcomp(pco.bc,dis.bc)
}
\keyword{multivariate}
