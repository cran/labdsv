\name{tsne}
\alias{tsne}
\alias{besttsne}
\title{t-Distributed Stochastic Neighbor Embedding}

\description{This function is a wrapper for the Rtsne function in the
    Rtsne package by Krijthe and van der Maaten.  The purpose is to convert the output
    to class \sQuote{dsvord} to simplify plotting and additional graphical analysis as
    well as to provide a summary method.}

\usage{
tsne(dis,k=2,perplexity=30,theta= 0.0,eta=200)
besttsne(dis,k=2,itr=100,perplexity=30,theta=0.0,eta = 200,pbar=TRUE)
}

\arguments{
    \item{dis}{a dist object returned from \code{dist} or a full symmetric
        dissimilarity or distance matrix}
    \item{k}{the desired number of dimensions for the result}
    \item{perplexity}{neighborhood size parameter (should be less than
                     (size(dis)-1) /3}
    \item{theta}{Speed/accuracy trade-off; set to 0.0 for exact TSNE, 
          (0,0,0.5] for increasing speeed (default: 0.0)}
    \item{eta}{Learning rate}
    \item{itr}{number of random starts to find best result}
    \item{pbar}{switch to control printing progress bar in interactive sessions}
}

\details{The tsne function simply calls the \code{Rtsne} function of the 
    Rtsne package with a specified distance/dissimilarity matrix rather
    than the community matrix.  By convention, t-SNE employs a PCA on the 
    input data matrix, and calculates distances among the first 50 eigenvectors
    of the PCA.  Rtsne, however, allows the submission of a pre-calculated 
    distance/dissimilarity matrix in place of the PCA.  Given the long history of 
    research into the use of PCA in ecological community analysis, tsne allows 
    the simple use of any of a vast number of distance/dissimilarity matrices
    known to work better with ecological data.

    In addition, the tsne function converts the output to an object of class 
    \sQuote{dsvord} to simplify plotting and analyses using the many functions 
    defined for objects of class \sQuote{dsvord}.  (see 
    \code{\link[labdsv]{plot.dsvord}} for more details.)

    The \sQuote{besttsne} function runs one run from a PCO solution as the initial
    configuration and 
    \sQuote{itr-1} number of
    random initial locations and returns the best result of the set.}

\value{an object of class \sQuote{dsvord}, with components:
    \item{points}{the coordinates of samples along axes}
    \item{type}{\sQuote{t-SNE}}
}
\references{     
     van der Maaten, L. 2014. Accelerating t-SNE using Tree-Based
     Algorithms. Journal of Machine Learning Research, 15, p.3221-3245.

     van der Maaten, L.J.P. & Hinton, G.E., 2008. Visualizing
     High-Dimensional Data Using t-SNE. Journal of Machine Learning
     Research, 9, pp.2579-2605.

  Krijthe, J,H,  2015. Rtsne: T-Distributed Stochastic Neighbor
  Embedding using a Barnes-Hut Implementation, URL:
  https://github.com/jkrijthe/Rtsne


}

\note{tsne is included as part of the LabDSV package to provide a consistent interface and 
    utility for ecological community ordination methods.  Other analyses included with the same
    interface at present include nonmetric multidimensional scaling (NMDS),
    principal components analysis (pca), and principal coordinates
    analysis (pco).}

\author{
    Jesse H. Krijthe for the original Rtsne R code, adapted from C++ code from 
    Laurens van der Maaten. 

    David W. Roberts 
    \email{droberts@montana.edu} 
}

\seealso{
    \code{\link[Rtsne]{Rtsne}} for the original function

    \code{\link[labdsv]{plot.dsvord}} for the \sQuote{plot} method, the \sQuote{plotid}
        method to identify points with a mouse, the \sQuote{points} method to 
        identify points meeting a logical condition, the \sQuote{hilight} method
        to color-code points according to a factor, 
        the \sQuote{chullord} method to add convex hulls for a factor, or the 
        the \sQuote{surf} method to add surface contours for 
        continuous variables.  

}

\examples{
data(bryceveg)
data(brycesite)
dis.man <- dist(bryceveg,method="manhattan")
demo.tsne <- tsne(dis.man,k=2)
plot(demo.tsne)
points(demo.tsne,brycesite$elev>8000)
plotid(demo.tsne,ids=row.names(brycesite))
}
\keyword{multivariate}
