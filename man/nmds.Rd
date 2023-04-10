\name{nmds}
\alias{nmds}
\alias{bestnmds}
\title{Nonmetric Multidimensional Scaling}

\description{This function is simply a wrapper for the isoMDS function in the
    MASS package by Venables and Ripley.  The purpose is to convert the output
    to class \sQuote{dsvord} to simplify plotting and additional graphical analysis as
    well as to provide a summary method.}

\usage{
nmds(dis,k=2,y=cmdscale(d=dis,k=k),maxit=50,trace=FALSE)
bestnmds(dis,k=2,itr=20,maxit=100,trace=FALSE,pbar=TRUE)
}

\arguments{
    \item{dis}{a dist object returned from \code{dist} or a full symmetric
        dissimilarity or distance matrix}
    \item{k}{the desired number of dimensions for the result}
    \item{y}{a matrix of initial locations (objects as rows, coordinates as columns,
        as many columns as specified by k).  If none is supplied, \code{cmdscale}
        is used to generate them}
    \item{maxit}{the maximum number of iterations in the isoMDS routine}
    \item{trace}{a switch to control printing intermediate results}
    \item{itr}{number of random starts to find best result}
    \item{pbar}{switch to control printing progress bar in interactive 
               sesssions}
}

\details{The nmds function simply calls the \code{isoMDS} function of the 
    MASS library, but converts the result from a list to an object of
    class \sQuote{dsvord}.  The only purpose for the function is to allow \sQuote{plot},
    \sQuote{identify},
    \sQuote{surf}, and other additional methods to be defined for the 
    class, to simplify the analysis of the result.

    The \sQuote{bestnmds} function runs one run from a PCO solution and
    \sQuote{itr-1} number of
    random initial locations and returns the best result of the set.}

\value{An object of class \sQuote{dsvord}, with components:
    \item{points}{the coordinates of samples along axes}
    \item{stress}{the "goodness-of-fit" computed as stress in percent}
    \item{type}{\sQuote{NMDS}}
}
\references{
Kruskal, J.B. (1964)  Multidimensional scaling by optimizing goodness of fit
to  nonmetric hypothesis.  Psychometrics 29:1-27.

Kruskal, J.B. (1964)  Nonmetric multidimensional scaling:  a numerical method.
Psychometrics 29:115-129.

T.F. Cox and M.A.A. Cox. (1994) \emph{Multidimensional Scaling.} Chapman and Hall.

}

\note{nmds is included as part of the LabDSV package to provide a consistent interface and 
    utility for vegetation ordination methods.  Other analyses included with the same
    interface at present include principal components analysis (pca), principal coordinates
    analysis (pco), and t-distributed neighborhood embedding (t-SNE).}

\author{
    Venables and Ripley for the original isoMDS function included in the MASS package.

    David W. Roberts 
    \email{droberts@montana.edu} 
}

\seealso{
    \code{isoMDS} for the original function

    \code{\link[labdsv]{plot.dsvord}} for the \sQuote{plot} method, the \sQuote{plotid}
        method to identify points with a mouse, the \sQuote{points} method to 
        identify points meeting a logical condition, the \sQuote{hilight} method
        to color-code points according to a factor, 
        the \sQuote{chullord} method to add convex hulls for a factor, or the 
        the \sQuote{surf} method to add surface contours for 
        continuous variables.  

   \code{\link[vegan]{initMDS}} for an alternative way to automate random starts

   \code{\link[vegan]{postMDS}} for a post-solution rescaling

   \code{\link[vegan]{metaMDS}} for a full treatment of variations
}

\examples{
data(bryceveg)
data(brycesite)
dis.man <- dist(bryceveg,method="manhattan")
demo.nmds <- nmds(dis.man,k=4)
plot(demo.nmds)
points(demo.nmds,brycesite$elev>8000)
plotid(demo.nmds,ids=row.names(brycesite))
}
\keyword{multivariate}
