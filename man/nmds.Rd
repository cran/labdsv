\name{nmds}
\alias{nmds}
\alias{bestnmds}
\title{Nonmetric Multidimensional Scaling}

\description{This function is simply a wrapper for the isoMDS function in the
    MASS package by Venables and Ripley.  The purpose is to establish a
    nmds class to simplify plotting and additional graphical analysis as
    well as a summary.}

\usage{
nmds(dis,k=2,y=cmdscale(d=dis,k=k),maxit=50)
bestnmds(dis,k=2,itr=20,maxit=100)
}

\arguments{
    \item{dis}{a dist object returned from \code{dist()} or a full symmetric
        dissimilarity or distance matrix}
    \item{k}{the desired number of dimensions for the result}
    \item{y}{a matrix of initial locations (objects as rows, coordinates as columns,
        as many columns as specified by k).  If none is supplied, \code{cmdscale}
        is used to generate them}
    \item{maxit}{the maximum number of iterations}
    \item{itr}{number of random starts to find best result}
}

\details{The nmds function simply calls the \code{isoMDS()} function of the 
    MASS library, but converts the result from a list to an object of
    class "nmds."  The only purpose for the function is to allow \sQuote{plot},
    \sQuote{identify},
    \sQuote{surf}, and other additional methods to be defined for the 
    nmds class, to simplify the analysis of the result.

    The \sQuote{bestnmds} function runs \sQuote{itr} number of
    random initial locations and returns the best result of the set.}

\value{an object of class \sQuote{nmds}, with components:
    \item{points}{the coordinates of samples along axes}
    \item{stress}{the "goodness-of-fit" computed as stress in percent}
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
    interface include principal components analysis (pca), principal coordinates
    analysis (pco), correspondence analysis (ca), canonical correspondence analysis
    (cca), and fuzzy set ordination (fso).}

\author{
    Venables and Ripley for the original isoMDS function

    David W. Roberts \email{droberts@montana.edu} \url{http://ecology.msu.montana.edu/labdsv}
    for the nmds class and interface
}

\seealso{
    \code{\link[labdsv]{isoMDS}} for the original function

    \code{\link[labdsv]{plot.nmds}} for the plot method

    \code{\link[labdsv]{points.nmds}} for a points method

    \code{\link[labdsv]{plotid.nmds}} for a function to identify points in the graphic

    \code{\link[labdsv]{surf.nmds}} for a function to plot contours of independent variables
        on top of the graphic (using the \code{gam} and \code{interp} functions 
        to calculate the surface

    \code{\link[labdsv]{jsurf.nmds}} for a function to plot contours of independent variables
                on top of the graphic (using the \code{gam()} and \code{interp()} functions 
                to calculate the surface.  Jitters the coordinates of points on the axes
        to avoid errors in the embedded \code{interp} function.

   \code{\link[labdsv]{bestnmds}} for a way to automate multiple ramdom starts and save the 
        best result

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
    data(brycesit)
    points(demo.nmds,brycesite$elev>8000)
    plotid(demo.nmds)
}
\keyword{multivariate}
