\name{dga}
\alias{dga}
\title{Direct Gradient Analysis}
\description{Direct gradient analysis is a graphical representation
of the abundance distribution of (typically) species along opposing environmental
gradients 
}
\usage{dga(z,x,y,step=25,pres="+",abs="-",labcex=1,
    xlab = deparse(substitute(x)), ylab = deparse(substitute(y)),
    pch = 1, title = "", \dots)
}
\arguments{
    \item{z}{the variable (typically a species abundance) to be plotted}
    \item{x}{the variable to use as the x axis}
    \item{y}{the variable to use as the y axis}
    \item{step}{controls the grid density fed to the GAM surface fitter}
    \item{pres}{the symbol to print when a species is present (presence/absence mode)}
    \item{abs}{the symbol to print when a species is absent (presence/absence mode)}
    \item{labcex}{the character size for contour labels}
    \item{xlab}{the x axis legend}
    \item{ylab}{the y axis legend}
    \item{pch}{the symbol to print in continuous abundance plots}
    \item{title}{the title to print} 
    \item{\dots}{miscellaneous arguments to pass to par}
}
\details{
\sQuote{dga} interpolates a grid of x,y values from the supplied data
and fits a GAM (from \code{\link[mgcv]{mgcv}}) of the z variable to the grid.  
The GAM surface is then represented by a contour map and abundance 
symbols as described above.}
}

\value{a graph of the distribution of the z variable on a grid of
x and y is displayed}
}
\references{\url{http://ecology.montana.msu.edu/labdsv}}
\note{
Direct gradient analysis was promoted by Robert Whittaker and
followers as a preferred method of vegetation analysis.
}
\author{
    David W. Roberts
    \email{droberts@montana.edu}
    \url{http://ecology.msu.montana.edu/labdsv}
}
\seealso{\code{\link[mgcv]{gam}}}

}
\examples{
    data(bryceveg) # returns a data.frame called bryceveg
    data(brycesite)
    dga(bryceveg$arcpat,brycesite$elev,brycesite$grorad)
}
\keyword{multivariate}