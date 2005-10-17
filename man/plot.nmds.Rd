\name{plot.nmds}
\alias{plot.nmds}
\alias{points.nmds}
\alias{plotid.nmds}
\alias{hilight.nmds}
\alias{chullord.nmds}
\alias{surf.nmds}
\alias{jsurf.nmds}

\title{Plotting Routines For Nonmetric Multi-dimensional Scaling Ordinations}
\description{A set of routines for plotting, highlighting points,
or adding fitted surfaces to NMDSs.}
\usage{
\method{plot}{nmds}(x, ax = 1, ay = 2, col = 1, title = "", pch = 1, \dots)
\method{points}{nmds}(x, which, ax = 1, ay = 2, col = 2, pch = 1, cex = 1, \dots)
\method{plotid}{nmds}(ord, ids = seq(1:nrow(ord$points)), ax = 1, ay = 2, 
    col = 1, \dots)
\method{hilight}{nmds}(ord, overlay, ax = 1, ay = 2, cols=c(2,3,4,5,6,7), glyph=c(1,3,5), 
    origpch = 1, blank = '#FFFFFF', \dots)
\method{chullord}{nmds}(ord, overlay, ax = 1, ay = 2, cols=c(2,3,4,5,6,7), ltys = c(1,2,3), \dots)
\method{surf}{nmds}(ord, var, ax = 1, ay = 2, col = 2, labcex = 0.8, 
    family = gaussian, \dots)
\method{jsurf}{nmds}(ord, var, ax = 1, ay = 2, col = 2, labcex = 0.8, 
    family = gaussian, \dots)
}

\arguments{
    \item{x}{an object of class \sQuote{nmds}}
    \item{ax}{the dimension to use for the X axis}
    \item{ay}{the dimension to use for the Y axis}
    \item{title}{a title for the plot}
    \item{which}{a logical variable to specify points to be highlighted}
    \item{ord}{an object of class \sQuote{nmds}}
    \item{overlay}{a factor or integer vector to hilight or distinguish}
    \item{cols}{the sequence of color indices to be used}
    \item{glyph}{the sequence of glyphs (pch) to be used}
    \item{origpch}{the pch number of the glyph employed in the original plot (to be obliterated by blank)}
    \item{blank}{the color to use to erase the glyphs of the original plot}
    \item{ltys}{the sequence of line types to be used}
    \item{var}{a variable to be surfaced}
    \item{family}{controls the link function passed to \sQuote{gam}: 
          one of \sQuote{gaussian}, \sQuote{binomial}, or \sQuote{poisson}}
    \item{ids}{identifier labels for samples.  Defaults to 1:n}
    \item{col}{color index for points or contours}
    \item{labcex}{size of contour interval labels}
    \item{pch}{plot character: glyph to plot}
    \item{cex}{character expansion factor: size of plotted characters}
    \item{\dots}{arguments to pass to the plot function}
}

\details{
Function \sQuote{plot} produces a scatter plot of sample
scores for the specified axes, erasing or over-plotting
on the current graphic device.  Axes dimensions are controlled to produce
a graph with the correct aspect ratio.
Functions \sQuote{points}, \sQuote{plotid}, \sQuote{surf}, and 
\sQuote{jsurf} add detail to an existing plot.  The axes specified must
match the underlying plot exactly.  

Function \sQuote{plotid} identifies 
and labels samples (optionally with values from a third vector) 
in the NMDS, and requires interaction with the mouse: 
left button identifies, right button exits.

Function \sQuote{points} is passed a logical 
vector to identify a set of samples by color of glyph.  It can be used
to identify a single set meeting almost any criterion that can be
stated as a logical expression.

Function \sQuote{hilight} is passed a factor vector or integer vector,
and identifies factor values by color and glyph.

Function \sQuote{chullord} is passed a factor vector or integer vector,
and plots a convex hull around all points in each factor class.
By specifying values for arguments \sQuote{cols} and \sQuote{ltys}
it is possible to control the sequence of colors and linetypes of the convex
hulls.

Functions \sQuote{surf} and \sQuote{jsurf} calculate and plot fitted 
surfaces for logical or quantitative
variables; \sQuote{jsurf} jitters the coordinates to avoid problems with 
identical values in fitting the surface.  The functions are simply
wrappers for the \code{\link[mgcv]{gam}} function, and send the 
sample coordinates and the \sQuote{var} variable 
to \sQuote{gam} to model.  The coordinates and the fitted values from 
\sQuote{gam} are then passed to \code{\link[akima]{interp}}, which plots the 
contours.  The default link function for fitting the GAMs is \sQuote{gaussian}, 
suitable for unbounded continuous variables.  For logical variables you 
should specify \sQuote{family = binomial} to get a logistic GAM, and 
for integer counts you should specify \sQuote{family = poisson} to get 
a Poisson GAM.
}

\value{Function \sQuote{plotid} returns a vector of row numbers of
identified plots}
\references{\url{http://ecology..msu.montana.edu/labdsv/R}}
\author{
David W. Roberts
\email{droberts@montana.edu}
\url{http://ecology.msu.montana.edu/droberts}
}


\examples{
data(bryceveg)
data(brycesite)
dis.bc <- dsvdis(bryceveg,'bray/curtis')
nmds.1 <- nmds(dis.bc,5)
plot(nmds.1)
points(nmds.1,brycesite$elev>8000)
surf(nmds.1,brycesite$elev)
\dontrun{plotid(nmds.1,ids=row.names(bryceveg))}
}
\keyword{hplot}
\keyword{aplot}
\keyword{iplot}
