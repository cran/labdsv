\name{plot.nmds}
\alias{plot.nmds}
\alias{points.nmds}
\alias{plotid.nmds}
\alias{hilight.nmds}
\alias{chullord.nmds}
\alias{surf.nmds}
\alias{density.nmds}
\alias{thull.nmds}
\alias{rgl.nmds}

\title{Plotting Routines For Nonmetric Multi-dimensional Scaling Ordinations}
\description{A set of routines for plotting, highlighting points,
or adding fitted surfaces to NMDSs.}
\usage{
\method{plot}{nmds}(x, ax = 1, ay = 2, col = 1, title = "", pch = 1, \dots)
\method{points}{nmds}(x, which, ax = 1, ay = 2, col = 2, pch = 1, cex = 1, breaks=FALSE, \dots)
\method{plotid}{nmds}(ord, ids = seq(1:nrow(ord$points)), ax = 1, ay = 2,
    col = 1, \dots)
\method{hilight}{nmds}(ord, overlay, ax = 1, ay = 2, title="", cols=c(2,3,4,5,6,7), glyph=c(1,3,5),
    \dots)
\method{chullord}{nmds}(ord, overlay, ax = 1, ay = 2, cols=c(2,3,4,5,6,7), ltys = c(1,2,3), \dots)
\method{surf}{nmds}(ord, var, ax = 1, ay = 2, thinplate = TRUE, col = 2, labcex = 0.8,
    family = gaussian, gamma=1, grid=50, \dots)
\method{thull}{nmds}(ord,var,grain,ax=1,ay=2,col=2,grid=50,nlevels=5,levels=NULL,lty=1,numitr=100,\dots)
\method{density}{nmds}(ord, overlay, ax = 1, ay = 2, cols = c(2, 3, 4, 5,
    6, 7), ltys = c(1, 2, 3), numitr, ...)
rgl.nmds(ord,ax=1,ay=2,az=3,radius=0.01,col=0)
}

\arguments{
    \item{x}{an object of class \sQuote{nmds}}
    \item{ax}{the dimension to use for the X axis}
    \item{ay}{the dimension to use for the Y axis}
    \item{az}{the dimension to use for the third axis}
    \item{title}{a title for the plot}
    \item{which}{a logical variable to specify points to be highlighted}
    \item{breaks}{a logical switch to control using variable glyph sizes in \sQuote{points}}
    \item{ord}{an object of class \sQuote{nmds}}
    \item{overlay}{a factor or integer vector to hilight or distinguish}
    \item{cols}{the sequence of color indices to be used}
    \item{glyph}{the sequence of glyphs (pch) to be used}
    \item{ltys}{the sequence of line types to be used}
    \item{var}{a variable to be surfaced or tension hulled}
    \item{thinplate}{a logical variable to control the fitting routine: thinplate = TRUE
                     (the default) fits a thinplate spline, thinplate = FALSE fits
                      independent smooth splines}
    \item{family}{controls the link function passed to \sQuote{gam}:
          one of \sQuote{gaussian}, \sQuote{binomial}, or \sQuote{poisson}}
    \item{gamma}{controls the smoothness of the fit from \code{\link[mgcv]{gam}}}
    \item{grid}{the number of X and Y values to use in establishing a grid for
           use in surf}
    \item{grain}{the size of cell to use in calculating the tensioned hull}
    \item{nlevels}{the number of contours to draw in representing the tensioned hull}
    \item{lty}{the line type to use in drawing tensioned hull contours}
    \item{ids}{identifier labels for samples.  Defaults to 1:n}
    \item{col}{color index for points or contours}
    \item{labcex}{size of contour interval labels}
    \item{pch}{plot character: glyph to plot}
    \item{cex}{character expansion factor: size of plotted characters}
    \item{numitr}{the number of iterations to use in estimating the probability of the observed density}
    \item{radius}{the radius of the spheres representing points in an rgl plot}
    \item{levels}{specific levels for contours in thull}
    \item{\dots}{arguments to pass to the plot function}
}

\details{
Function \sQuote{plot} produces a scatter plot of sample
scores for the specified axes, erasing or over-plotting
on the current graphic device.  Axes dimensions are controlled to produce
a graph with the correct aspect ratio.
Functions \sQuote{points}, \sQuote{plotid}, and \sQuote{surf}
add detail to an existing plot.  The axes specified must
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

Function \sQuote{density} calculates the fraction of points within the convex
hull that belong to the specified type.

Function \sQuote{surf} calculates and plots fitted surfaces for logical or
quantitative variables.  The function employs the \code{\link[mgcv]{gam}}
function to fit a variable to the ordination coordinates, and to predict the
values at all grid points.  The grid is established with the
\sQuote{expand.grid} function, and the grid is then specified in a call to
\sQuote{predict.gam}.  The predicted values are trimmed to the the convex hull
of the data, and the contours are fit by \sQuote{contour}.  The default link
function for fitting the GAMs is \sQuote{gaussian}, suitable for unbounded
continuous variables.  For logical variables you should specify \sQuote{family
= binomial} to get a logistic GAM, and for integer counts you should specify
\sQuote{family = poisson} to get a Poisson GAM.  

Function \sQuote{thull} calculates a tensioned hull for a specific variable
on the NMDS.  A tensioned hull is a minimum volume container.  The grain size
must be specified as a fraction of the units of the NMDS, with larger values
generating smoother representations, and smaller numbers a more resolved 
container.  \sQuote{thull} returns an invisible object of class \sQuote{thull}
which has an associated plot function.  Plotting the thull object produces a 
colored surface representation of the thull with optional contour lines.
}

\value{Function \sQuote{plotid} returns a vector of row numbers of
identified plots}

\note{Previous versions of surf relied on the \sQuote{interp} function of
package \code{\link[akima]{akima}}.  The revised routine using
\code{\link[mgcv]{predict.gam}} was suggested by Jari Oksanen as used in
\code{\link[vegan]{ordisurf}}.
}


\references{\url{http://ecology..msu.montana.edu/labdsv/R/labdsv}}
\author{
David W. Roberts
\email{droberts@montana.edu}
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
