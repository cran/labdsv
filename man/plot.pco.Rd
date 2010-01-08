\name{plot.pco}
\alias{plot.pco}
\alias{points.pco}
\alias{plotid.pco}
\alias{hilight.pco}
\alias{chullord.pco}
\alias{density.pco}
\alias{surf.pco}
\alias{thull.pco}

\title{Plotting Routines For Principal Coordinates Ordinations}
\description{A set of routines for plotting, highlighting points,
or adding fitted surfaces to PCOs.}
\usage{
\method{plot}{pco}(x, ax = 1, ay = 2, col = 1, title = "", pch = 1, \dots)
\method{points}{pco}(x, which, ax = 1, ay = 2, col = 2, pch = 1, cex = 1, \dots)
\method{plotid}{pco}(ord, ids = seq(1:nrow(ord$points)), ax = 1, ay = 2,
    col = 1, \dots)
\method{hilight}{pco}(ord, overlay, ax = 1, ay = 2, title="", cols=c(2,3,4,5,6,7), glyph=c(1,3,5),
    \dots)
\method{chullord}{pco}(ord, overlay, ax = 1, ay = 2, cols=c(2,3,4,5,6,7), ltys = c(1,2,3), \dots)
\method{density}{pco}(ord, overlay, ax = 1, ay = 2, cols = c(2, 3, 4, 5,
    6, 7), ltys = c(1, 2, 3), numitr=100, ...)
\method{surf}{pco}(ord, var, ax = 1, ay = 2, thinplate=TRUE, col = 2, labcex = 0.8,
    family = gaussian, grid=50, gamma=1, \dots)
\method{thull}{pco}(ord,var,grain,ax=1,ay=2,col=2,grid=50,nlevels=5,levels=NULL,lty=1,numitr=100,...)
}

\arguments{
    \item{x}{an object of class \sQuote{pco}}
    \item{ax}{the dimension to use for the X axis}
    \item{ay}{the dimension to use for the Y axis}
    \item{title}{a title for the plot}
    \item{which}{a logical variable to specify points to be highlighted}
    \item{ord}{an object of class \sQuote{pco}}
    \item{overlay}{a factor or integer vector to hilight or distinguish}
    \item{cols}{the sequence of color indices to be used}
    \item{glyph}{the sequence of glyphs (pch) to be ed}
    \item{ltys}{the sequence of line types to be used}
    \item{numitr}{number of iterations to use in estimating the probability of obtaining
           the observed density}
    \item{var}{a variable to be surfaced}
    \item{thinplate}{a logical variable to control how the surface is fit: thinplate
                     = TRUE (the default) fits a thinplate spline, thinplate = FALSE
                     fits independent smooth splines}
    \item{family}{controls the link function passed to \sQuote{gam}:
          one of \sQuote{gaussian}, \sQuote{binomial}, or \sQuote{poisson}}
    \item{gamma}{controls the smoothness of the fit from \code{\link[mgcv]{gam}}}
    \item{grid}{the number of X and Y points to use in establishing a grid
          for surf}
    \item{ids}{identifier labels for samples.  Defaults to 1:n}
    \item{col}{color index for points or contours}
    \item{labcex}{size of contour interval labels}
    \item{pch}{plot character: glyph to plot}
    \item{cex}{character expansion factor: size of plotted characters}
    \item{grain}{the size of moving window to use in calculating the tensioned hull}
    \item{nlevels}{the number of contour intervals to draw on the tensioned hull}
    \item{levels}{specific levels to use in drawing the tensioned hull}
    \item{lty}{the line type to use in drawing the tensioned hull contours}
    \item{\dots}{arguments to pass to the plot function}
}

\details{
Function \sQuote{plot} produces a scatterplot of sample
scores for the specified axes, erasing or over-plotting
on the current graphic device.  Axes dimensions are controlled to produce
a graph with the correct aspect ratio.
Functions \sQuote{points}, \sQuote{plotid}, \sQuote{hilight},
\sQuote{chullord}, and \sQuote{surf}
add detail to an existing plot.  The axes specified must
match the underlying plot exactly.

Function \sQuote{plotid} identifies
and labels samples (optionally with values from a third vector)
in the PCO, and requires interaction with the mouse:
left button identifies, right button exits.

Function \sQuote{points} is passed a logical
vector to identify a set of samples by color of glyph.  It can be used
to identify a single set meeting almost any criterion that can be
stated as a logical expression.

Function \sQuote{hilight} is passed a factor vector or integer vector,
and identifies factor values by color and glyph.  By specifying values for
arguments \sQuote{cols} and \sQuote{glyph} it is possible to control the sequence of colors and pch glyphs used in the hilight.

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
\sQuote{family = poisson} to get a Poisson GAM.  }

\value{Function \sQuote{plotid} returns a vector of row numbers of
identified plots}

\note{Previous versions of surf relied on the \sQuote{interp} function of
package \code{\link[akima]{akima}}.  The revised routine using
\code{\link[mgcv]{predict.gam}} was suggested by Jari Oksanen as used in
package\code{\link[vegan]{ordisurf}}.
}

\references{\url{http://ecology.msu.montana.edu/labdsv/R/labdsv}}
\author{
David W. Roberts
\email{droberts@montana.edu}
}

\examples{
data(bryceveg)
data(brycesite)
dis.bc <- dsvdis(bryceveg,'bray/curtis')
pco.1 <- pco(dis.bc,5)
plot(pco.1)
points(pco.1,brycesite$elev>8000)
surf(pco.1,brycesite$elev)
\dontrun{plotid(pco.1,ids=row.names(bryceveg))}
}
\keyword{hplot}
\keyword{aplot}
\keyword{iplot}
