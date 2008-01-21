\name{plot.pca}
\alias{plot.pca}
\alias{points.pca}
\alias{plotid.pca}
\alias{hilight.pca}
\alias{chullord.pca}
\alias{surf.pca}
\alias{varplot.pca}

\title{Plotting Routines For Principal Components Ordinations}
\description{A set of routines for plotting, highlighting points,
or adding fitted surfaces to PCAs.}
\usage{
\method{plot}{pca}(x, ax = 1, ay = 2, col = 1, title = "", pch = 1, \dots)
\method{points}{pca}(x, which, ax = 1, ay = 2, col = 2, pch = 1, cex = 1, \dots)
\method{plotid}{pca}(ord, ids = seq(1:nrow(ord$scores)), ax = 1, ay = 2,
    col = 1, \dots)
\method{hilight}{pca}(ord, overlay, ax = 1, ay = 2, cols=c(2,3,4,5,6,7), glyph=c(1,3,5),
    origpch = 1, blank = '#FFFFFF', \dots)
\method{chullord}{pca}(ord, overlay, ax = 1, ay = 2, cols=c(2,3,4,5,6,7), ltys = c(1,2,3), \dots)
\method{surf}{pca}(ord, var, ax = 1, ay = 2, col = 2, labcex = 0.8,
    family = gaussian, thinplate=TRUE, grid=50, gamma=1, \dots)
\method{varplot}{pca}(x, dim=length(x$sdev))
}

\arguments{
    \item{x}{an object of class \sQuote{pca}}
    \item{ax}{the dimension to use for the X axis}
    \item{ay}{the dimension to use for the Y axis}
    \item{title}{a title for the plot}
    \item{which}{a logical variable to specify points to be highlighted}
    \item{ord}{an object of class \sQuote{pca}}
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
    \item{dim}{number of dimensions to include in barplot}
    \item{col}{color index for points or contours}
    \item{labcex}{size of contour interval labels}
    \item{thinplate}{a logical swith to control using thinplate splines versus independent additive fits}
    \item{gamma}{controls the smoothness of the fit from \code{\link[mgcv]{gam}}}
    \item{grid}{the number of X and Y values used in establishing a grid}
    \item{pch}{plot character: glyph to plot}
    \item{cex}{character expansion factor: size of plotted characters}
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
in the PCA, and requires interaction with the mouse:
left button identifies, right button exits.

Function \sQuote{points} is passed a logical
vector to identify a set of samples by color of glyph.  It can be used
to identify a single set meeting almost any criterion that can be
stated as a logical expression.

Function \sQuote{hilight} is passed a factor vector or integer vector,
and identifies factor values by color and glyph.  By specifying values for
arguments \sQuote{cols} and \sQuote{glyph} it is possible to control
the sequence of colors and pch glyphs used in the hilight.

Function \sQuote{chullord} is passed a factor vector or integer vector,
and plots a convex hull around all points in each factor class.
By specifying values for arguments \sQuote{cols} and \sQuote{ltys}
it is possible to control the sequence of colors and linetypes of the convex
hulls.

Function \sQuote{surf} calculates and plots fitted surfaces for logical or
quantitative variables.  The function employs the \code{\link[mgcv]{gam}}
function to fit a variable to the ordination coordinates, and to predict the
values at all grid points.  The grid is established with the
\sQuote{expand.grid} function, and the grid is then specified in a call to
\sQuote{gam.predict}.  The predicted values are trimmed to the the convex hull
of the data, and the contours are fit by \sQuote{contour}.  The default link
function for fitting the GAMs is \sQuote{gaussian}, suitable for unbounded
continuous variables.  For logical variables you should specify \sQuote{family
= binomial} to get a logistic GAM, and for integer counts you should specify
\sQuote{family = poisson} to get a Poisson GAM.  }

\value{Function \sQuote{plotid} returns a vector of row numbers of
identified plots.

Function \sQuote{varplot.pca} produces two plots: (1) the variance accounted for
by eigenvector up to the specified number of dimensions
(default = all), and (2) the cumulative variance accounted for by dimension.
}

\note{Previous versions of surf relied on the \sQuote{interp} function of
package \code{\link[akima]{akima}}.  The revised routine using
\code{\link[mgcv]{predict.gam}} was suggested by Jari Oksanen as used in
package\code{\link[vegan]{vegan}}.
}

\references{\url{http://ecology.msu.montana.edu/labdsv/R/labdsv}}
\author{
David W. Roberts
\email{droberts@montana.edu}
}

\examples{
data(bryceveg)
data(brycesite)
pca.1 <- pca(bryceveg)
plot(pca.1)
points(pca.1,brycesite$elev>8000)
surf(pca.1,brycesite$elev)
\dontrun{plotid(pca.1,ids=row.names(bryceveg))}
}
\keyword{hplot}
\keyword{aplot}
\keyword{iplot}
