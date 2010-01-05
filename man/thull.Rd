\name{plot.thull}
\alias{plot.thull}
\alias{thull}
\title{Plotting a Tensioned Hull}
\description{A tensioned hull is a minimum volume container for specified
elements of an ordination.  A \sQuote{thull} object is returned as an invisible
object by plotting a thull of an NMDS or PCO (or MFSO).  Subsequently plotting
the returned thull results in an \sQuote{image} of the representation.
}
\usage{
\method{plot}{thull}(x,col=rainbow(20),levels=NULL,cont=TRUE,
          xlab=x$xlab,ylab=x$ylab,main=x$main,\dots)
}
\arguments{
    \item{x}{an object of class \sQuote{thull} from function
\link[labdsv]{thull}}
    \item{col}{the color to use plotting the contours}
    \item{levels}{the specific levels desired for the contours}
    \item{cont}{a logical variable to control plotting contours on the image
                representation of the tensioned hull}
    \item{xlab}{the X axis label}
    \item{ylab}{the Y axis label}
    \item{main}{the main title}
    \item{\dots}{other graphics parameters}
}
\details{Tensioned hull analysis fits a minimun volume envelope to specific
points in an ordination.  A tenioned hull object is returned from function
\code{\link[labdsv]{thull}} of an NMDS or MFSO.  This function plots the
resulting tensioned hull as an image, with optional overlays of contours. 
}

\value{
Produces a plot on the cureent grapic device.
}

\references{\url{http://ecology.montana.msu.edu/labdsv/R}}

\author{
    David W. Roberts
    \email{droberts@montana.edu}
}

\examples{
    data(bryceveg) # returns a dataframe called brycceveg
    dis.bc <- dsvdis(bryceveg,'bray') # calculates a Bray-Curtis dissimilarity
                                      # matrix
    nmds.bc <- nmds(dis.bc) # calculates an NMDS ordination
    plot(nmds.bc) # plots the ordination on the current device
    demo.thull <- thull(nmds.bc,bryceveg$arcpat,0.25) # calculates the tensioned
                     # hull rperesenting the distribution of a species
    plot(demo.thull) # portrays the image version of the tensioned hull
}
\keyword{aplot}

