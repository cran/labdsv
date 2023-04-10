\name{calibrate}
\alias{calibrate}
\alias{calibrate.dsvord}
\title{Calculate fitted environmental attributes in an ordination}

\description{Fits a Generalized Additive Model (GAM) for 
    each environmental variable in a data.frame against an ordination.}

\usage{
\method{calibrate}{dsvord}(ord,site,dims=1:ncol(ord$points),
           family='gaussian',gamma=1,keep.models=FALSE,\dots)
}

\arguments{
    \item{ord}{an ordination object of class dsvord}
    \item{site}{a matrix or data.frame with sample units as rows
               and environmental variables as columns}
    \item{dims}{the specific dimensions of the ordination to consider}
    \item{family}{the error distribution specifier for the GAM function}
    \item{gamma}{the gamma parameter to control fitting GAM models}
    \item{keep.models}{a switch to control saving the individual GAM models}
    \item{\dots}{arguments to pass}
}

\details{The calibrate function sequentially and independently fits a GAM model
for each environmental variable as a function of ordination coordinates, using the 
family and gamma specifiers supplied in the function call, or their defaults.  The
model fits two or three dimensional models; if the length of dims is greater than three
the dimensions are truncated to the first three chosen.}

\value{A list object with vector elements aic, dev.expl, adj.rsq, and fitted value matrix.  
Optionally, if keep.models is TRUE, a list with all of the GAM models fitted.
List element aic gives the model AICs for each variable, dev.expl gives the deviance 
explained, adj.rsq gives the adjusted r-Squared, and fitted gives the expected value 
of each variable in each sample unit.}

\author{
    David W. Roberts 
    \email{droberts@montana.edu} 
}

\seealso{\link[labdsv]{predict} for the complementary function that fits GAM models
for species}

\examples{
data(bryceveg)
dis.man <- dist(bryceveg,method="manhattan")
demo.nmds <- nmds(dis.man,k=4)
\dontrun{res <- calibrate(demo.nmds,brycesite[,c(2,4,7,12)],minocc=10)}
}
\keyword{multivariate}
