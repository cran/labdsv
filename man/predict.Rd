\name{predict}
\alias{predict}
\alias{predict.dsvord}
\title{Predict species abundances in an ordination}

\description{This function fits a Generalized Additive Model (GAM) for 
    each species in a data.frame against an ordination.}

\usage{
\method{predict}{dsvord}(object,comm,minocc=5,dims=1:ncol(object$points),
                         family='nb',gamma=1,keep.models=FALSE,\dots)
}

\arguments{
    \item{object}{an object of class dsvord}
    \item{comm}{a community matrix or data.frame with samples as rows
               and species as columns}
    \item{minocc}{the minimum number of occurrences to model a species}
    \item{dims}{which specific dimensions to include}
    \item{family}{the error distribution specifier for the GAM function;
          can be 'nb' for negative binomial, 'poisson' for the 
          Poisson distribution, or 'binomial' for presence/absence data}
    \item{gamma}{the gamma parameter to control fitting GAM models}
    \item{keep.models}{a switch to control saving the individual GAM models}
    \item{\dots}{ancillary arguments to function predict}
}

\details{The predict function sequentially and independently fits a GAM model
of each species distribution as a function of ordination coordinates, using the 
family and gamma specifiers supplied in the function call, or their defaults.
The function fits two or three dimensional models; if the length of dims is greater than 
three the dimensions are truncated to the first three chosen.}

\value{A list object with vector elements aic, dev.expl, adj.rsq, and matrix fitted.  
Optionally, if keep.models is TRUE, a list with all of the GAM models fitted.
list element aic gives the model AICs for each species, dev.expl gives the deviance 
explained, adj.rsq gives the adjusted r-Squared, and fitted gives the expected abundance
of each species in each sample unit.}

\author{
    David W. Roberts 
    \email{droberts@montana.edu} 
}

\seealso{\link[labdsv]{calibrate} for the complementary function that fits GAM models
for environment variables}

\examples{
data(bryceveg)
dis.man <- dist(bryceveg,method="manhattan")
demo.nmds <- nmds(dis.man,k=4)
\dontrun{res <- predict(demo.nmds,bryceveg,minocc=10)}
}
\keyword{multivariate}
