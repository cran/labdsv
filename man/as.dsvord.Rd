\name{as.dsvord}
\alias{as.dsvord}
\title{Convert existing and external ordinations to dsv format}
\description{
This function updates ordinations from previous versions of labdsv and
converts ordinations of class 
\sQuote{boral} from package boral, list output objects 
from package Rtsne, class \sQuote{metaMDS} objects from package vegan, or class 
\sQuote{ordiplot} objects from package vegan into objects of class \sQuote{dsvord}
for plotting and comparison.
}
\usage{as.dsvord(obj)
}
\arguments{
    \item{obj}{an object of class nmds, pco, pca, boral, metaMDS, or ordiplot
               or an output list object from Rtsne}
}
\details{as.dsvord calls internal format-specific conversion functions to produce
an object of class \sQuote{dsvord} from the given input.
}

\value{an object of class \sQuote{dsvord}, i.e. a list with items
\sQuote{points} and \sQuote{type} (optionally more), and attributes
\sQuote{call} and \sQuote{timestamp} and \sQuote{class}.
}
\references{\url{http://ecology.msu.montana.edu/labdsv/R/}}
\note{LabDSV recently converted all ordination objects to a single class
with an ancillary \sQuote{type} specification to differentiate ordination
types.}
\author{
    David W. Roberts
    \email{droberts@montana.edu}
    \url{http://ecology.msu.montana.edu/droberts/droberts.html}
}
\examples{
\dontrun{data(bryceveg)
dis.bc <- dsvdis(bryceveg,'bray')
library(vegan)
demo.metaMDS <- metaMDS(bryceveg)
metamds.dsv <- as.dsvord(demo.metaMDS)
demo.ordi <- plot(demo.metaMDS)
ordip.dsv <- as.dsvord(demo.ordi)
library(boral)
demo.boral <- boral(bryceveg,row.eff='random')
boral.dsv <- as.dsvord(demo.boral)
}
}
\keyword{multivariate}
