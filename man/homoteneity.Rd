\name{homoteneity}
\alias{homoteneity}
\title{Homoteneity Analysis of Classified Ecological Communities}
\description{Homoteneity is defined as \sQuote{the mean constancy of the S most constant
species, expressed as a fraction, where S is the mean species richness of a
type.}
}
\usage{homoteneity(taxa,clustering)
}
\arguments{
    \item{taxa}{a data.frame of species abundances with samples as rows and
          species as columns}
    \item{clustering}{a vector of (integer) class memberships, or an object of
         class \sQuote{clustering}, class \sQuote{partana},
         of class \code{\link[cluster]{partition}}}
}

\value{a data.frame of homoteneity values
}
\references{\url{http://ecology.montana.msu.edu/labdsv/}}
\note{This function was adapted from the Virginia Heritage
Program at 

http://www.dcr.virginia.gov/natural_heritage/ncstatistics.shtml}
\author{
    David W. Roberts
    \email{droberts@montana.edu}
}
\seealso{\code{\link[labdsv]{const}}, \code{\link[labdsv]{concov}}
}
\examples{
    data(bryceveg) # returns a data.frame of species in sample plots   
    data(brycesite) # returns a data.frame of site variables
    homoteneity(bryceveg,brycesite$quad) # analysis of species constancy
                                         # by USGS quad location
}
\keyword{multivariate}

