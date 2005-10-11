\name{vardist}
\alias{vardist}
\title{Variable Pairwise Difference}
\description{Calculates a matrix of pair-wise differences between value
      for a particular numeric variable for a dataset.
}
\usage{vardist(x)
}
\arguments{
    \item{x}{a vector of numeric values}
}
\details{Calculates the absolute value of the difference between
   all possible values in a vector, producing a matrix of differences
   with as many rows and columns as the length of the vector
}

\value{an object of class \code{dist}}

\references{\url{http://ecology.msu.montana.edu/labdsv/R}}
\note{This function can be used to compare the compositional differences
    among samples to the environmental differences by correlation or 
    regression, or perhaps other routines.}
\author{
    David W. Roberts
    \email{droberts@montana.edu}
    \url{http://ecology.msu.montana.edu/droberts}
}
\examples{
    data(bryceveg) # returns a vegetation data.frame called bryceveg
    data(brycesite) # returns an environmental data.frame called brycesite
    dis.bc <- dsvdis(bryceveg,'bray/curtis') # returns a Bray/Curtis dissimilarity matrix
    dis.elev <- vardist(brycesite$elev) # matrix of pair-wide differences in elevation
    cor(as.vector(dis.bc),as.vector(dis.elev))
}
\keyword{manip}
