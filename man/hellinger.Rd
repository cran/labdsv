\name{hellinger}
\alias{hellinger}
\title{Hellinger Data Transformation}
\description{Performs the Hellinger data transformation 
(square root of sample total standardized data).
}
\usage{hellinger(comm)
}
\arguments{
    \item{comm}{a community data.frame (samples as rows, species as columns)}
}
\details{Calculates a sample total standardization (all values in a 
row are divided by the row sum), and then takes the square root of the 
values.

}

\value{A community data.frame
}
\note{Hellinger standardization is a convex standardization that 
simultaneously helps minimize effects of vastly different sample total
abundances.}
\author{
    David W. Roberts
    \email{droberts@montana.edu}
}
\seealso{spcmax, samptot, abundtrans

}
\examples{
data(bryceveg)
hellveg <- hellinger(bryceveg)
}
\keyword{standardization}

