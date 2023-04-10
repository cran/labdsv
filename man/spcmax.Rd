\name{spcmax}
\alias{spcmax}
\title{Species Maximum Standardization}
\description{Standardizes a community data.frame by
dividing the abundance of each species by the maximum 
value obtained for that species.}
\usage{spcmax(comm)
}
\arguments{
    \item{comm}{community data.frame (samples as rows, species as columns)}
}
\details{This is a simple standardization to make each species abundance
scaled from 0 to 1, essentially relativizing abundance by species and making
each species equal in the calculation of distance or dissimilarity or 
other analyses.
}

\value{A data.frame of standardized community data.
}
\author{
    David W. Roberts
    \email{droberts@montana.edu}
}
\seealso{samptot, abundtrans, hellinger

}
\examples{
data(bryceveg)
smveg <- spcmax(bryceveg)
apply(smveg,2,max)
}
\keyword{standardization}

