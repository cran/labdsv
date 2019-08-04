\name{samptot}
\alias{samptot}
\title{Sample total standardization}
\description{Standardizes a community data set to a sample total
standardization.}
\usage{samptot(comm)
}
\arguments{
    \item{comm}{a community matrix (samples as rows, species as columns)}
}
\details{This function simply calculates row sums for the 
community matrix and then divides all values in that row by 
the appropriate sum so that all samples total to 1.0.
}

\value{A data frame of sample total standardized community data.
}
\references{\url{http://ecology.msu.montana.edu/labdsv/R}}
\author{
    David W. Roberts
    \email{droberts@montana.edu}
    \url{http://ecology.msu.montana.edu/droberts/droberts.html}
}
\seealso{
spcmax, abundtrans

}
\examples{
    data(bryceveg)
    stveg <- samptot(bryceveg)
    apply(stveg,1,sum)
}
\keyword{standardization}

