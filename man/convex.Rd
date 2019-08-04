\name{convex}
\alias{convex}
\title{Convex Data Transformation}
\description{Calculates a convex data transformation for a given
number of desired classes.}

\usage{convex(n,b=2,stand=FALSE)}

\arguments{
    \item{n}{the desired number of values}
    \item{b}{the base of the exponential function}
    \item{stand}{a switch to control standardizing values to a maximum of 1.0}
}
\details{Calculates a series of values where the difference between adjacent values
is 1/b the previous difference.  With the default b=2 you get an octave scale.}

\value{a vector of numeric values}

\references{\url{http://ecology.msu.montana.edu/labdsv/R}}
\author{
    David W. Roberts
    \email{droberts@montana.edu}
    \url{http://ecology.msu.montana.edu/droberts/droberts.html}
}
\seealso{spcmax, samptot, abundtrans, hellinger

}
\examples{
convex(5,2)
}
\keyword{standardization}

