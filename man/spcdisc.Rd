\name{spcdisc}
\alias{spcdisc}
\title{Species Discrimination Analysis}
\description{
Calculates the degree to which species are restricted
to certain classes of classified vegetation}
}
\usage{spcdisc(x,sort=FALSE)}
\arguments{
    \item{x}{a classified vegetation table returned by
          \sQuote{const}, or \sQuote{importance}}
    \item{sort}{return in sorted order if TRUE}
}
\details{Calculates a Shannon-Weiner infomation statistic
on the relative abundance of species within classes.
}

\value{
A vector of discrimination values.
}
\author{
    David W. Roberts
    \email{droberts@montana.edu}
    \url{http://ecology.msu.montana.edu/labdsv}
}
\seealso{\code{\link[labdsv]{const}}, \code{\link[labdsv]{importance}},
        \code{\link[labdsv]{duleg}}, \code{\link[labdsv]{duarm}}
}
\examples{
data(bryceveg)
data(brycesite)
test <- const(bryceveg,brycesite$quad)
spcdisc(test)
}
\keyword{multivariate}
