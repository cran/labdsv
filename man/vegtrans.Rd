\name{vegtrans}
\alias{vegtrans}
\title{Vegetation Data Transformation}
\description{Transforms vegetation abundances according to
an arbitrary specified vector}
\usage{
vegtrans(taxa,code,value)
}
\arguments{
    \item{taxa}{the original vegetation (or other taxon) data.frame}
    \item{code}{a vector containing the set of values appearing in the original data.frame}
    \item{value}{a vector containing the set of respective values to substitute}
}
\details{Performs a respective substitution to transform specific values
    in an initial data.frame to other specified values.}

\value{a data.frame of transformed vegetation}

\references{\url{http://ecology.msu.montana.edu/labdsv/R/labs/lab1/lab1.html}}
\note{Vegetation data are often collected in arbitrary abundance schemes
(e.g. Braun-Blanquet, Domin, etc.) which have no direct algebraic
transformation (e.g. log).  This function transforms coded abundances to
arbitrary importance values as specified.}

\author{
    David W. Roberts
    \email{droberts@montana.edu}
}
\seealso{\code{\link[vegan]{decostand}}, \code{\link[vegan]{wisconsin}}}
\examples{
    data(bryceveg)
    old <- c(0.2,0.5,1.0,2.0,3.0,4.0,5.0,6.0)
    new <- c(0.2,0.5,3.0,15.0,37.5,62.5,85.0,97.5)
    newveg <- vegtrans(bryceveg,old,new)
}
\keyword{arith}
