\name{clustify}
\alias{clustify}
\title{Convert a Clustering Vector to a Consistent Factor}
\description{Convertsian object of class \sQuote{clustering} or a vector of 
integers, characters, or factors to an object of class \sQuote{factor}.}
\usage{clustify(clustering)
}
\arguments{
    \item{clustering}{an object of class \sQuote{clustering} or a vector of 
integers, characters, or factor levels}
}
\details{
Converts the input vector to a factor so that functions which receive the 
output can deal consistently with clustering methods and maintain the appropriate
labels.
}

\value{a object of class \sQuote{factor}}

\references{\url{http://ecology.msu.montana.edu/labdsv/R}}
\note{Many multivariate statistical methods require a vector of cluster
memberships or values.  Depending on the function that prodfuces the input vector, 
it could be a vector of integers, factor levels, characters, or logicals.  This
function simply converts all these cases to a factor.  Where a function requires 
consecutive integer codes, as.numeric(clustify(\sQuote{anything})) will produce
the required form, and where the classfication variable is a factor, 
levels(clustify(\sQuote{anything})) will produce the required output.
}
\author{
    David W. Roberts
    \email{droberts@montana.edu}
}


\examples{
data(brycesite) # returns a site variable data.frame
clustify(cut(brycesite$elev,5))
}
\keyword{multivariate}
