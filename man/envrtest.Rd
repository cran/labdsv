\name{envrtest}
\alias{envrtest}
\title{Environmental Distribution Test}
\description{Calculates whether the value of a specified environmental variable
has an improbable distribution with repect to a specified vector}
\usage{envrtest(set,env,numitr=1000,minval=0,replace=FALSE)
}
\arguments{
    \item{set}{a vector of logical or quantitative values}
    \item{env}{the quantitative variable whose distribution is to be tested}
    \item{numitr}{the number of randomizations to iterate to calculate
probabilities}
    \item{minval}{the threshold to use to partition the data into a logical 
if set is quantitative}
    \item{replace}{whether to permute (replace=FALSE) or bootstrap
(replace=TRUE) the values in the permutation test}
}
\details{Calculates the maximum within-set difference in the values of vector
env, and the distribution of the permuted random within-set differences.  It
then plots the observed difference as a red line, and the sorted permuted
differences and prints the probability of getting such a limited distribution.
The probability is calculated by permuting numitr-1 times, counting the number
of times the permuted maximum difference is as small or smaller than observed (n),
and calculating (n+1)/numitr.  To get three-digit proabilities, set numitr=1000 (the default)
}

\value{Produces a plot on the current graphics device, and an invisible list with the
components obsered within-set difference and the p-value.
}
\references{\url{http://ecology.montana.msu.edu/labdsv/}}
\note{The plot is based on the concept of constraint, or limiting value, and
checks to see whether the distribution of a particuar variable within a cluster
is constrained in an improbable way.}
\author{
    David W. Roberts
    \email{droberts@montana.edu}
    \url{http://ecology.msu.montana.edu/labdsv}
}
\examples{
    data(bryceveg) # returns a vegetation data.frame
    data(brycesite) # returns and environmental data.frame
    dis.bc <- dsvdis(bryceveg,'bray/curtis') # returns a dissimilarity object
    clust <- sample(1:5,nrow(bryceveg),replace=TRUE)
    envrtest(clust,brycesite$elev)}
}
\keyword{cluster}