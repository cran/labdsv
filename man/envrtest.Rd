\name{envrtest}
\alias{envrtest}
\title{Environmental Distribution Test}
\description{Calculates whether the value of a specified environmental variable
has an improbable distribution with respect to a specified vector}
\usage{envrtest(set,env,numitr=1000,minval=0,replace=FALSE,
     plotit = TRUE, main = paste(deparse(substitute(set)),
     " on ", deparse(substitute(env))))
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
    \item{plotit}{logical; plot results if TRUE}
    \item{main}{title for plot if plotted}
}
\details{Calculates the maximum within-set difference in the values of vector
\sQuote{env}, and the distribution of the permuted random within-set differences.  It
then plots the observed difference as a red line, and the sorted permuted
differences as a black line and prints the probability of getting such a limited distribution.
The probability is calculated by permuting numitr-1 times, counting the number
of times the permuted maximum difference is as small or smaller than observed (n),
and calculating (n+1)/numitr.  To get three-digit probabilities, set numitr=1000 (the default)
}

\value{Produces a plot on the current graphics device, and an invisible list with the
components observed within-set difference and the p-value.
}
\note{The plot is based on the concept of constraint, or limiting value, and
checks to see whether the distribution of a particular variable within a cluster
is constrained in an improbable way.}
\author{
    David W. Roberts
    \email{droberts@montana.edu}
}
\examples{
    data(bryceveg) # returns a vegetation data.frame
    data(brycesite) # returns and environmental data.frame
    envrtest(bryceveg$berrep>0,brycesite$elev)
}
\keyword{cluster}
