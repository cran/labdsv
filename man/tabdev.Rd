\name{tabdev}
\alias{tabdev}
\title{Table Deviance}
\description{Calculates the species (column-wise) and total deviance of
a classified vegetation data frame}
}
\usage{tabdev(taxa,clustering,nitr=1000)
}
\arguments{
    \item{taxa}{a vegetation (or other taxon) data frame}
    \item{clustering}{an integer vector specifying class membership 
         for each plot}
    \item{nitr}{number of iterations to permute the data}
}
\details{Calculates the relative abundance of taxa in classes, and then 
calculates the deviance associated with that taxon.  Total table deviance
is simply the sum of species deviances.  It calculates the probability of
obtaining as low a deviance as observed by permuting the species
values nitr-1 times.  The p-val is then calculated as (n+1)/nitr where
n is the number of cases with values as low or lower than observed.
}

\value{a list with components:
\item{spcdev}{a dataframe with columns species, deviance, p-value}
\item{totdev}{total table deviance}
}
\references{\url{http://ecology.msu.montana.edu/labdsv/R}}
\note{Table deviance is a useful way to test the validity of
clusters derived by other means, including cutting hierarchical
cluster analyses or non-hierarchical approaches.  In addition, it is
useful for identifying species of interest in the local flora.}
\author{
    David W. Roberts
    \email{droberts@montana.edu}
    \url{http://ecology.msu.montana.edu/droberts}
}
\seealso{\code{\link[labdsv]{duleg}}, \code{\link[labdsv]{duarm}}}

}
\examples{
    data(bryceveg) # retrieves a vegetation data frame
    clust <- sample(1:5,nrow(bryceveg),replace=TRUE)
    tabdev(bryceveg,clust)
}
\keyword{cluster}

