\name{vegtab}
\alias{vegtab}
\title{Vegetation Table}
\description{Produces an ordered table of abundance of species in samples,
sub-sampled by (an optional) classification of the samples
}
\usage{vegtab(comm,set,minval=1,pltord,spcord,pltlbl,trans=FALSE)}
\arguments{
    \item{comm}{a vegetation (or other taxon) data.frame }
    \item{set}{a logical variable specifying which samples to include}
    \item{minval}{a minimum abundance threshold to include in the table}
    \item{pltord}{a numeric vector specifying the order of rows in the output}
    \item{spcord}{a numeric vector specifying the order of columns in the output}
    \item{pltlbl}{a vector specifying an alternative row label (must be unique!)}
    \item{trans}{a logical variable to control transposing the table}
}
\details{Subsets a vegetation data.frame according to specified plots or
   minimum species abundances, optionally ordering in arbitrary order.
}

\value{a data.frame with specified rows, columns, and row.names}
\references{\url{http://ecology.msu.montana.edu/labdsv/R/labs/lab3/lab3.html}}
\note{Vegetation tables are a common tool in vegetation analysis.  In recent
years analysis has tended to become more quantitative, and less oriented to
sorted tables, but even still presenting the results from these analyses
often involves a sorted vegetation table.}
\author{
    David W. Roberts
    \email{droberts@montana.edu}
    \url{http://ecology.msu.montana.edu/droberts/droberts.html}
}
\seealso{\code{\link[vegan]{vegemite}}}
\examples{
data(bryceveg)  # returns a vegetation data frame called bryceveg
data(brycesite) # returns an environmental data frame called 
                # brycesite
vegtab(bryceveg,minval=10,pltord=brycesite$elev)
        # produces a sorted table for species whose abundance sums
        # to 10, with rows in order of elevation.
}
\keyword{multivariate}
