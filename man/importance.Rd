\name{importance}
\alias{importance}
\title{Importance Table}
\description{For a classified set of vegetation samples,
a importance table lists for each species
the average or typical abundance of each species in each class.
}
\usage{importance(taxa,clustering,minval=0,digits=2,show=minval,
       sort=FALSE,typical=TRUE,spcord)
}
\arguments{
    \item{taxa}{a data.frame of species abundances with samples as rows and
species as columns}
    \item{clustering}{a vector of (integer) class memberships, or an object of
         class \sQuote{clustering}, class \sQuote{partana},
         of class \code{\link[cluster]{partition}}}
    \item{minval}{the minimum importance a species must have in at least one class
to be included in the output}
    \item{digits}{the number of digits to report in the table}
    \item{show}{the minimum value a species must have to print a value}
    \item{sort}{a switch to control interactive re-ordering} 
    \item{typical}{a swith to control how mean abundance is calculated.
Typical=TRUE divides the sum of species abundance by the number of plots in which
it occurs; typical=FALSE divides by the number of plots in the type}
    \item{spcord}{a vector of integers to specify the order in which species
should be listed in the table}
}

\value{a data.frame with species as rows, classes as columns, with
    average abundance of species in classes.}

\references{\url{http://ecology.msu.montana.edu/labdsv/R/labs/lab3/lab3.html}}
\note{Importance tables are often used in vegetation classification to
calculate or present characteristic species for specific classes or types.
Importance may be combined with \code{\link[labdsv]{const}},
\code{\link[labdsv]{concov}} and \code{\link[labdsv]{vegtab}} to achieve a
vegetation table-oriented analysis.}

\author{
    David W. Roberts
    \email{droberts@montana.edu}
    \url{http://ecology.msu.montana.edu/droberts}
}
\seealso{\code{\link[labdsv]{const}}, \code{\link[labdsv]{vegtab}},
\code{\link[labdsv]{concov}}
}
\examples{
    data(bryceveg) # returns a data.frame called bryceveg
    class <- sample(1:10,nrow(bryceveg),replace=TRUE)
    importance(bryceveg,class,minval=0.25)
}
\keyword{multivariate}
