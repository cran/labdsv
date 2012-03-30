\name{concov}
\alias{concov}
\title{Constancy-Coverage Table for Ecological Community Data}
\description{Produces a table of combined species constancy and importance}
\usage{concov(taxa,clustering,digits=1,width=5,typical=TRUE,thresh=10)}
\arguments{
    \item{taxa}{a taxon data.frame, samples as rows and species as columns}
    \item{clustering}{a vector of (integer) cluster memberships, or an object of 
class \sQuote{clustering}, class \sQuote{partana}, or class \sQuote{partition}}
    \item{digits}{the number of digits for the importance value of species}
    \item{width}{controls the formatting of columns}
    \item{typical}{an argument passed to \code{\link[labdsv]{importance}} to control
how mean abundance is calculated}
    \item{thresh}{a theshold parameter to control the suppression of small
details in the output.  Species must have >= thresh constancy in at least one
type to appear in the output table}
}
\details{concov calls \code{\link[labdsv]{const}} and
\code{\link[labdsv]{importance}} and then combines the output in a single table.}
\value{a data.frame with factors (combined constancy and coverage) as columns}
\references{\url{http://ecology.montana.msu.edu/labdsv/}}
\note{Constancy-coverage tables are an informative and concise representation
of species in classified types. The output format [constancy(mean cover)] follows
the convention of the US Forest Service vegetation classifications.}
\author{
    David W. Roberts
    \email{droberts@montana.edu}
}
\seealso{\code{\link[labdsv]{const}}, \code{\link[labdsv]{importance}}}
\examples{
    data(bryceveg)  # returns a vegetation data.frame
    data(brycesite) # returns a site data.frame
    concov(bryceveg,brycesite$quad) # calculates the constance and coverage of
                                    # species by USGS quad map location
}
\keyword{multivariate}

