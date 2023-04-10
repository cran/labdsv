\name{concov}
\alias{concov}
\title{Constancy-Coverage Table for Ecological Community Data}
\description{Produces a table of combined species constancy and importance}
\usage{concov(comm,clustering,digits=1,width=5,typical=TRUE,thresh=10)}
\arguments{
    \item{comm}{a community data.frame, samples as rows and species as columns}
    \item{clustering}{(1) an object of class \sQuote{clustering}, class \sQuote{partana}, 
or class \sQuote{partition}, (2) a vector of integer cluster memberships, (3) 
a factor vector, or (4) a character vector}
    \item{digits}{the number of digits for the importance value of species}
    \item{width}{controls the formatting of columns}
    \item{typical}{an argument passed to \code{\link[labdsv]{importance}} to control
how mean abundance is calculated}
    \item{thresh}{a threshold parameter to control the suppression of small
details in the output.  Species must have >= thresh constancy in at least one
type to appear in the output table}
}
\details{concov calls \code{\link[labdsv]{const}} and
\code{\link[labdsv]{importance}} and then combines the output in a single table.}
\value{a data.frame with factors (combined constancy and coverage) as columns}
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
\dontrun{concov(bryceveg,brycesite$quad) # calculates the constancy 
                                         # and coverage by USGS quad}
}
\keyword{multivariate}


