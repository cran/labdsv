\name{const}
\alias{const}
\title{Constancy Table}
\description{For a classified set of vegetation samples, 
lists for a each species the fraction of samples in each 
class the species occurs in.}
\usage{const(veg, class, minval = 0, digits = 2, spcord)}
\arguments{
    \item{veg}{a data.frame of species abundances with samples as rows and 
species as columns}
    \item{class}{a vector of (integer) class memberships}
    \item{minval}{the minimum constance a species must have in at least 
         one class to be included in the output}
    \item{digits}{the number of digits to report in the table}
    \item{spcord}{a vector of integers to specify the order in which species 
         should be listed in the table}
}
\details{Produces a table with samples as rows, and species constancy in clusters
    as columns.

The \sQuote{class} vector represents a classification of the samples that
the table summarizes.  It may result from a cluster analysis, partitioning
an ordination, subjective partitioning of a vegetation table, or other source.

The \sQuote{minval} argument is used to select the dominant species and 
suppress the rare species.  Vegetation tables are often very sparse, and
this argument simplifies making them more compact.

The \sQuote{digits} argument limits the reported precision of the calculations.
Generally, relatively low precicion is adequate and perhaps more realsitic.

The \sQuote{spcord} argument specifies the order species are listed in a table.
You can use the reverse of the number of occurrences to get dominant species
at the top to rarer at the bottom, use fidelty values for the ordered clusters,
or possibly the order of species centroids in an ordination. 
}

\value{a data.frame with species as rows, classes as columns, with fraction 
of occurrrence of species in classes.}

\references{\url{http://ecology.montana.msu.edu/labdsv}}
\note{Constancy tables are often used in vegetation classification to
calculate or present characteristic species for specific classes or types.
\sQuote{const} may be combined with sQuote{importance} and \sQuote{vegtab} to achieve a 
vegetation table-oriented analysis.}
\author{
    David W. Roberts
    \email{droberts@montana.edu}
    \url{http://ecology.msu.montana.edu/labdsv}
}
\seealso{
\code{\link[labdsv]{importance}}, 
\code{\link[labdsv]{vegtab}},
\code{\link[vegan]{vegemite}}
}
\examples{
data(bryceveg) # returns a data.frame called bryceveg
class <- sample(1:10,nrow(bryceveg),replace=TRUE)
const(bryceveg,class,minval=0.25)
}
\keyword{multivariate}
