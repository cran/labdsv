\name{const}
\alias{const}
\title{Constancy Table}
\description{For a classified set of vegetation samples,
lists for each species the fraction of samples in each
class the species occurs in.}
\usage{const(comm, clustering, minval = 0, show = minval, digits = 2, 
             sort = FALSE, spcord = NULL)}
\arguments{
    \item{comm}{a data.frame of species abundances with samples as rows and
species as columns}
    \item{clustering}{(1) an object of class \sQuote{clustering},
           class \sQuote{partana}, or class \sQuote{partition}, (2) a vector of 
numeric cluster memberships, (3) a factor vector, or (4) a character vector.}
    \item{minval}{the minimum constancy a species must have in at least
         one class to be included in the output}
    \item{show}{the minimum constancy a species must have to show a printed value}
    \item{digits}{the number of digits to report in the table}
    \item{sort}{a switch to control interactive re-ordering of the output table}
    \item{spcord}{a vector of integers to specify the order in which species
         should be listed in the table}
}
\details{Produces a table with species as rows, and species constancy in clusters
    as columns.

The \sQuote{clustering} vector represents a classification of the samples that
the table summarizes.  It may result from a cluster analysis, partitioning
an ordination, subjective partitioning of a vegetation table, or other source.

The \sQuote{minval} argument is used to emphasize the dominant species and
suppress the rare species.  Vegetation tables are often very sparse, and
this argument simplifies making them more compact.

The \sQuote{digits} argument limits the reported precision of the calculations.
Generally, relatively low precision is adequate and perhaps more realistic.

The \sQuote{spcord} argument specifies the order species are listed in a table.
You can use the reverse of the number of occurrences to get dominant species
at the top to rarer at the bottom, use fidelity values for the ordered clusters,
or possibly the order of species centroids in an ordination.
}

\value{a data.frame with species as rows, classes as columns, with fraction
of occurrence of species in classes.}

\note{Constancy tables are often used in vegetation classification to
calculate or present characteristic species for specific classes or types.
\sQuote{const} may be combined with \sQuote{importance} and \sQuote{vegtab} to achieve a
vegetation table-oriented analysis.}
\author{
    David W. Roberts
    \email{droberts@montana.edu}
}
\seealso{
\code{\link[labdsv]{importance}},
\code{\link[labdsv]{vegtab}},
\code{\link[vegan]{vegemite}}
}
\examples{
data(bryceveg) # returns a data.frame called bryceveg
data(brycesite)
class <- cut(brycesite$elev,10,labels=FALSE)
const(bryceveg,class,minval=0.25)
}
\keyword{multivariate}
