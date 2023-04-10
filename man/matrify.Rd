\name{matrify}
\alias{matrify}
\title{Create Taxon Data.frames From Three Column Database Form}
\description{Takes a data.frame in three column form (sample.id, taxon,
abundance) and converts it into full matrix form, and then exports it as a
data.frame with the appropriate row.names and column names.}
\usage{matrify(data, strata=FALSE, base=100)}
\arguments{
    \item{data}{a data.frame or matrix in three column format (or database
format), where the first column is the sample ID, the second column is the taxon
ID, and the third sample is the abundance of that taxon in that sample.}
    \item{strata}{are the species abundances recorded in multiple strata?}
    \item{base}{what is the numeric base relative to 1.0}
}
\details{The routine is pure R code to convert data from database form to the sparse
matrix form required by multivariate analyses in packages \sQuote{labdsv} and
\sQuote{vegan}, as well as \code{dist} and other routines.  If TRUE, the strata argument
specifies calculating individual species abundances as independent overlap of strata.  
The base function is useful for converting percent to a fraction.
}

\value{A data.frame with samples as rows, taxa as columns, and abundance values
for taxa in samples.
}
\note{Typically, the source of the data will be an ASCII file or a dBase
database or a CSV file from an Excel file in three column format.  That file can
be read into a data.frame with \code{read.table} or \code{read.csv} and then
that data.frame can be matrified by this function.}
\author{
    David W. Roberts
    \email{droberts@montana.edu}
}
\seealso{\code{\link[labdsv]{dematrify}}}

\examples{
x <- cbind(c('a','a','b','b','b','c','c'),
           c('x','y','x','z','w','y','z'),
           c(1,2,1,3,2,2,1))
matrify(x) 
}
\keyword{IO}

