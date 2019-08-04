\name{dematrify}
\alias{dematrify}
\title{Create Three Column Database Form Data Frame from Sparse Data Frames}
\description{Takes a sparse matrix data frame (typical of ecological
abundance data) and converts it into three column database format.}
\usage{dematrify(comm, filename, sep = ",", thresh = 0)}
\arguments{
    \item{comm}{a sparse data.frame or matrix, with samples as rows and comm 
as columns} 
    \item{filename}{the name of the filename to produce}
    \item{sep}{the separator to use in separating columns}
    \item{thresh}{the minimum abundance to be included in the output}
}
\details{The routine is pure R code to convert data from sparse
matrix form to three column database form for export or reduced storage}


\value{a data.frame with the first column the sample ID, the second column
the taxon ID, and the third column the abundance.
}
\note{Typically, large ecological data sets are characterized by sparse matrices
of taxon abundance in samples with many zeros in the matrix.   Because these datasets
may be many columns wide, they are difficult to work with in text editors or
spreadsheets, and require excessive amount of space for storage.  The reduced three column
form is suitable for input to databases, and more easily edited.}
\author{
    David W. Roberts
    \email{droberts@montana.edu}
    \url{http://ecology.msu.montana.edu/droberts/droberts.html}
}
\seealso{\code{\link[labdsv]{matrify}}}


\examples{
library(labdsv)
data(bryceveg)
x <- dematrify(bryceveg)
}
\keyword{IO}
