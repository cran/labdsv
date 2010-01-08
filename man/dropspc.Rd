\name{dropspc}
\alias{dropspc}
\title{Dropping Species with Few Occurrences}
\description{Eliminates species from the taxon data frame that occur fewer than
or equal to a threshold number of occurrences.}

\usage{dropspc(taxa,min=0)
}
\arguments{
    \item{taxa}{a taxon data frame}
    \item{min}{the threshold number of occurrences}
}
\details{The function is useful for eliminating species (columns) from taxon
data frames which never occur, which often happens if you eliminate plots, and
those plots are the only ones that contain that species.  In addition, many
species are rare in data frames, and some algorithms (especially dissimilarity
functions and table sorting routines) benefit from smaller simpler data frames.
}

\value{produces a new taxon data frame}

\note{This is a heavy-handed approach to managing rare species in data frames.
It is often possible to write a mask (logical vector) that suppresses the
influence of rare species and keep the original data frame intact, but this
function simplifies data management for some purposes.}

\author{
    David W. Roberts
    \email{droberts@montana.edu}
}
\examples{
    data(bryceveg) # returns a data frame called bryceveg
    newveg <- dropspc(bryceveg,5) # deletes species which occur 5 or fewer times
}
\keyword{data}
