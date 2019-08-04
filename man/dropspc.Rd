\name{dropspc}
\alias{dropspc}
\title{Dropping Species with Few Occurrences}
\description{Eliminates species from the community data frame that occur fewer than
or equal to a threshold number of occurrences.}

\usage{dropspc(comm,minocc=0,minabu=0)
}
\arguments{
    \item{comm}{a community data frame}
    \item{minocc}{the threshold number of occurrences to be dropped}
    \item{minabu}{the threshold minimum abundance to be dropped}
}
\details{The function is useful for eliminating species (columns) from community
data frames which never occur, which often happens if you eliminate plots, and
those plots are the only ones that contain that species.  In addition, many
species are rare in data frames, and some algorithms (especially dissimilarity
functions and table sorting routines) benefit from smaller, simpler data frames.
}

\value{Produces a new community data frame}

\note{This is a heavy-handed approach to managing rare species in data.frames.
It is often possible to write a mask (logical vector) that suppresses the
influence of rare species and keeps the original data.frame intact, but this
function simplifies data management for some purposes.}

\author{
    David W. Roberts
    \email{droberts@montana.edu}
    \url{http://ecology.msu.montana.edu/droberts/droberts.html}
}
\examples{
data(bryceveg) # returns a data frame called bryceveg
newveg <- dropspc(bryceveg,5) # deletes species which 
                              # occur 5 or fewer times
}
\keyword{manip}
