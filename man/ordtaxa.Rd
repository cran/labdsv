\name{ordtaxa}
\alias{ordtaxa}
\title{Re-Order the Rows and Columns of a Taxon Data Frame}
\description{Allows analysts to interactively re-order a taxon data frame to
achieve a \sQuote{structured} table following phytosociological principles.}

\usage{ordtaxa(taxa,site)
}
\arguments{
    \item{taxa}{a taxon data frame}
    \item{site}{a site or environment data frame}
}
\details{Prints a copy of the taxon data frame, and then prompts for plots to
move in front of another plot.  It then prompts for species to move in front of
a specified species.  Multiple plots or species can be moved in a single move,
with plot or species IDs separated by commas with no blanks.  The program cycles
between prompting for plots to move, and then species to move, until both prompts
are responded to with blank lines.
}

\value{produces a list with two components:
\item{taxa}{the new taxon data frame}
\item{site}{the new site data frame}
}
\note{This is a a fairly simple means to sort a table.  For large tables, it is
often possible (and preferable) to sort the tables with ordination coordinates
or other indices, but this function allows analysts to order the table
arbitrarily into any form.}
\author{
    David W. Roberts
    \email{droberts@montana.edu}
}
\seealso{
    \code{summary.indval},\code{const},\code{importance}
}
\examples{
    \dontrun{data(bryceveg)} # returns a data frame called bryceveg
    \dontrun{data(brycesite)} # returns a data frame called brycesite
    \dontrun{demo <- ordtaxa(bryceveg,brycesite)}
    \dontrun{newveg <- demo$taxon}
    \dontrun{newsite <- demo$site}
}
\keyword{data}
