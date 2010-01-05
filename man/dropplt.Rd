\name{dropplt}
\alias{dropplt}
\title{Dropping Plots with Missing Values Form Taxon and Site Data Frames}
\description{Looks for plots which have missing values in site or environment
data, and deletes those plots from both the taxon and site data frames.}

\usage{dropplt(taxa,site)
}
\arguments{
    \item{taxa}{a taxon data frame}
    \item{site}{a site or environment data frame}
}
\details{First looks to see that the row names of the taxon data frame and the
site or environment data frame are identical.  If not, it prints an error
message and exits.  It then looks at the site or environment data frame for
plots or samples that have missing values, and deletes those plots from both the
taxon and site data frames.
}

\value{produces a list with two components:
\item{taxa}{the new taxon data frame}
\item{site}{the new site data frame}
}
\note{This is a VERY heavy-handed approach to managing missing values.  Most R
routines (including most of the package functions) have ways of handling missing
values that are fairly graceful.  This function simply maintains the
correspondence between the taxon and site data frames while eliminating ALL
missing values, and all plots that have missing values.}
\author{
    David W. Roberts
    \email{droberts@montana.edu}
}
\examples{
    data(bryceveg) # returns a data frame called bryceveg
    data(brycesite) # returns a data frame called brycesite
    demo <- dropplt(bryceveg,brycesite)
    newveg <- demo$taxon
    newsite <- demo$site
}
\keyword{data}
