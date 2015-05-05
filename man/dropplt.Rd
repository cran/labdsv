\name{dropplt}
\alias{dropplt}
\title{Dropping Plots with Missing Values From Taxon and Site Data Frames}
\description{Looks for plots which have missing values in site or environment
data, and deletes those plots from both the taxon and site data frames.}

\usage{dropplt(taxa,site,which=NULL)
}
\arguments{
    \item{taxa}{a taxon data frame with samples as rows and species as columns}
    \item{site}{a site or environment data frame with samples as rows and 
variables as columns}
    \item{which}{a switch to dpecify specific plots to drop from both data.frames}
}
\details{First looks to see that the row names of the taxon data frame and the
site or environment data frame are identical.  If not, it prints an error
message and exits.  if which is NULL, It then looks at the site or environment data frame for
plots or samples that have missing values, and deletes those plots from both the
taxon and site data frames.  Alternatively, if which is a numeric scalar or vector
it deletes the specified plots from both the taxon and site data.frames.}

\value{produces a list with two components:
\item{taxa}{the new taxon data frame}
\item{site}{the new site data frame}
}
\note{This is a VERY heavy-handed approach to managing missing values.  Most R
routines (including most of the labdsv package functions) have ways of handling
missing values that are fairly graceful.  This function simply maintains the
correspondence between the taxon and site data frames while eliminating ALL
missing values, and all plots that have missing values.} 

\author{ David W. Roberts
    \email{droberts@montana.edu}
}
\examples{
    data(bryceveg)  # returns a data frame called bryceveg
    data(brycesite) # returns a data frame called brycesite
    demo <- dropplt(bryceveg,brycesite)
    newveg <- demo$taxa
    newsite <- demo$site
}
\keyword{manip}
