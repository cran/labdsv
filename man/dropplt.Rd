\name{dropplt}
\alias{dropplt}
\title{Dropping Plots with Missing Values From Taxon and Site Data Frames}
\description{Looks for plots which have missing values in site or environment
data, and deletes those plots from both the community and site data frames.}

\usage{dropplt(comm,site,which=NULL)
}
\arguments{
    \item{comm}{a community data frame with samples as rows and species as columns}
    \item{site}{a site or environment data frame with samples as rows and 
variables as columns}
    \item{which}{a switch to specify specific plots to drop from both data.frames}
}
\details{First looks to see that the row names of the community data frame and the
site or environment data frame are identical.  If not, it prints an error
message and exits.  If which is NULL, it then looks at the site or environment data frame for
plots or samples that have missing values, and deletes those plots from both the
community and site data frames.  Alternatively, if which is a numeric scalar or vector
it deletes the specified plots from both the community and site data.frames.}

\value{produces a list with two components:
\item{site}{the new site data frame}
}
\note{This is a VERY heavy-handed approach to managing missing values.  Most R
routines (including most of the labdsv package functions) have ways of handling
missing values that are fairly graceful.  This function simply maintains the
correspondence between the community and site data frames while eliminating ALL
missing values, and all plots that have missing values.} 

\author{ David W. Roberts
    \email{droberts@montana.edu}
    \url{http://ecology.msu.montana.edu/droberts/droberts.html}
}
\examples{
data(bryceveg)  # returns a data frame called bryceveg
data(brycesite) # returns a data frame called brycesite
demo <- dropplt(bryceveg,brycesite)
newcomm <- demo$comm
newsite <- demo$site
}
\keyword{manip}
