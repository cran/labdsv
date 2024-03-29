\name{reconcile}
\alias{reconcile}
\title{Reconcile Community and Site Data.Frames}
\description{reconcile takes two data frames (comm and site) and sorts both
into the same order, and then deletes any rows unique to either of the two
data.frames, achieving perfect correspondence of the two. } 

\usage{reconcile(comm,site,exlist)}
\arguments{
\item{comm}{a community abundance data.frame with samples as rows and species as columns}
\item{site}{a data.frame of site or environmental variables with samples as rows and variables as columns}
\item{exlist}{a switch to control listing specific plots vs simply the number of plots}
}
\details{reconcile sorts each data.frame alphabetically by row.name, and then
compares the list of row.names to identify sample plots common to both
data.frames.  Sample plots which occur in only one of the data.frames are
deleted.
}

\value{A list object with two elements: comm and site, which are the sorted and
reconciled data.frames.
}
\note{Package labdsv (and many other packages in ecological data analysis)
require two data.frames to structure the data.  One contains the abundance of
species within samples with samples as rows and species as columns.  This data.frame I
refer to as the sQuote{comm} data.frame.  The other data.frame contains all the
environmental or site data collected at the same samples.  This data.frame I
refer to as the \sQuote{site} data.frame.  Due to independent subsampling, sorting or
editing of the data (often outside of R) the two data.frames often lose the necessary
requirement of the identical number of rows, with the rows in exactly the same
order.  The reconcile() function is a simple remedy to correct this situation
while maintaining the maximum amount of data.}
\author{
    David W. Roberts
    \email{droberts@montana.edu}
}
\examples{
data(bryceveg)   # returns a data.frame of taxon abundance
data(brycesite)  # returns a data.frame of site variables
test <- reconcile(bryceveg,brycesite)
}
\keyword{manip}

