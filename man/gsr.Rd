\name{gsr}
\alias{gsr}
\title{Global Search and Replace for Data.frames}

\description{Performs in-place editing of data.frames that have
factor columns while correcting for the change to levels.} 

\usage{
gsr(field,old,new)
}

\arguments{
   \item{field}{a vector or specific column in a data.frame}
   \item{old}{a character vector of values to search for}
   \item{new}{a character vector of values to replace the 
              respective items in old}
}

\details{The function temporarily converts a vector or vector column in a data.frame
to a character vector, and then loops through the \sQuote{old} vector
looking for values to replace with the respective value in the \sQuote{new}
vector.  The column is then converted back to a factor.}

\value{a factor vector}

\note{The function is designed to make simple editing changes to data.frames
or factor vectors, resetting the levels appropriately.}

\author{
    David W. Roberts
    \email{droberts@montana.edu}
}

\examples{
data(brycesite)
brycesite$quad <- gsr(brycesite$quad,
    old=c('bp','bc','pc','rp','tc','tr'),
    new=c('BP','BC','PC','RP','TC','TR'))
}

\keyword{manip}


