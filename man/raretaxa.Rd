\name{raretaxa}
\alias{raretaxa}
\title{Identify Rare Taxa in a Data Set}

\description{Identifes the distribution of rare taxa in a taxon dataframe,
using a specified rareness threshold.}

\usage{
raretaxa(taxa,min=1,log=FALSE,type='b', panel='all')
}
\arguments{
\item{taxa}{a taxon dataframe with samples as rows and species as columns}
\item{min}{the minimum number of occurrences for a species to be considered rare}
\item{log}{controls whether or not the Y axis on some graphs should be log
scaled} 
\item{type}{the plot type.  \sQuote{b} = both points and lines}
\item{panel}{a switch to control which graphic is displayed.  Can be either an integer
from 1 to 3 or the word \sQuote{all}.
}
}

\details{Rare species are an issue in ecological data sets.  This function
produces three graphs identifying (1) the distribution of rare species/plot, (2)
the mean abundance (when present) of rare species, and (3) the total abundance
or rare species/plot.
}

\value{Produces only graphs and returns no output}

\author{
David W. Roberts
\email{droberts@montana.edu}
}

\examples{
data(bryceveg)
\dontrun{raretaxa(bryceveg,min=3,log=TRUE)}
}

\keyword{hplot}
