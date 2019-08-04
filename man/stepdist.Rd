\name{stepdist}
\alias{stepdist}
\title{Step-Across Distance}
\description{Solves for the shortest-path step-across distance for 
a given distance matrix}

\usage{
stepdist(dis,alpha)
}

\arguments{
    \item{dis}{a distance or dissimilarity object of class \sQuote{dist}}
    \item{alpha}{a threshold distance to establish the step-across}
}


\details{The function takes the dist object and converts all values
>= alpha to 9999.9 and then solves for new distances by calculating 
the transitive closure of the triangle inequality.}



\value{an object of class \sQuote{dist}}


\note{The \sQuote{dsvdis} function includes a step-across function in
the initial calculation of a distance or dissimilarity matrix.  This function simply
allows the conversion to take place at a later time, or on distance metrics that
\sQuote{dsvdis} doesn't support.}  

\author{
    David W. Roberts
    \email{droberts@montana.edu}
    \url{http://ecology.msu.montana.edu/droberts/droberts.html}
}

\examples{
data(bryceveg)
dis.bc <- dsvdis(bryceveg,'bray')
dis.bcx <- stepdist(dis.bc,1.00)
disana(dis.bcx)
}


\keyword{multivariate}
