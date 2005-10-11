\name{duarm}
\alias{duarm}

\title{Indicator Species Analysis Minimizing Partial Occurrences}

\description{Calculates the degree to which species are either 
always present or always absent within clusters or types.}
\usage{duarm(taxa,clustering,sort=FALSE)}

\arguments{
    \item{taxa}{a matrix or dataframe of samples, species as columns, samples as
               rows}
    \item{clustering}{a vector of numeric cluster memberships for samples, or a
                 classification object returned from \code{\link[cluster]{pam}}, 
                 or \code{\link[labdsv]{partana}}}
    \item{sort}{if TRUE, return in order of highest value to lowest rather than
                input order}
}

\details{Calculates the constancy (fractional occurrence of each species in
every type), and then calculates twice the the sum of the absolute values of the
constancy - 0.5, normalized to the number of clusters (columns).}

\value{a dataframe of species indicator values} 

\references{\url{http://ecology.msu.montana.edu/labdsv/R}}

\note{The name duarm is a horrible pun on the name duleg, which is an
abbreviation for Dufrene and Legendre who defined an alternative indicator
species algorithm.}

\author{
    David W. Roberts
    \email{droberts@montana.edu}
    \url{http://ecology.msu.montana.edu/droberts}
}

\seealso{\code{\link[labdsv]{duleg}}}

\examples{
    data(bryceveg)
    dis.bc <- dsvdis(bryceveg,'bray/curtis')
    clust <- sample(1:5,nrow(bryceveg),replace=TRUE)
    duarm(bryceveg,clust)
}
\keyword{cluster}
