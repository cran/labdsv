\name{isamic}
\alias{isamic}
\alias{duarm}

\title{Indicator Species Analysis Minimizing Intermediate Occurrences}

\description{Calculates the degree to which species are either
always present or always absent within clusters or types.}
\usage{isamic(comm,clustering,sort=FALSE)}

\arguments{
    \item{comm}{a matrix or data.frame of samples, species as columns, samples as
               rows}
    \item{clustering}{a vector of numeric cluster memberships for samples, or a
                 classification object returned from \code{\link[cluster]{pam}}, 
                 \code{\link[optpart]{partana}}, or \code{\link[optpart]{slice}}}
    \item{sort}{if TRUE, return in order of highest value to lowest rather than
                input order}
}

\details{Calculates the constancy (fractional occurrence of each species in
every type), and then calculates twice the the sum of the absolute values of the
constancy - 0.5, normalized to the number of clusters (columns).}

\value{A data.frame of species indicator values} 

\references{
Aho, K., D.W. Roberts, and T.W. Weaver.  2008. Using geometric and
non-geometric internal evaluators to compare eight vegetation
classification methods.  J. Veg. Sci. 19(4):549-562.}

\author{
    David W. Roberts
    \email{droberts@montana.edu}
    \url{http://ecology.msu.montana.edu/droberts/droberts.html}
}

\seealso{\code{\link[labdsv]{indval}}}

\examples{
data(bryceveg)
data(brycesite)
clust <- cut(brycesite$elev,5,labels=FALSE)
isamic(bryceveg,clust)
}
\keyword{cluster}
