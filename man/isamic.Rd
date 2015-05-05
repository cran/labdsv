\name{isamic}
\alias{isamic}
\alias{duarm}

\title{Indicator Species Analysis Minimizing Intermediate Occurrences}

\description{Calculates the degree to which species are either
always present or always absent within clusters or types.}
\usage{isamic(taxa,clustering,sort=FALSE)}

\arguments{
    \item{taxa}{a matrix or data.frame of samples, species as columns, samples as
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

\value{a data.frame of species indicator values} 

\references{\url{http://ecology.msu.montana.edu/labdsv/R}
Aho, K., D.W. Roberts, and T.W.Weaver.  2008. Using geometric and
non-geometric internal evaluators to compare eight vegetation
classification methods.  J. Veg. Sci. In press.}

\note{This function was previously called \sQuote{duarm}, a horrible pun on the 
name \code{duleg}, which is an abbreviation for Dufrene and Legendre who defined
an alternative indicator species algorithm.  Following publication of Aho et al. 2008,
it was renamed \sQuote{isamic} as decribed in that paper. }

\author{
    David W. Roberts
    \email{droberts@montana.edu}
}

\seealso{\code{\link[labdsv]{indval}}}

\examples{
    data(bryceveg)
    dis.bc <- dsvdis(bryceveg,'bray/curtis')
    clust <- sample(1:5,nrow(bryceveg),replace=TRUE)
    isamic(bryceveg,clust)
}
\keyword{cluster}
