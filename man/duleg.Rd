\name{duleg}
\alias{duleg}
\alias{summary.duleg}

\title{Dufrene-Legendre Indicator Species Analysis}
\description{Calculates the indicator value (fidelity and relative abundance) of
species in clusters or types.} 

\usage{
duleg(veg,class,numitr=1000)
method{summary}{dga}(object, \dots)
}

\arguments{
    \item{veg}{a matrix or data.frame of samples with species as columns and
               samples as rows}
    \item{class}{a vector of numeric class memberships for samples, or a
                 classification object returned from \code{\link[cluster]{pam}}, 
                 or \code{\link[labdsv]{partana}}}
    \item{numitr}{the number of ramdomizations to iterate to calculate
                  probabilities}
    \item{object}{an object of class \sQuote{duleg}}
}
\details{Calculates the indicator value $d$ of species as the product of the 
relative frequency and relative average abundance in clusters.  Specifically,

where:\cr
$p_{i,j} = $ presence/absence (1/0) of species $i$ in sample $j$; \cr
$x_{i,j} = $ abundance of species $i$ in sample $j$; \cr
$n_c = $ number of samples in cluster $c$;\cr
for cluster $c$ in set $K$; \cr 

\deqn{f_{i,c} = {\sum_{j \in c} p_{i,j} \over n_c}}\cr
\deqn{a_{i,c} = {(\sum_{j \in c} x_{i,j}) / n_c \over \sum_{k=1}^K ((\sum_{j \in k} x_{i,j}) / n_k)}}\cr
\deqn{d_{i,c} = f_{i,c} \times a_{i,c}}\cr
}

\value{a list with components:
    \item{relfrq}{relative frequency of species in classes}
    \item{relabu}{relative abundance of species in classes}
    \item{indval}{the indicator value for each species}
    \item{maxcls}{the class each species has maximum indicator value for}
    \item{indcls}{the indicator value for each species to its maximum class}
    \item{pval}{the probability of obtaining as high an indicator values as
                observed over the specified interations}  

The \sQuote{summary} function simply returns the sum of probabilities for
the species.
}

\note{Indicator value analysis was proposed by Dufrene and Legendre (1997) as a
source of possible stopping rules for clustering, but has been used by
ecologists for a variety of analyses.  Dufrene and Legendre's nomenclature in
the paper is somewhat ambiguous, but the equations above are taken from the 
worked example in the paper, not the equations on page 350 which 
appear to be in error.  Dufrene and Legendre, however, multiply $d$ by 100;
this function does not.}

\author{
    David W. Roberts
    \email{droberts@montana.edu}
    \url{http://ecology.msu.montana.edu/labdsv}
}

\references{Dufrene, M. and Legendre, P.  1997.  Species assemblages and indicator species:
the need for a flexible asymmetrical approach.  Ecol. Monogr. 67(3):345-366.}

\seealso{\code{\link[labdsv]{duarm}}}

\examples{
    data(bryceveg) # returns a vegetation dataframe
    dis.bc <- dsvdis(bryceveg,'bray/curtis') # returns a dissimilarity matrix
    clust <- sample(1:5,nrow(bryceveg),replace=TRUE)
    duleg(bryceveg,clust)
}
\keyword{cluster}
