\name{indval}
\alias{indval}
\alias{duleg}
\alias{indval.default}
\alias{indval.stride}
\alias{summary.indval}

\title{Dufrene-Legendre Indicator Species Analysis}
\description{Calculates the indicator value (fidelity and relative abundance) of
species in clusters or types.} 

\usage{
indval(x, \dots)
\method{indval}{default}(x,clustering,numitr=1000,\dots)
\method{indval}{stride}(x,comm,numitr=1,\dots)
\method{summary}{indval}(object, p=0.05, type='short', digits=2, show=p,
       sort=FALSE, too.many=100, \dots)
}

\arguments{
    \item{x}{a matrix or data.frame of samples with species as columns and
               samples as rows, or an object of class \sQuote{stride} from function
               \code{\link[optpart]{stride}}}
    \item{clustering}{a vector of numeric cluster memberships for samples, or a
                 classification object returned from \code{\link[cluster]{pam}}, 
                 or \code{\link[optpart]{optpart}}, \code{\link[optpart]{slice}},
                 or \code{\link[optpart]{archi}}}
    
    \item{numitr}{the number of randomizations to iterate to calculate
                  probabilities}
    \item{comm}{a data.frame with samples as rows and species as columns}
    \item{object}{an object of class \sQuote{indval}}
    \item{p}{the maximum probability for a species to be listed in the summary}
    \item{type}{a switch to choose between \sQuote{short} and \sQuote{long} style summary}
    \item{digits}{the number of significant digits to show}
    \item{show}{the threshold to show values as opposed to a dot column place-holder}
    \item{sort}{a switch to control user-managed interactive table sorting}
    \item{too.many}{a threshold reduce the listing for large data sets}
    \item{\dots}{additional arguments to the summary or generic function}
}
\details{Calculates the indicator value \sQuote{d} of species as the product of the 
relative frequency and relative average abundance in clusters.  Specifically,

where:\cr
\eqn{p_{ij}}{p_(ij)} =  presence/absence (1/0) of species \eqn{i} in 
   sample \eqn{j}; \cr
\eqn{x_{ij}}{x_(ij)} =  abundance of species \eqn{i} in sample \eqn{j}; \cr
\eqn{n_c} = number of samples in cluster \eqn{c};\cr
for cluster \eqn{c \in K}; \cr 

\deqn{f_{ic} = {\sum_{j \in c} p_{ij} \over n_c}}

\deqn{a_{ic} = {\sum_{j \in c} x_{ij} / n_c \over \sum_{k=1}^K (\sum_{j \in k} x_{ij} / n_k)}}

\deqn{d_{ic} = f_{ic} \times a_{ic}}

Calculated on a \sQuote{stride} the function calculates the indicator values of
species for each of the separate partitions in the stride.
}
\value{
The default function returns a list of class \sQuote{indval} with components:
    \item{relfrq}{relative frequency of species in classes}
    \item{relabu}{relative abundance of species in classes}
    \item{indval}{the indicator value for each species}
    \item{maxcls}{the class each species has maximum indicator value for}
    \item{indcls}{the indicator value for each species to its maximum class}
    \item{pval}{the probability of obtaining as high an indicator values as
                observed over the specified iterations}  

The stride-based function returns a data.frame with the number of clusters
in the first column and the mean indicator value in the second.

The \sQuote{summary} function has two options.  In \sQuote{short} mode it presents
a table of indicator species whose probability is less then \sQuote{p}, giving their 
indicator value and the identity of the cluster they indicate, along with the
sum of probabilities for the entire data set.  In \sQuote{long} mode, the indicator 
value of each species in each class is shown, with values less than \sQuote{show} 
replaced by a place-holder dot to emphasize larger values.

If \sQuote{sort==TRUE}, a prompt is given to re-order the rows of the matrix 
interactively.
}

\note{Indicator value analysis was proposed by Dufrene and Legendre (1997) as a
possible stopping rule for clustering, but has been used by
ecologists for a variety of analyses.  Dufrene and Legendre's nomenclature in
the paper is somewhat ambiguous, but the equations above are taken from the
worked example in the paper, not the equations on page 350 which
appear to be in error.  Dufrene and Legendre, however, multiply \eqn{d}
by 100; this function does not.}

\author{
    David W. Roberts
    \email{droberts@montana.edu}
    \url{http://ecology.msu.montana.edu/droberts/droberts.html}
}

\references{Dufrene, M. and Legendre, P.  1997.  Species assemblages and indicator species:
the need for a flexible asymmetrical approach.  Ecol. Monogr. 67(3):345-366.}

\seealso{\code{\link[labdsv]{isamic}}}

\examples{
data(bryceveg) # returns a vegetation data.frame
data(brycesite)
clust <- cut(brycesite$elev,5,labels=FALSE)
summary(indval(bryceveg,clust))
}
\keyword{cluster}
