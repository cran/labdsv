\name{dsvdis}
\alias{dsvdis}
\title{Dissimilarity Indices and Distance Measures}
\description{This function provides a set of alternative dissimilarity indices
    and distance metrics for classification and ordination, including
    weighting by species (columns) and
    shortest-path adjustment for dissimilarity indices.}

\usage{dsvdis(x,index,weight=rep(1,ncol(x)),step=0.0,
       diag=FALSE, upper=FALSE)}

\arguments{
    \item{x}{a matrix of observations, samples as rows and variables as columns}
    \item{index}{a specific dissimilarity or distance index (see details below)}
    \item{weight}{a vector of weights for species (columns)}
    \item{step}{a threshold dissimilarity to initiate shortest-path adjustment
         (0.0 is a flag for no adjustment)}
    \item{diag}{a switch to control returning the diagonal (default=FALSE)}
    \item{upper}{a switch to control returning the upper (TRUE) or lower (FALSE)
                 triangle}
}

\details{
    The function calculates dissimilarity or distance between
    rows of a matrix of
    observations according to a specific index.  Three indices convert the data to
    presence/absence automatically.  In contingency table notation, they are:

    \tabular{ll}{
        steinhaus \tab \eqn{1 - a / (a + b + c)} \cr
        sorensen \tab \eqn{1 - 2a / (2a + b +c)} \cr
        ochiai \tab \eqn{1 - a / \sqrt{(a+b) * (a+c)}} \cr
    }

    Others are quantitative.  For variable i in samples x and y:

    \tabular{ll}{
        ruzicka \tab \eqn{1 - \sum min(x_i,y_i) / \sum max(x_i,y_i)} \cr
        bray/curtis \tab \eqn{1 - \sum[2 * min(x_i,y_i)] / \sum x_i + y_i} \cr
        roberts \tab \eqn{1 - [(x_i+y_i) * min(x_i,y_i) / max(x_i,y_i)] / (x_i + y_i)} \cr
        chisq \tab \eqn{(exp - obs) / \sqrt{exp}} \cr
    }
    The weight argument allows the assignment of weights to individual species in the calculation
    of plot-to-plot similarity.  The weights can be assigned by life-form, indicator value,
    or for other investigator specific reasons.  For the presence/absence indices the
    weights should be integers; for the quantitative indices the weights should be in the interval [0,1].
    The default (rep(1,ncol(x)) is to set all species = 1.

    The threshold dissimilarity \sQuote{step} sets all values greater than or equal to "step" to 9999.9 and
    then solves for the shortest path distance connecting plots to other non-9999.9 values
    in the matrix.   Step = 0.0 (the default) is a flag for "no shortest-path correction".
}

\value{Returns an object of class "dist", equivalent to that from \code{dist}.}

\note{Ecologists have spent a great deal of time and effort examining the properties
    of different dissimilarity indices and distances for ecological data.  Many
    of these indices should have more general application however.  Dissimilarity indices
    are bounded [0,1], so that samples with no attributes in common cannot be more
    dissimilar than 1.0, regardless of their separation along hypothetical or
    real gradients.  The shortest-path adjustment provides a partial solution.
    Pairs of samples more dissimilar than a specified threshold are set to
    9999.9, and the algorithm solves for their actual dissimilarity from the
    transitive closure of the triangle inequality.  Essentially, the dissimilarity
    is replaced by the shortest connected path between points less than the
    threshold apart.  In this way it is possible to obtain dissimilarities
    much greater than 1.0.

    The chi-square distance is not usually employed directly in cluster analysis or
    ordination, but is provided so that you can calculate correspondence analysis
    as a principal coordinates analysis (using \code{cmdscale}) from a simple
    distance matrix.}

\author{David W. Roberts 
        \email{droberts@montana.edu} 
        \url{http://ecology.msu.montana.edu/droberts/droberts.html}
}
\references{\url{http://ecology.msu.montana.edu/labdsv/R/labs/lab8/lab8.html}}

\seealso{\code{dist}, \code{\link[vegan]{vegdist}}}

\examples{
data(bryceveg)   # returns a data.frame called "bryceveg"
dis.ochiai <- dsvdis(bryceveg,index="ochiai")
dis.bc <- dsvdis(bryceveg,index="bray/curtis")
}

\keyword{multivariate}
