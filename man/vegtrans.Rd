\name{vegtrans}
\alias{vegtrans}
\alias{stdveg}
\title{Vegetation Transformation and Standardization}
\description{Transforms vegetation abundances according to
an arbitrary specified vector, or standardize to sample sum
or species max.}
}
\usage{
vegtrans(x,a,b)
stdveg(x,pltsum=NULL,spcmax=NULL)
}
\arguments{
    \item{x}{the original vegetation data.frame}
    \item{a}{the set of values appearing in the original data.frame}
    \item{b}{the set of respective values to substitute}
    \item{pltsum}{standardize by plot sum if TRUE}
    \item{spcmax}{standardize by species maximum if TRUE}
}
\details{Performs a respective substitution to transform specific values
    in an initial data.frame to other specified values, or standardizes 
     the values.}

\value{a data.frame of transformed vegetation} 

\references{\url{http://ecology.montana.msu.edu/labdsv/}}
\note{Vegetation data are often collected in arbitrary abundance schemes
(e.g. Braun-Blanquet, Domin, etc. which have no direct algebraic 
transformation (e.g. log).  This function transforms coded abundances to 
arbitrary importance values as specified.

Many ecologist find that standardizing vegetation data improves the 
performance of some ordination or classification techniques.  Using
function \sQuote{stdveg} with i\sQuote{pltsum=TRUE} divides all values in a
sample by the sum of values for that sample, which eliminates the effect
of differeing productivity or estimation bias in the subsecquent analyses.

Using function \sQuote{vegstd} with \sQuote{spcmax=TRUE} divides each
values by the maximum values observed in the data set for that species.  
This has the effect of making all species equally important in subsequent 
analyses.}
 
\author{
    David W. Roberts
    \email{droberts@montana.edu}
    \url{http://ecology.msu.montana.edu/labdsv}
}
\seealso{\code{\linl[vegan]{decostand}}, \code{\link[vegan]{wisconsin}}}
\examples{
    data(bryceveg)
    old <- c(0.2,0.5,1.0,2.0,3.0,4.0,5.0,6.0)
    new <- c(0.2,0.5,3.0,15.0,37.5,62.5,85.0,97.5)
    newveg <- vegtrans(bryceveg,old,new)
}
\keyword{arith}
