\name{disana}
\alias{disana}
\title{Dissimilarity Analysis}
\description{Dissimilarity analysis is a graphical analysis of the 
distribution of values in a dissimilaritiy matrix}
}
\usage{disana(x)
}
\arguments{
    \item{x}{an object of class \sQuote{dist} such as returned by 
    \code{\link{dist}},
    \code{\link[vegan]{vegdist}} or \code{\link[labdsv]{dsvdis}}}
}
\details{Calculates three vectors: the minimum, mean, 
and maximum dissimilarity for each sample in a dissimilarity matrix.  
By default it produces three plots: the sorted dissimilarity values,
the sorted min, mean, and maximum dissimilarity for each sample, and
the mean dissimilarity versus the minimum dissimilarity for each sample.  
Optionally, you can identify sample plots in the last panel with the mouse.
}

\value{Plots three graphs to the current graphical device, and returns
   an (invisible) list with four components:
\item{min}{the minimum dissimilarity of each sample to all others}
\item{mean}{the mean dissimilarity of each sampleto all others}
\item{max}{the maximum dissimilarity of each sample to all others}
\item{plots}{a vector of samples identified in the last panel}
}
\references{\url{http://ecology.montana.msu.edu/labdsv/disana}}
\note{Dissimilarity matrices are often large, and difficult to visualize
directly.  \sQuote{disana} is designed to highlight aspects of interest in these
large matrices. }
\author{
    David W. Roberts
    \email{droberts@montana.edu}
    \url{http://ecology.msu.montana.edu/labdsv}
}
\examples{
    data(bryceveg) # returns a data.frame called veg
    dis.bc <- dsvdis(bryceveg,'bray/curtis')
    \dontrun{disana(dis.bc)}
}
\keyword{multivariate}
