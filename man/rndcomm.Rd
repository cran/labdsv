\name{rndcomm}
\alias{rndcomm}
\alias{rndtaxa}
\title{Randomize a Community Data.Frame}
\description{Permutes a vegetation (or other) data.frame to establish
a basis for null model tests in vegetation ecology.
}
\usage{rndcomm(comm,replace=FALSE,species=FALSE,plots=FALSE)}
\arguments{
    \item{comm}{the vegetation (or other taxon) data.frame, samples as rows, species as columns}
    \item{replace}{a switch for permuting (if FALSE) or boostrapping (if TRUE)}
    \item{species}{a switch to control randomizing by species (if TRUE),
       maintaining species occurrence distributions}
    \item{plots}{a switch to control randomizing by samples (if TRUE),
       maintaining plot-level species richness}
}
\details{Permutes or bootstraps a vegetation data frame for input to
   \code{\link{dist}}, \code{\link[vegan]{vegdist}}, \code{\link[labdsv]{dsvdis}},
    or other routines.  Can randomize by columns (species=TRUE), samples
    (plots=TRUE), or fully (neither species nor plots = TRUE).
}

\value{a data.frame with samples as rows and species as columns of the same
       dimensions as entered.}

\note{Randomizing vegetation often leads to unrealistic data distributions,
    but this function attempts to preserve either species occurrence distributions
    or plot-level species richness.  It is probably worth examining the output
    of this function with \code{\link[labdsv]{abuocc}} to see its characteristics
    before engaging in extensive analysis.}
\author{
    David W. Roberts
    \email{droberts@montana.edu}
}
\examples{
data(bryceveg) # returns a vegetation data.frame called bryceveg
test <- rndcomm(bryceveg,species=TRUE) # preserves species abundance
                                       # distribution
test2 <- rndcomm(bryceveg,plots=TRUE) # preserves plot-level 
                                      # species richness}
\keyword{datagen}
