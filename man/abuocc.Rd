\name{abuocc}
\alias{abuocc}
\title{Abundance/Occurrence Graphical Analysis}
\description{Calculates and plots summary statistics about species
occurrences in a data frame}

\usage{abuocc(taxa,minabu=0,panel='all')}
\arguments{
    \item{taxa}{a taxon data.frame with samples as rows 
     and species as columns}
    \item{minabu}{a minimum abundance threshold species must exceed to be 
     included in the calculations (default=0)}
    \item{panel}{controls which of three graphs is drawn, and can be 'all'
                 or integers 1-4}
}
\details{This functions calculates and plots four data summaries about the 
         occurrence of species:

Plots:

1) the number of samples each species occurs in on a log scale, sorted from maximum to minimum

2) the number of species in each sample plot (species richness) from highest to lowest

3) the mean abundance of non-zero values (on a log scale) as a function of the number of plots a species occurs in

4) the total abundance/sample as a function of the plot-level species richness
                                                                                                  
The third plot allows you to identify individual species with the mouse;
the fourth plot allows you to identify individual samples with the mouse.
}


\value{Returns an (invisible) list composed of:
\item{spc.plt}{number of species/sample}
\item{plt.spc}{number of samples each species occurs in}
\item{mean}{mean abundance of each species when present 
     (excluding values smaller than minabu)}
}

\references{
\url{http://ecology.msu.montana.edu/labdsv/R/labs/lab1/lab1.html}
}
\note{It's common in niche theory analyses to calculate the rank abundances 
of taxa in a sample.  This function is similar, but works on multiple samples simultaneously.
The spc.plt vector in the returned list can be used anywhere species richness is desired.  
The plt.spc vector in the returned list can be used to mask out rare species in 
calculations of sample similarity using \code{\link[labdsv]{dsvdis}} among other purposes.}

\author{
	David W. Roberts
	\email{droberts@montana.edu}
}
\seealso{\code{\link[vegan]{fisherfit}}, 
         \code{\link[vegan]{prestonfit}},
         \code{\link[vegan]{radfit}}}
\examples{
    data(bryceveg) # produces a data.frame called bryceveg
    abuocc(bryceveg)
}
\keyword{multivariate}
