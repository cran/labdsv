\name{abuocc}
\alias{abuocc}
\title{Abundance/Occurrence Graphical Analysis}
\description{Calculates and plots summary statistics about species 
occurrences in a data frame}
}
\usage{abuocc(taxa,minabu=0)}
\arguments{
    \item{taxa}{a vegetation (or other taxon) dataframe with samples as rows 
     and species as columns}
    \item{minabu}{a minimum abundance threshold species must exceed to be 
     included in the calculations (default=0)}
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
\item{mean.abu}{mean abundance of each species when present 
     (excluding values smaller then minabu)}
}

\references{
\url{http://ecology.msu.montana.edu/labdsv/R}
}
\note{It's common in niche theory analyses to calculate the rank abundances 
of taxa in a site.  This function is similar, but works on multiple samples simultaneously.
The spc.plt vector in the returned list can be used anywhere species richness is desired.  
The plt.spc vector in the returned list can be used to mask out rare species in 
calculations of sample similarity using \code{\link[labdsv]{dsvdis}} among other purposes. }

\author{
	David W. Roberts
	\email{droberts@montana.edu}
	\url{http://ecology.msu.montana.edu/droberts}
}
\seealso{\code{\link[vegan]{fisherfit}}, 
         \code{\link[vegan]{prestonfit}},
         \code{\link[vegan]{radfit}}}
\examples{
    \dontrun{data(bryceveg)} # produces a data.frame called bryceveg
    \dontrun{abuocc(bryceveg)}
}
\keyword{multivariate}
