\name{ccm}
\alias{ccm}
\title{Community Composition Modeling}
\description{Compares the composition of modeled communities
to real data using Bray-Curtis similarity}
\usage{ccm(model,data)}
\arguments{
    \item{model}{fitted data from a predictive model}
    \item{data}{actual data from the modeled communities}
}
\value{A list object with two components:
\item{sim}{a vector of similarities of modeled communities to 
actual data}
\item{diverse}{Shannon-Weaver diversity values for modeled and real data}
}
\details{The algorithm sweeps through the fitted values and data
one sample unit at time calculating the similarity to the simulated community
to the real community.  The calculation is similarity, not dissimilarity, and
results in a vector of length equal to the number of sample units.

The diverse matrix has the diversity of the data in the first column, and the
diversity of the simulated or fitted data in the second column.} 

\author{ David W. Roberts
    \email{droberts@montana.edu}
}
\examples{
data(bryceveg) 
bryceveg <- dropspc(bryceveg,4)
bryce.bc <- dsvdis(bryceveg,'bray')
bryce.nmds <- nmds(bryce.bc)
\dontrun{bryce.preds <- predict(bryce.nmds,bryceveg)}
\dontrun{bryce.ccm <- ccm(bryceveg,bryce.preds$fitted)}
\dontrun{summary(bryce.ccm$sim)}
\dontrun{boxplot(bryce.ccm$diverse)}
}

