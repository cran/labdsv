\name{ordtest}
\alias{ordtest}
\title{Ordination Distribution Test}
\description{Testing the distribution of points in an ordination}
\usage{
ordtest(ord, var, dim=1:ncol(ord$points), index = 'euclidean',
   nitr = 1000)
}
\arguments{
\item{ord}{an object of class \sQuote{pca}, \sQuote{pco}, or
       \sQuote{nmds}}
\item{var}{a logical or factor vector used to organize the calculation
     of within-set distances}
\item{dim}{the specific dimensions to use in the calculation}
\item{index}{the distance metric for the calculation of within-set distances.
   Currently only euclidean is accepted}
\item{nitr}{the number of iterations to perform to establish p-values}
}
\details{
Calculates the sum of within-set pair-wise distances and compares to \sQuote{nitr}
permutations of the same distribution to calculate the probability of observing
clusters as tight as observed or tighter.  The p-value is calculated by
running nitr-1 permutations and counting the number of cases where the sum of
pair-wise distances is as small as smaller than observed.
That count is increased by one and divided by nitr to estimate p.
}
\value{
Produces a list with components:
\item{obs}{the observed sum of within-set distances}
\item{p}{the probability of obtaining a value that small}
\item{reps}{the sum of within-set pairwise distances for all permutations}
}
\references{\url{http://ecology.msu.montana.edu/labdsv/R}}
\author{
    David W. Roberts
    \email{droberts@montana.edu}
}
\seealso{\code{\link[vegan]{anosim}}}
\examples{
data(bryceveg)
data(brycesite)
dis.bc <- dsvdis(bryceveg,'bray/curtis')
pco.bc <- pco(dis.bc)
plot(pco.bc)
demo <- ordtest(pco.bc,brycesite$quad)
demo$p
}
\keyword{multivariate}
