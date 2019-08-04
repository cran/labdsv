\name{dsvls}
\alias{dsvls}
\title{LabDSV Object ls() Command}

\usage{
dsvls(frame=NULL,opt='full')
}

\arguments{
   \item{frame}{an environment; if null substitutes parent.frame()}
   \item{opt}{a switch for \sQuote{full} or \sQuote{brief} output}
}

\description{
The function searches through all the objects in the specified
environment, and determines which ones have specific meaning
in LabDSV.  It then produces an output of a summary of every known
LabDSV object sorted by type.}

\value{Prints output to the console}

\references{
\url{http://ecology.msu.montana.edu/labdsv/R}
}

\note{It's common that after a while the number of objects in your
workspace can get large, and even with disciplined naming of objects
the list can get overwhelming.  dsvls() attempts to organize and 
report on the objects LabDSV understands.}

\author{
        David W. Roberts
        \email{droberts@montana.edu}
        \url{http://ecology.msu.montana.edu/droberts/droberts.html}
}

\examples{
data(bryceveg)
dis.bc <- dsvdis(bryceveg,'bray')
nmds.bc <- nmds(dis.bc,2)
dsvls()
}

\keyword{utilities}
