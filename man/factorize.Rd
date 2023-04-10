\name{factorize}
\alias{factorize}
\title{Change Character Vectors in Data.frames to Factors}

\description{Looks at each column in a data.frame, and converts character
vector columns to factors.}

\usage{
factorize(df)
}

\arguments{
    \item{df}{a data.frame}
}

\details{The function simply scans each column in a data.frame
looking for character vector columns.  For each character column it calls
the \sQuote{factor()} function to convert the column to a factor.}

\value{Returns a data.frame where every character column has been converted
to a factor}

\note{This function simplifies editing data.frames by allowing users to 
edit character columns (which have no levels constraints) and then 
converting the results to factors for modeling.  It is often used in a cycle of

defactorize(df)

edit the columns as necessary to correct errors or simplify

factorize(df)
}

\author{
    David W. Roberts
    \email{droberts@montana.edu}
}

\seealso{\link[labdsv]{defactorize}}

\examples{
data(brycesite)
brycesite <- defactorize(brycesite)
brycesite$quad[brycesite$quad=='bp'] <- 'BP'
brycesite <- factorize(brycesite)
}

\keyword{manip}


