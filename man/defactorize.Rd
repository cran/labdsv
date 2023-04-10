\name{defactorize}
\alias{defactorize}
\title{Change Factors in Data.frames to Character Vectors}

\description{Looks at each column in a data.frame, and converts factors
to character vectors.}

\usage{
defactorize(df)
}

\arguments{
    \item{df}{a data.frame}
}

\details{The function simply scans each column in a data.frame
looking for factor columns.  For each factor column it calls
the \sQuote{as.character()} function to convert the column to a character vector.}

\value{Returns a data.frame where every factor column has been converted
to a character vector.}

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

\seealso{\link[labdsv]{factorize}}

\examples{
data(brycesite)
brycesite <- defactorize(brycesite)
brycesite$quad[brycesite$quad=='bp'] <- 'BP'
brycesite <- factorize(brycesite)
}

\keyword{manip}


