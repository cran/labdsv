\name{confus}
\alias{confus}
\title{Confusion Matrix}
\description{A confusion matrix is a cross-tabulation of actual class
membership with memberships predicted by a disciminant function, 
classification tree, or other predictive model.}
\usage{confus(class,fitted)}
\arguments{
    \item{class}{a vector of (integer) class membership values}
    \item{fitted}{a matrix of predicted class memberships}
}
\details{Cross-classifies each sample by actual class membership and 
predicted membership, computing overall accuracy, and the Kappa 
statistic of agreement.}

\value{produces a list with elements
\itemlist{
\item{matrix}{the cross-tabulation matrix}
\item{correct}{the fraction of correctly predicted samples}
\item{kappa}{the value of the Kappa statistic}
\item{legend}{the text legend for the cross-tabulation matrix}
}
}
\references{\url{http://ecology.montana.msu.edu/labdsv/confus.html}}
\note{Confusion matrices are commonly computed in remote sensing
applications, but are equally suited to the evaluation of any 
predictive methods of class membership or factors.}
\author{
    David W. Roberts
    \email{droberts@montana.edu}
    \url{http://ecology.msu.montana.edu/labdsv}
}
\examples{
    data(bryceveg) # returns a data frame of vegetation data
    data(brycesite)
    \dontrun{library(tree)}
    \dontrun{mod <- tree(factor(bryceveg$arcpat>0)~
             elev+slope+av,data=brycesite)}
    \dontrun{pred <- predict(mod,newdata=brycesite)}
    \dontrun{confus(bryceveg$arcpat>0,pred)}
}
\keyword{htest}
