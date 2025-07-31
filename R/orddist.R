orddist <- function (x, dim)
{
    z <- x$points

    if (missing(dim)) dim <- ncol(z)
    if (dim != ncol(z))
        message(paste("Only comparing first",dim,"dimensions"))
    if (dim > ncol(z)) {
        dim <- ncol(z)
        message(paste("The ordination is only",dim,"dimensionsal."))
    }

    tmp <- dist(z[, 1:dim])
    tmp
}
