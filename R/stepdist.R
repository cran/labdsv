stepdist <- function (dis,alpha) 
{ 
    labels<- attr(dis,'Labels')
    dis <- as.matrix(dis)
    dis[dis >= alpha] <- 9999.9
    n <- nrow(dis)
    out <- .Fortran('stepdist',
                    as.double(dis),
                    as.integer(n),
                    PACKAGE='labdsv')
    out <- as.dist(matrix(out[[1]],nrow=n))
    attr(out,'timestamp') <- date()
    attr(out,'call') <- match.call()
    attr(out,'Labels') <- labels
    if (max(out) == 9999.9) print('Space is disjunct')
    invisible(out)
}

