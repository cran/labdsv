orddist <- function(x,dim=2)
{
    x <- as.matrix(x)
    size <- (nrow(x)*(nrow(x)-1))/2
    dist <- rep(0.0,size)
    tmp <- .Fortran("orddist",
        as.double(x),
        nrow(x),
        ncol(x),
        as.integer(dim),
        as.integer(size),
        out = dist,
        PACKAGE='labdsv')$out
    attr(tmp,"Size") <- nrow(x)
    class(tmp) <- "dist"
    return(tmp)
}
