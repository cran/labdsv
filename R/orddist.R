orddist <- function(x,dim=2)
{
    x <- as.matrix(x)
    size <- (nrow(x)*(nrow(x)-1))/2
    dist <- rep(0.0,size)
    tmp <- .Fortran("orddist",
        x,
        nrow(x),
        ncol(x),
        as.integer(dim),
        size,
        out = dist,
        PACKAGE='labdsv')$out
    attr(tmp,"Size") <- nrow(x)
    class(tmp) <- "dist"
    return(tmp)
}
