euclidify <- function (x,upper=FALSE,diag=FALSE)
{
    tmp <- .Fortran("euclid",x=as.matrix(x),as.integer(attr(x,"Size")),
                     PACKAGE='labdsv')
    tmp2 <- as.dist(tmp$x)
    attr(tmp2, "Labels") <- dimnames(x)[[1]]
    attr(tmp2, "Diag") <- diag
    attr(tmp2, "Upper") <- upper
    attr(tmp2, "index") <- "euclidify"
    attr(tmp2, "call") <- match.call()
    attr(tmp2, "Size") <- nrow(x)
    tmp2
}
