euclidify <- function (dis,upper=FALSE,diag=FALSE)
{
    if (!inherits(dis,'dist')) 
        stop('The first argument must be an object of class dist')
    tmp <- .Fortran("euclid",dis=as.matrix(dis),as.integer(attr(dis,"Size")),
                     PACKAGE='labdsv')
    tmp2 <- as.dist(tmp$dis)
    attr(tmp2, "Labels") <- dimnames(dis)[[1]]
    attr(tmp2, "Diag") <- diag
    attr(tmp2, "Upper") <- upper
    attr(tmp2, "method") <- paste("euclidify", attr(dis, "method"))
    attr(tmp2, "call") <- match.call()
    attr(tmp2, "timestamp") <- date()
    tmp2
}



as.euclidean <- function(dis,upper=FALSE,diag=FALSE)
{
    return(euclidify(dis,upper=upper,diag=diag))
}
