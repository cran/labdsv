metrify <- function (dis,upper=FALSE,diag=FALSE)
{
    if (!inherits(dis,'dist'))
        stop('The first argument must be an object of class dist')
    tmp <- .Fortran("metric",dis=as.matrix(dis),as.integer(attr(dis,"Size")),
                     PACKAGE='labdsv')
    tmp2 <- as.dist(tmp$dis)
    attr(tmp2, "Labels") <- dimnames(dis)[[1]]
    attr(tmp2, "Diag") <- diag
    attr(tmp2, "Upper") <- upper
    attr(tmp2, "method") <- paste("metrify", attr(dis, "method"))
    attr(tmp2, "call") <- match.call()
    tmp2
}

as.metric <- function (dis,upper=FALSE,diag=FALSE)
{ 
    if (!inherits(dis,'dist'))
        stop('The first argument must be an object of class dist')
    tmp <- .Fortran("metric",dis=as.matrix(dis),as.integer(attr(dis,"Size")),
                     PACKAGE='labdsv')
    tmp2 <- as.dist(tmp$dis)
    attr(tmp2, "Labels") <- dimnames(dis)[[1]]
    attr(tmp2, "Diag") <- diag
    attr(tmp2, "Upper") <- upper
    attr(tmp2, "method") <- paste("metrify", attr(dis, "method"))
    attr(tmp2, "call") <- match.call()
    tmp2
}

is.metric <- function (dis)
{ 
    if (!inherits(dis,'dist'))
        stop('The first argument must be an object of class dist')
    flag <- 0
    tmp <- .Fortran("ismetric",dis=as.matrix(dis),as.integer(attr(dis,"Size")),
                     flag=as.integer(flag),PACKAGE='labdsv')
    if (tmp$flag==0) ans <- TRUE
    else ans <- FALSE
    ans
}

