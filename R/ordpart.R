ordpart <- function(ord, ax = 1, ay = 2)
{
    UseMethod("ordpart")
}

ordpart.pco <- function(ord,ax=1,ay=2)
{
    set <- 0
    clust <- rep(0,nrow(ord$points))
    while (1) {
        set <- set + 1
        tmp <- locator(type='l',col=set+1)
        if (length(tmp$x) > 0) {
            x <- c(tmp$x,tmp$x[1])
            y <- c(tmp$y,tmp$y[1])
            lines(x,y,col=set+1)
            tmp <- pip(ord$points[,ax],ord$points[,ay],x,y)
            points(ord,as.logical(tmp),ax,ay,col=set+1)
            clust <- pmax(clust,tmp*set)
        } else {
            break
        }
    }
    attr(clust,'class') <- 'clustering'
    return(clust)
}

ordpart.pca <- function(ord,ax=1,ay=2)
{
    set <- 0
    clust <- rep(0,nrow(ord$points))
    while (1) {
        set <- set + 1
        tmp <- locator(type='l',col=set+1)
        if (length(tmp$x) > 0) {
            x <- c(tmp$x,tmp$x[1])
            y <- c(tmp$y,tmp$y[1])
            lines(x,y,col=set+1)
            tmp <- pip(ord$scores[,ax],ord$scores[,ay],x,y)
            points(ord,as.logical(tmp),ax,ay,col=set+1)
            clust <- pmax(clust,tmp*set)
        } else {
            break
        }
    }
    attr(clust,'class') <- 'clustering'
    return(clust)
}

ordpart.nmds <- function (ord,ax=1,ay=2)
{
    set <- 0
    clust <- rep(0,nrow(ord$points))
    while (1) {
        set <- set + 1
        tmp <- locator(type='l',col=set+1)
        if (length(tmp$x) > 0) {
            x <- c(tmp$x,tmp$x[1])
            y <- c(tmp$y,tmp$y[1])
            lines(x,y,col=set+1)
            tmp <- pip(ord$points[,ax],ord$points[,ay],x,y)
            points(ord,as.logical(tmp),ax,ay,col=set+1)
            clust <- pmax(clust,tmp*set)
        } else {
            break
        }
    }
    attr(clust,'class') <- 'clustering'
    return(clust)
}

pip <- function (x,y,polyx,polyy) 
{
    z <- rep(0,length(x))
    res <- .Fortran("pip",
        as.double(x),
        as.double(y),
        as.integer(z),
        as.double(polyx),
        as.double(polyy),
        as.integer(length(x)),
        as.integer(length(polyx)),
        PACKAGE='labdsv')
    return(res[[3]])
}
