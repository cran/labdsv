ordcomp <- function(x,y,dim=2,xlab="Computed Distance",ylab="Ordination Distance",title="",pch=1)
{
    if(!is.null(class(y))) {
        if(class(y) == "dist") {}
    }
    if(is.matrix(y)) {
        y <- as.dist(y)
    }
    if (!is.null(class(x))) {
        if (class(x) == "pca") {
            z <- x$scores
            if (dim != ncol(x$scores)) {
                cat(paste("Only comparing first",dim,"dimensions\n"))
            }
        } else if (class(x) == "pco" | class(x) == "nmds") {
            z <- x$points
            if (dim != ncol(x$points)) {
                cat(paste("Only comparing first",dim,"dimensions\n"))
            }
        } else if (class(x) == "cca" | class(x) == "ca") {
            z <- x$F
        }
        else if (class(x) == "fso") {
            z <- as.matrix(x$mu)
            if (dim != ncol(z)) {
                cat(paste("Only comparing first", dim, "dimensions\n"))
            }
        }
    } else {
        z <- x
    }
    if (length(y) > 5000 & missing(pch)) pch <- "."
    a <- orddist(z,dim)
    plot(y,a,xlab=xlab,ylab=ylab,main=title,pch=pch)
    text(min(y),max(a),paste("r = ",format(cor(y,a),digits=3)),pos=4)
}
