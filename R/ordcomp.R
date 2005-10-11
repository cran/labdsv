ordcomp <- function(x,y,dim=2,xlab="Computed Distance",ylab="Ordination Distance",title="",pch=1)
{
    y <- as.dist(y)
    if (class(x) == "pca") {
        z <- x$scores
        if (dim != ncol(x$scores)) {
            cat(paste("Only comparing first",dim,"dimensions\n"))
        }
    } 
    else if (inherits(x, c("pco", "nmds", "metaMDS"))) {
        z <- x$points
        if (dim != ncol(x$points)) {
            cat(paste("Only comparing first",dim,"dimensions\n"))
        }
    } 
    else if (class(x) == "fso") {
        z <- as.matrix(x$mu)
        if (dim != ncol(z)) {
            cat(paste("Only comparing first", dim, "dimensions\n"))
        }
    } 
    if (length(y) > 5000 & missing(pch)) pch <- "."
    a <- orddist(z,dim)
    plot(y,a,xlab=xlab,ylab=ylab,main=title,pch=pch)
    text(min(y),max(a),paste("r = ",format(cor(y,a),digits=3)),pos=4)
    invisible()
}
