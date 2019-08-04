ordcomp <- function(x,dis,dim,xlab="Computed Distance",ylab="Ordination Distance",title="",pch=1)
{
    if (inherits(x,"dsvord")) {
        z <- x$points
    } else if (inherits(x,"ordiplot")) {
        z <- x$sites
    } else {
        z <- x
    }

    y <- as.dist(dis)

    if (missing(dim)) dim <- ncol(z)
    if (ncol(z) > dim) cat(paste("Only comparing first",dim,"dimensions\n"))
    if (length(y) > 5000 & missing(pch)) pch <- "."

    a <- dist(z[,1:dim])
    plot(y,a,xlab=xlab,ylab=ylab,main=title,pch=pch)
    text(min(y),max(a),paste("r = ",format(cor(y,a),digits=3)),pos=4)
    invisible(cor(y,a))
}
