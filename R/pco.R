pco <- function(dis, k=2)
{
    tmp <-cmdscale(dis,k=k,eig=TRUE)
    class(tmp) <- "pco"
    return(tmp)
}

plot.pco <- function(x, ax = 1, ay = 2, col = 1, title = "", pch = 1, ...)
{
    if(missing(x)) {
        stop("You must specify an object of class pco from pco()")
    }
    oldpin <- par("pin")
    par(pin=c(min(oldpin[1],oldpin[2]),min(oldpin[1],oldpin[2])))
    xlim <- range(x$points[,ax])
    ylim <- range(x$points[,ay])    
    tol <- 0.04
        midx <- 0.5 * (xlim[2] + xlim[1])
        midy <- 0.5 * (ylim[2] + ylim[1])
    if (xlim[2]-xlim[1] > ylim[2]-ylim[1]) {
        xlim <- midx + (1 + tol) * 0.5 * c(-1, 1) * (xlim[2] - xlim[1])
        ylim <- midy + (1 + tol) * 0.5 * c(-1, 1) * (xlim[2] - xlim[1])
    }
    else {
        xlim <- midx + (1 + tol) * 0.5 * c(-1, 1) * (ylim[2] - ylim[1])
        ylim <- midy + (1 + tol) * 0.5 * c(-1, 1) * (ylim[2] - ylim[1])
    }
    plot(x$points[, ax], x$points[, ay], xlim = xlim, ylim = ylim, 
        col = col, xlab = paste("PCO", ax), ylab = paste("PCO", ay),
        pch = pch, main = title, ...)
    par(pin=oldpin)
    invisible()
}


points.pco <- function(x, which, ax = 1, ay = 2, col = 2,  pch = 1, cex=1, ...)
{
    if(missing(x)) {
        stop("You must specify an object of class pco from pco()") 
    }
    if(missing(which)) {
        stop("You must specify a logical subscript")
    }
    points(x$points[, ax][which], x$points[, ay][which],col=col,pch=pch,cex=cex, ...) 
}

plotid.pco <- function(ord, ids=seq(1:nrow(ord$points)), ax = 1, ay = 2, col = 1, ...)
{
    if(missing(ord)) {
        stop("You must specify a list object from princomp()")
    }
    identify(ord$points[, ax],ord$points[, ay],ids)
}

surf.pco <- function(ord, var, ax=1, ay=2, col=2, labcex=0.8, family=gaussian, ...) 
{
    if (is.null(class(ord))) {
        stop("You must supply an object of class pco from pco()")
    }
    else if(class(ord) != "pco") {
        stop("You must specify a pco object from pco()")
    }
    if(missing(var)) {
        stop("You must specify a variable to surface")
    }
    x <- ord$points[,ax]
    y <- ord$points[,ay]
    if (any(is.na(var))) {
        cat("Omitting plots with missing values \n")
        x <- x[!is.na(var)]
        y <- y[!is.na(var)]
        var <- var[!is.na(var)]
    }
    if (is.logical(var)) {
        tmp <- gam(var~s(x)+s(y),family=binomial)
    } else {
        tmp <- gam(var~s(x)+s(y),family=family)
    }
    contour(interp(x,y,fitted(tmp)),add=TRUE,col=col,labcex=labcex, ...)
    print(tmp)
    d2  <- (tmp$null.deviance-tmp$deviance)/tmp$null.deviance
    cat(paste("D^2 = ",formatC(d2,width=4),"\n"))
}

jsurf.pco <- function(ord, var, ax=1, ay=2, col=2, labcex=0.8, family=gaussian, ...)
{
    if (is.null(class(ord))) {
        stop("You must supply an object of class pco from pco()")
    }
    else if(class(ord) != "pco") {
        stop("You must specify a pco object from pco()")
    }
    if(missing(var)) {
        stop("You must specify a variable to surface")
    }
    x <- ord$points[,ax]
    y <- ord$points[,ay]
    if (any(is.na(var))) {
        cat("Omitting plots with missing values \n")
        x <- x[!is.na(var)]
        y <- y[!is.na(var)]
        var <- var[!is.na(var)]
    }
    if (is.logical(var)) {
        tmp <- gam(var~s(x)+s(y),family=binomial)
    } else {
        tmp <- gam(var~s(x)+s(y),family=family)
    }
    contour(interp(jitter(x),jitter(y),fitted(tmp)),add=TRUE,col=col,labcex=labcex, ...)
    print(tmp)
    d2  <- (tmp$null.deviance-tmp$deviance)/tmp$null.deviance
    cat(paste("D^2 = ",formatC(d2,width=4),"\n"))
}

hilight.pco <- function (ord, factor, ax=1, ay=2, ...)
{
    if (is.null(class(ord)))
       stop("You must pass an object of class pco")
    if (!is.null(class(factor))) {
        if (class(factor) == 'partana' ||
            class(factor) == 'pam' ||
            class(factor) == 'slice')  factor <- factor$clustering
    }
    else if (is.logical(factor)) {
        factor <- as.numeric(factor)
    }
    col <- 1
    pch <- 1
    for (i in 1:max(factor)) {
        if (i >= 8)
            points(ord, factor == i, ax, ay, col = 8, pch = 1)
        points(ord, factor == i, ax, ay, col = col, pch = pch)
        col <- col + 1
        if (col == 8) {
            col <- 1
            pch <- pch + 2
        }
    }
}

