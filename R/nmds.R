nmds <- function(dis,k=2,y=cmdscale(d=dis,k=k),maxit=50)
{
    tmp <- isoMDS(dis,y=y,k=k,maxit=maxit)
    class(tmp) <- "nmds"
    return(tmp)
}

plot.nmds <- function(x,ax = 1, ay = 2, col = 1, title = "", pch = 1, ...)
{
    if (is.null(class(x))) {
        stop("You must supply an object of class nmds from nmds")
    } else {
        if (class(x) != "nmds") {
            stop("You must supply an object of class nmds from nmds")
        }
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
        col = col, xlab = paste("NMDS", ax), ylab = paste("NMDS", ay),
        pch = pch, main = title, ...)
    par(pin=oldpin)
    invisible()
}

points.nmds <- function(x, which, ax = 1, ay = 2, col = 2,  pch = 1, cex=1, ...)
{
    if (is.null(class(x))) {
        stop("You must supply an object of class nmds from nmds")
    } else {
        if (class(x) != "nmds") {
            stop("You must supply an object of class nmds from nmds")
        }
    }
    if (is.logical(which)) {
        points(x$points[, ax][which], x$points[, ay][which],
            col = col, pch = pch, cex = cex, ...)
    } else if (is.numeric(which)) {
        if (!is.null(breaks)) {
            mask <- !is.na(which)
            cex = (which-min(which[mask])) / (max(which[mask])-min(which[mask])) * 5
        } else {
            cex = 1
        }
        points(x$points[, ax], x$points[, ay], 
            col = col, pch = pch, cex = cex, ...)
    }
}

plotid.nmds <- function(ord, ids=seq(1:nrow(ord$points)), ax = 1, ay = 2, col = 1, ...)
{
    if (is.null(class(ord))) {
        stop("You must supply an object of class nmds from nmds")
    } else {
        if (class(ord) != "nmds") {
            stop("You must supply an object of class nmds from nmds")
        }
    }
    identify(ord$points[, ax],ord$points[, ay],ids,col=col)
}

surf.nmds <- function(ord, var, ax=1, ay=2, col=2, 
     labcex = 0.8, family=gaussian, ...)
{
    if (is.null(class(ord))) {
        stop("You must supply an object of class nmds from nmds")
    } else {
        if (class(ord) != "nmds") {
            stop("You must supply an object of class nmds from nmds")
        }
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
        tvar <- as.numeric(var)
        tmp <- gam(tvar~s(x)+s(y),family=binomial)
    } else {
        tmp <- gam(var~s(x)+s(y),family=family)
    }
    contour(interp(x,y,fitted(tmp)),add=TRUE,col=col)
    print(tmp)
    d2  <- (tmp$null.deviance-tmp$deviance)/tmp$null.deviance
    cat(paste("D^2 = ",formatC(d2,width=4),"\n"))
}

jsurf.nmds <- function(ord, var, ax=1, ay=2, col=2, 
      labcex = 0.8, family=gaussian, ...)
{
    if (is.null(class(nmds))) {
        stop("You must supply an object of class nmds from nmds")
    } else {
        if (class(nmds) != "nmds") {
            stop("You must supply an object of class nmds from nmd")
        }
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
        tvar <- as.numeric(var)
        tmp <- gam(tvar~s(x)+s(y),family=binomial)
    } else {
        tmp <- gam(var~s(x)+s(y),family=family)
    }
    contour(interp(jitter(x),jitter(y),fitted(tmp)),add=TRUE,col=col)
    print(tmp)
    d2  <- (tmp$null.deviance-tmp$deviance)/tmp$null.deviance
    cat(paste("D^2 = ",formatC(d2,width=4),"\n"))
}
 
bestnmds <- function (dis,k=2,itr=20,maxit=100)
{
    out <- list()
    best <- 0
    minstr <- 99999
    for (i in 1:itr) {
        out[[i]] <- nmds(dis,k=k,y=matrix(runif(k*attr(dis,'Size')),ncol=k),maxit=maxit)
        if (out[[i]]$stress < minstr) {
            minstr <- out[[i]]$stress
            best <- i
        }
    }
    print(paste("best result = ",best))
    out[[best]]
}

hilight.nmds <- function (ord, factor, ax=1, ay=2, ...)
{
    if (is.null(class(ord))) 
       stop("You must pass an object of class nmds")
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
