pca <- function(mat, cor=FALSE, dim=min(nrow(mat),ncol(mat)))
{
    tmp <- prcomp(mat, retx=TRUE, center=TRUE, scale=cor)
    out <- list()
    out$scores <- tmp$x[,1:dim]
    out$loadings <- tmp$rotation[,1:dim]
    out$sdev <- tmp$sdev[1:dim]
    class(out) <- "pca"
    return(out)
}
                

plot.pca <- function(x, ax = 1, ay = 2, col = 1, title = "", pch = 1, ...)
{
    if(missing(x)) {
        stop("You must specify a PCA")
    }
    oldpin <- par("pin")
    par(pin = c(min(oldpin[1], oldpin[2]), min(oldpin[1], oldpin[2])))
    xlim <- range(x$scores[, ax])
    ylim <- range(x$scores[, ay])
    tol <- 0.04
    midx <- 0.5 * (xlim[2] + xlim[1])
    midy <- 0.5 * (ylim[2] + ylim[1])
    if(xlim[2] - xlim[1] > ylim[2] - ylim[1]) {
        xlim <- midx + (1 + tol) * 0.5 * c(-1, 1) * (xlim[2] - xlim[ 1])
        ylim <- midy + (1 + tol) * 0.5 * c(-1, 1) * (xlim[2] - xlim[ 1])
    }
    else {
        xlim <- midx + (1 + tol) * 0.5 * c(-1, 1) * (ylim[2] - ylim[ 1])
        ylim <- midy + (1 + tol) * 0.5 * c(-1, 1) * (ylim[2] - ylim[ 1])
    }
    plot(x$scores[, ax], x$scores[, ay], xlim = xlim, ylim = ylim, col = 
        col, xlab = paste("PCA", ax), ylab = paste("PCA", ay), pch = pch,
        main = title)
    par(pin = oldpin)
    invisible()
}

points.pca <- function(x, which, ax = 1, ay = 2, col = 2,  pch = 1, ...)
{
    if(missing(x)) {
        stop("You must specify a list object from pca")
    }
    if(missing(which)) {
        stop("You must specify a logical subscript")
    }
    if (length(which) != nrow(x$scores)) {
        stop("Points specifier must be of the same length as the number of samples")
    }
    points(x$scores[, ax][which], x$scores[, ay][which],col=col,pch=pch) 
}

plotid.pca <- function(ord, ids=seq(1:nrow(ord$scores)), ax = 1,  ay = 2, col = 1, ...)
{
    if(missing(ord)) {
        stop("You must specify a list object from princomp()")
    }
    identify(ord$scores[, ax],ord$scores[, ay],ids)
}

surf.pca <- function(ord, var, ax=1, ay=2, col=2, labcex=0.8, family=gaussian, ...)
{
    if(missing(ord)) {
        stop("You must specify an object of class pca")
    }
    if(missing(var)) {
        stop("You must specify a variable to surface")
    }
    x <- ord$scores[, ax]
    y <- ord$scores[, ay]
    if (is.logical(var)) {
        tmp <- gam(var~s(x)+s(y),family=binomial)
    } else {
        tmp <- gam(var~s(x)+s(y),family=family)
    }
    contour(interp(x,y,fitted(tmp)),add=TRUE,col=col,labcex=labcex,...)
    print(tmp)
    d2  <- (tmp$null.deviance-tmp$deviance)/tmp$null.deviance
    cat(paste("D^2 = ",formatC(d2,width=4),"\n"))
}

jsurf.pca <- function(ord, var, ax=1, ay=2, col=2, labcex=0.8, family=gaussian, ...){
    if(missing(ord)) {
        stop("You must specify an object of class pca")
    }
    if(missing(var)) {
        stop("You must specify a variable to surface")
    }
    x <- jitter(ord$scores[, ax])
    y <- jitter(ord$scores[, ay])
   if (is.logical(var)) {
        tmp <- gam(var~s(x)+s(y),family=binomial)
    } else {
        tmp <- gam(var~s(x)+s(y),family=family)
    }
    contour(interp(x,y,fitted(tmp)),add=TRUE,col=col,labcex=labcex,...)
    print(tmp)
    d2  <- (tmp$null.deviance-tmp$deviance)/tmp$null.deviance
    cat(paste("D^2 = ",formatC(d2,width=4),"\n"))
}

summary.pca <- function(object, dim=length(object$sdev), ...)
{
    vars <- object$sdev^2
    vars <- vars/sum(vars)
    cat("Importance of components:\n")
    print(rbind("Standard deviation" = object$sdev[1:dim],
        "Proportion of Variance" = vars[1:dim],
        "Cumulative Proportion" = cumsum(vars[1:dim])))
}

scores.pca <- function (x,labels=NULL,dim=length(x$sdev)) 
{
    if (dim>length(x$sdev)) {
        cat("Only",length(x$sdev)," axes available\n")
        dim <- length(x$sdev)
    }
    if (!is.null(labels)) {
        cbind(labels,x$scores[,1:dim])
    } else {
        x$scores[,1:dim]
    }
}

loadings.pca <- function (x, dim=length(x$sdev), digits=3, cutoff=0.1)
{
    if (dim>ncol(x$loadings)) {
        cat("Only",ncol(x$loadings),"axes available\n")
        dim <- ncol(x$loadings)
    }
    cat("\nLoadings:\n")
    cx <- format(round(x$loadings[,1:dim], digits = digits))
    cx[abs(x$loadings[,1:dim]) < cutoff] <- substring("       ",1, nchar(cx[1, 1]))
    print(cx, quote = FALSE)
    invisible()
}

varplot.pca <- function(x,dim=length(x$sdev)) 
{
    var <- x$sdev^2
    barplot(var[1:dim],ylab="Variance")
    readline("Hit Return to Continue\n")
    barplot((cumsum(var/sum(var)))[1:dim],ylab="Cumulative Variance")
}

hilight.pca <- function (ord, factor, ax=1, ay=2, ...)
{
    if (is.null(class(ord)))
       stop("You must pass an object of class pca")
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

