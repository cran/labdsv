indspc <- function (taxa, dis, numitr = 100)
{
    if (class(dis) != "dist")
        stop("Must pass a dist object")
    if (max(dis) > 1)
        stop("indspc is only defined for dissimlarities, not distances")
    if (!is.data.frame(taxa)) taxa <- data.frame(taxa)
    numspc <- ncol(taxa)
    numocc <- apply(taxa>0,2,sum)
    tmp <- 1 - as.matrix(dis)
    indval <- rep(0,numspc)
    for (i in 1:numspc) {
        mask <- taxa[, i] > 0
        if (sum(mask) > 1) {
            x <- as.matrix(tmp[mask, mask])
            indval[i] <- mean(x[row(x) > col(x)])
        }
        else {
            indval[i] <- 0
        }
    }

    maxocc <- max(apply(taxa>0,2,sum))
    q99 <- rep(0,maxocc)
    q95 <- rep(0,maxocc)
    q05 <- rep(0,maxocc)
    q01 <- rep(0,maxocc)
    pvals <- rep(1,numspc)
    sim <- 1-dis
    for (i in 2:maxocc) {
        tmp <- rep(0,numitr-1)
        pairs <- (i^2-i)/2
        for (j in 1:(numitr-1)) {
            tmp[j] <- mean(sample(sim,pairs,replace=FALSE))
        }
        q01[i] <- quantile(tmp,0.01)
        q05[i] <- quantile(tmp,0.05)
        q95[i] <- quantile(tmp,0.95)
        q99[i] <- quantile(tmp,0.99)
        for (j in seq(1:numspc)[numocc==i]) {
            pvals[j] <- (sum(tmp>indval[j])+1)/(numitr)
        }
    }

    x <- data.frame(indval, numocc, pvals)
    names(x) <- c("indval", "numocc", "pval")
    y <- data.frame(q01,q05,q95,q99)
    out <- list(vals=x, quantiles=y,mean=1-mean(dis))
    class(out) <- "indspc"
    out
}

plot.indspc <- function(x, ...)
{
    if (class(x) != 'indspc')
        stop("only defined for objects of class indspc")
    plot(x$vals$numocc[x$vals$numocc>1],x$vals$indval[x$vals$numocc>1],log='x')
    abline(x$mean,0,col=2)
    lines(2:max(x$vals$numocc),smooth(x$quantiles$q01[2:max(x$vals$numocc)]),col=2)
    lines(2:max(x$vals$numocc),smooth(x$quantiles$q05[2:max(x$vals$numocc)]),col=2)
    lines(2:max(x$vals$numocc),smooth(x$quantiles$q95[2:max(x$vals$numocc)]),col=2)
    lines(2:max(x$vals$numocc),smooth(x$quantiles$q99[2:max(x$vals$numocc)]),col=2)
    yorn <- readline("Do you want to identify species [Y or N] : ")
    if (yorn == 'Y' || yorn == 'y') {
        identify(x$vals$numocc,x$vals$indval,row.names(x$vals))
    }
    invisible()
}
