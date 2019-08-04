compspec <- function (comm, dis, numitr = 100, drop=FALSE, progress=FALSE)
{
    compspec.core <- function(comm,dis,maxocc,numitr)
    {
        numspc <- ncol(comm)
        numocc <- apply(comm>0,2,sum)
        tmp <- 1 - as.matrix(dis)
        compval <- rep(0,numspc)
        for (i in 1:numspc) {
            mask <- comm[, i] > 0
            if (sum(mask) > 1) {
                x <- as.matrix(tmp[mask, mask])
                compval[i] <- mean(x[row(x) > col(x)])
            } else {
                compval[i] <- 0
            }
        }

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
                pvals[j] <- (sum(tmp>compval[j])+1)/(numitr)
            }
        }
        x <- list(compval=compval, numocc=numocc, pvals=pvals,
                 quantiles=data.frame(q01,q05,q95,q99),mean=1-mean(dis))
        attr(x,'call') <- match.call()
        return(x)
    }

    if (!inherits(dis,"dist"))
        stop("Must pass a dist object")
    if (max(dis) > 1)
        stop("compspec is only defined for dissimilarities, not distances")
    if (!is.data.frame(comm)) comm <- data.frame(comm)

    maxocc <- max(apply(comm>0,2,sum))

    if (drop) {
        mean <- 0
        res <- list()
        compval <- rep(0,ncol(comm))
        numocc <- rep(0,ncol(comm))
        pval <- rep(1,ncol(comm))
        quantiles <- matrix(0,nrow=max(apply(comm>0,2,sum)),ncol=4)
        res$spc <- list()
        for (i in 1:ncol(comm)) {
            if (progress) cat(paste(i,'/',ncol(comm),'\n'))
            tmp.dis <- dsvdis(comm[,-i],attr(dis,'method'))
            res$spc[[names(comm)[i]]] <- 
                compspec.core(comm,tmp.dis,maxocc,numitr=numitr)
            quantiles <- quantiles + res$spc[[i]]$quantiles
            mean <- mean + res$spc[[i]]$mean
        }
        quantiles <- quantiles / ncol(comm)
        mean <- mean / ncol(comm)

        for (i in 1:length(res$spc)) {
            compval[i] <- res$spc[[i]]$compval[i]
            numocc[i] <- res$spc[[i]]$numocc[i]
            pval[i] <- res$spc[[i]]$pval[i]
        }
        res$compval <- compval
        res$numocc <- numocc
        res$pvals <- pval
        res$quantiles <- quantiles
        res$mean <- mean
    } else {
        res <- compspec.core(comm=comm,dis=dis,
            maxocc=maxocc,numitr=numitr)
    }
    out <- list()
    out$vals <- data.frame(res$compval,res$numocc,res$pvals)
    row.names(out$vals) <- names(comm)
    names(out$vals) <- c('compval','numocc','pval')
    out$quantiles <- res$quantiles
    out$mean <- res$mean
    if (drop) out$spc <- res$spc
    class(out) <- 'compspec'
    attr(out,'call') <- match.call()
    attr(out,'timestamp') <- date()
    out
}

plot.compspec <- function (x, spc=NULL, pch=1, type='p', col = 1, ...)
{
    if (!inherits(x,"compspec"))
        stop("only defined for objects of class compspec")
    if (is.null(spc)) {
        maxval <- max(x$vals$numocc)
        plot(x$vals$numocc[x$vals$numocc > 1], 
             x$vals$compval[x$vals$numocc > 1], 
             col = col, log = "x", xlim = c(2, maxval), pch=pch, type=type,
             xlab = "Number of Occurrences", ylab = "Similarity", ...)
        abline(x$mean, 0, col = 2)
        lines(2:maxval, smooth(x$quantiles$q01[2:maxval], endrule = "copy"),
            col = 2)
        lines(2:maxval, smooth(x$quantiles$q05[2:maxval], endrule = "copy"),
            col = 2)
        lines(2:maxval, smooth(x$quantiles$q95[2:maxval], endrule = "copy"),
            col = 2)
        lines(2:maxval, smooth(x$quantiles$q99[2:maxval], endrule = "copy"),
            col = 2)
        yorn <- readline("Do you want to identify species [Y or N] : ")
        if (yorn == "Y" || yorn == "y") {
            identify(x$vals$numocc, x$vals$compval, row.names(x$vals))
        }
    } else {
        maxval <- max(x$spc[[spc]]$numocc)
        plot(x$spc[[spc]]$numocc[x$spc[[spc]]$numocc>1],
            x$spc[[spc]]$compval[x$spc[[spc]]$numocc>1],
            log = "x", xlim = c(2, maxval), xlab = "Number of Occurrences",
            ylab = "Similarity", ...)
        abline(x$spc[[spc]]$mean, 0, col = 2)
        lines(2:maxval, smooth(x$spc[[spc]]$quantiles$q01[2:maxval], 
            endrule = "copy"), col = 2)
        lines(2:maxval, smooth(x$spc[[spc]]$quantiles$q05[2:maxval], 
            endrule = "copy"), col = 2)
        lines(2:maxval, smooth(x$spc[[spc]]$quantiles$q95[2:maxval], 
            endrule = "copy"), col = 2)
        lines(2:maxval, smooth(x$spc[[spc]]$quantiles$q99[2:maxval], 
            endrule = "copy"), col = 2)
        yorn <- readline("Do you want to identify species [Y or N] : ")
        print(yorn)
        if (yorn == "Y" || yorn == "y") {
            identify(x$spc[[spc]]$numocc, x$spc[[spc]]$compval, 
                names(x$spc[[spc]]$numocc))
        }

    }
    out <- data.frame(numocc=x$vals$numocc,compval=x$vals$compval)
    invisible(out)
}

summary.compspec <- function (object,pval=0.05,...)
{ 
    if (!inherits(object,'compspec'))
        stop("You must pass an object of class 'compspec'")

    cat(paste('call    = ',deparse(attr(object,'call')),'\n'))
    cat(paste('created = ',attr(object,'timestamp'),'\n'))
    tot <- nrow(object$vals)
    sigs <- sum(object$vals$pval<=pval)
    cat(paste('number of significant species = ',sigs,'/',tot,'\n'))
}

