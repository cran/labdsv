predict.dsvord <- function(object,comm,minocc=5,dims=1:ncol(object$points),
                           family='nb',gamma=1,keep.models=FALSE, ...)
{
    if (!inherits(object,'dsvord'))
        stop("The first argument must be an object of class 'dsvord'")
    if (nrow(comm) != nrow(object$points))
        stop("The arguments are incompatible")

    getdev <- function(object) 
    {
        a <- object$deviance
        b <- object$null.deviance
        out <- 1 - (a/b)
        out
    }

    r.sq <- function (object)
    { 
        w <- as.numeric(object$prior.weights)
        mean.y <- sum(w * object$y)/sum(w)
        w <- sqrt(w)
        residual.df <- length(object$y) - sum(object$edf)
        nobs <- nrow(object$model)
        r.sq <- 1 - var(w * (as.numeric(object$y) - object$fitted.values)) *
            (nobs - 1)/(var(w * (as.numeric(object$y) - mean.y)) *
            residual.df)
        r.sq
    }

    points <- object$points[,dims]
    numdim <- ncol(points)
    if (numdim > 3) {
        cat("\n truncating to 3D\n")
        points <- points[,1:3]
    }
 
    check <- apply(comm>0,2,sum)>=minocc
    if (sum(check) < ncol(comm)) {
        rare <- ncol(comm)-sum(check)
        cat(paste("\n deleting",rare,"rare species\n"))
        comm <- comm[,check]
    }
    size <- ncol(comm)
    if (size == 0) stop("No species left, reduce minocc")
    res <- list()
    if (interactive()) pb <- txtProgressBar(min=0, max=ncol(comm), style=3)

    if (numdim == 2) {
        for (i in 1:size) {
            res[[i]] <- try(gam(comm[,i] ~ s(points[,1],points[,2]),
                family=family,gamma=gamma))
            if (inherits(res[[i]],'try-error'))
                res[[i]] <- gam(comm[,i] ~ s(points[,1]) + s(points[,2]),
                    family=family,gamma=gamma)
            if (interactive()) setTxtProgressBar(pb,i)
        }
    } else if (numdim == 3) {
        for (i in 1:size) {
            res[[i]] <- try(gam(comm[,i] ~
                s(points[,1],points[,2],points[,3]),
                family=family,gamma=gamma))
            if (inherits(res[[i]],'try-error'))
                res[[i]]<- gam(comm[,i] ~ s(points[,1]) + s(points[,2])+
                    s(points[,3]),family=family,gamma=gamma)
            if (interactive()) setTxtProgressBar(pb,i)
        }
    }
    if (interactive()) close(pb)
    aic <- sapply(res,AIC)
    dev <- sapply(res,getdev)
    adj.rsq <- sapply(res,r.sq)
    fitted <- sapply(res,predict,type='response')
    dimnames(fitted) <- list(row.names(comm),names(comm))
    out <- list(fitted=fitted,aic=aic,dev.expl=dev,adj.rsq=adj.rsq)
    if (keep.models) {
        out$models <- res
        dimnames(out$models) <- names(comm)
    }
    out
}
