calibrate.dsvord <- function(dsvord,site,dims=1:ncol(dsvord$points),
                             family='gaussian',gamma=1,keep.models=FALSE)
{
    if (!inherits(dsvord,'dsvord'))
        stop("The first argument must be an object of class 'dsvord'")
    if (nrow(site) != nrow(dsvord$points))
        stop("The arguments are incompatible")
    if (length(family) == 1) family <- rep(family,ncol(site))
    if (length(gamma) == 1) gamma <- rep(gamma,ncol(site))

    getdev <- function(object)
    {
        a <- object$deviance
        b <- object$null.deviance
        out <- 1 - (a/b)
        out
    }

    r.sq <- function(object)
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

    points <- dsvord$points[,dims]
    numdim <- ncol(points)
    if (numdim > 3) {
        cat("\n truncating to 3D\n")
        points <- points[,1:3]
    }

    check <- sapply(site,class)=="numeric"
    if (sum(check) > 0) {
        cat("\n omitting factors\n")
        site <- site[,check]
    }
    size <- ncol(site)
    res <- list()
    if (interactive()) pb <- txtProgressBar(min=0, max=ncol(site), style=3)

    if (numdim == 2) {
        for (i in 1:size) {
            res[[i]] <- try(gam(site[,i] ~ s(points[,1],points[,2]),
                family=family[i],gamma=gamma[i]))
            if (inherits(res[[i]],'try-error'))
                res[[i]] <- gam(site[,i] ~ s(points[,1]) + s(points[,2]),
                    family=family[i],gamma=gamma[i])
            if (interactive()) setTxtProgressBar(pb,i)
        }
    } else if (numdim == 3) {
        for (i in 1:size) {
            res[[i]] <- try(gam(site[,i] ~
                s(points[,1],points[,2],points[,3]),
                family=family[i],gamma=gamma[i]))
            if (inherits(res[[i]],'try-error'))
                res[[i]]<- gam(site[,i] ~ s(points[,1]) + s(points[,2])+
                    s(points[,3]),family=family[i],gamma=gamma[i])
            if (interactive()) setTxtProgressBar(pb,i)
        }
    }
    if (interactive()) close(pb)
    fitted <- sapply(res,predict,type='response')
    dimnames(fitted) <- list(row.names(site),names(site))
    aic <- sapply(res,AIC)
    dev <-sapply(res,getdev)
    adj.rsq <- sapply(res,r.sq)
    out <- list(fitted=fitted,aic=aic,dev.expl=dev,adj.rsq=adj.rsq)
    if (keep.models) {
        out$models <- res
        names(out$models) <- names(site)
    }
    out
}

