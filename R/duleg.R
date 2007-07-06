duleg <- function(taxa,clustering,numitr=1000)
{
    if (!is.data.frame(taxa)) taxa <- data.frame(taxa)
    numplt <- nrow(taxa)
    numspc <- ncol(taxa)
    numcls <- as.integer(length(table(clustering)))
    maxcls <- rep(0,numspc)
    relfrq <- matrix(0,nrow=numspc,ncol=numcls)
    relabu <- matrix(0,nrow=numspc,ncol=numcls)
    indval <- matrix(0,nrow=numspc,ncol=numcls)
    indcls <- rep(0,numspc)
    pval <- rep(0,numspc)
    tmpfrq <- rep(0.0,numcls)
    tmpabu <- rep(0.0,numcls)
    pclass <- rep(0,numplt)
    tclass <- rep(0,numplt)
    tmp <- .Fortran("duleg",
        as.double(as.matrix(taxa)),
        as.integer(numplt),
        as.integer(numspc),
        as.integer(factor(clustering)),
        as.integer(table(clustering)),
        as.integer(numcls),
        as.integer(numitr),
        relfrq = relfrq,
        relabu = relabu,
        indval = indval,
        pval = pval,
	indcls = indcls,
        maxcls = as.integer(maxcls),
        as.double(tmpfrq),
        as.double(tmpabu),
        as.integer(pclass),
        as.integer(tclass),
        PACKAGE='labdsv')
    out <- list(relfrq=data.frame(tmp$relfrq),relabu=data.frame(tmp$relabu),
              indval=data.frame(tmp$indval),
              maxcls=tmp$maxcls,indcls=tmp$indcls,pval=tmp$pval)
    row.names(out$relfrq) <- names(taxa)
    row.names(out$relabu) <- names(taxa)
    row.names(out$indval) <- names(taxa)
    names(out$maxcls) <- names(taxa)
    names(out$indcls) <- names(taxa)
    names(out$pval) <- names(taxa)
    names(out$relfrq) <- levels(factor(clustering))
    names(out$relabu) <- levels(factor(clustering))
    names(out$indval) <- levels(factor(clustering))
    class(out) <- 'duleg'
    out
}

summary.duleg <- function(object, p=0.05, ...) 
{
    tmp <- data.frame(object$maxcls[object$pval <= p],
                      round(object$indcls[object$pval <= p],4),
                      object$pval[object$pval <= p])
    names(tmp) <- c('cluster','indicator_value','probability')
    print(tmp[order(tmp$cluster,-tmp$indicator_value),])
    cat(paste("\nSum of probabilities = ",sum(object$pval),"\n"))
}
