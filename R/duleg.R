duleg <- function(veg,class,numitr=1000)
{
    numplt <- nrow(veg)
    numspc <- ncol(veg)
    numcls <- as.integer(length(table(class)))
    maxcls <- rep(0,numspc)
    relfrq <- matrix(0,nrow=numspc,ncol=numcls)
    relabu <- matrix(0,nrow=numspc,ncol=numcls)
    indval <- matrix(0,nrow=numspc,ncol=numcls)
    indcls <- rep(0,numspc)
    pval <- rep(0,numspc)
    tmp <- .Fortran("duleg",
        as.matrix(veg),
        as.integer(numplt),
        as.integer(numspc),
        as.integer(factor(class)),
        as.integer(table(class)),
        as.integer(numcls),
        maxcls = as.integer(maxcls),
        relfrq = relfrq,
        relabu = relabu,
        indval = indval,
	indcls = indcls,
        pval = pval,
        as.integer(numitr),
        PACKAGE='labdsv')
    out <- list(relfrq=data.frame(tmp$relfrq),relabu=data.frame(tmp$relabu),
              indval=data.frame(tmp$indval),
              maxcls=tmp$maxcls,indcls=tmp$indcls,pval=tmp$pval)
    names(out$relfrq) <- levels(factor(class))
    names(out$relabu) <- levels(factor(class))
    names(out$indval) <- levels(factor(class))
    class(out) <- 'duleg'
    out
}

summary.duleg <- function(object, ...) 
{
    cat(paste("Sum of probabilities = ",sum(object$pval),"\n"))
}
