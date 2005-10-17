tabdev <- function (taxa,clustering,nitr=1000)
{
    totdev <- 0.0
    spcdev <- rep(0.0,ncol(taxa))
    pval <- rep(0.0,ncol(taxa))
    ntypes <- max(as.integer(clustering))
    relsum <- rep(0.0,ntypes)
    colsum <- rep(0.0,ntypes)
    spcsum <- rep(0.0,ncol(taxa))
    tmpdev <- 0.0
    pclass <- rep(0,nrow(taxa))
    if (inherits(clustering,c('partana','partition','slice')))
        clustering <- clustering$clustering
    tmp <- .Fortran('tabdev',
        as.double(as.matrix(taxa)),
        as.integer(nrow(taxa)),
        as.integer(ncol(taxa)),
        as.integer(clustering),
        max(as.integer(clustering)),
        spcdev = as.double(spcdev),
        totdev = as.double(totdev),
        pval = as.double(pval),
        as.integer(nitr),
        as.double(relsum),
        as.double(colsum),
        as.double(spcsum),
        as.double(tmpdev),
        as.integer(pclass),
        PACKAGE='labdsv')
    tmp2 <- data.frame(names(taxa),tmp$spcdev,round(tmp$pval,3))
    names(tmp2) <- c('species','deviance','p-val')
    result <- list(spcdev=tmp2,totdev=tmp$totdev)
    return(result)
}
