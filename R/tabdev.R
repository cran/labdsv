tabdev <- function (veg,clustering,nitr=1000)
{
    totdev <- 0.0
    spcdev <- rep(0.0,ncol(veg))
    pval <- rep(0.0,ncol(veg))
    tmp <- .Fortran('tabdev',
        as.matrix(veg),
        as.integer(nrow(veg)),
        as.integer(ncol(veg)),
        as.integer(clustering),
        as.integer(max(clustering)),
        spcdev = spcdev,
        totdev = totdev,
        pval = pval,
        as.integer(nitr),
        PACKAGE='labdsv')
    tmp2 <- data.frame(names(veg),tmp$spcdev,round(tmp$pval,3))
    names(tmp2) <- c('species','deviance','p-val')
    result <- list(spcdev=tmp2,totdev=tmp$totdev)
    return(result)
}
