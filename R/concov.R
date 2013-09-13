concov <- function (taxa,clustering,digits=1,width=5,typical=TRUE,thresh=10)
{
    if (inherits(clustering,c('clustering','partana','pam'))) 
        clustering <- clustering$clustering
    if (is.numeric(clustering)) {
        if (min(clustering)< 0 || (length(table(clustering)) != max(clustering))) {
            cat('WARNING: renumbering clusters to consecutive integers\n')
            clustering <- match(clustering,sort(unique(clustering)))
        }
    }

    x <- const(taxa,clustering)
    y <- importance(taxa,clustering,typical=typical)
    tmp <- NULL
    keep <- apply(as.matrix(x),1,max) >= thresh/100

    for (i in 1:length(table(clustering))) {
        a <- formatC(as.numeric(x[,i])*100,width=2,format='d')
        b <- formatC(as.numeric(y[,i]),width=width,digits=digits,format='f')
        tmp <- cbind(tmp,paste(a,'(',b,')',sep=''))
    }
    tmp <- tmp[keep,]
    tmp <- data.frame(tmp)
    row.names(tmp) <- names(taxa)[keep]
    names(tmp) <- as.character(levels(factor(clustering)))
    tmp
}
