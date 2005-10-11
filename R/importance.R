importance <- function (taxa, clustering, minval = 0, digits = 2, spcord) 
{
    chop <- function(x) (as.integer(sum(x) * 10^digits/sum(clustering == 
        i))/10^digits)
    if (inherits(clustering,c('partana','partition','slice'))) 
        clustering <- clustering$clustering
    namlst <- NULL
    if (missing(clustering)) 
        clustering <- rep(1, nrow(taxa))
    clustering <- clustering[!is.na(clustering)]
    if (is.factor(clustering)) {
        namlst <- levels(clustering)
        clustering <- as.integer(clustering)
    } else if (is.numeric(clustering)) {
        namlst <- as.integer(levels(factor(clustering)))
    }
    if (is.vector(clustering)) {
        res <- matrix(0, nrow = ncol(taxa), ncol = max(clustering))
        for (i in 1:max(clustering)) {
            res[, i] <- apply(taxa[clustering == i, ], 2, chop)
        }
        keep <- as.logical(apply(res, 1, max) >= minval)
        res <- res[keep, ]
        tmp <- as.data.frame(res)
        row.names(tmp) <- names(taxa)[keep]
        if (!missing(spcord)) {
            tmp <- tmp[rev(order(spcord[keep])), ]
        }
    }
    if (!is.null(namlst)) 
        names(tmp) <- namlst
    tmp
}
