const <- function (veg, class, minval = 0, digits = 2, spcord) 
{
    chop <- function(x) (as.integer(sum(x > 0) * 10^digits/sum(class == 
        i))/10^digits)
    if (!is.null(class(class)) && class(class) == 'partana') class <- class$clusid
    if (!is.null(class(class)) && class(class) == 'pam') class <- class$clustering
    if (!is.null(class(class)) && class(class) == 'slice') class <- class$clusid
    namlst <- NULL
    if (missing(class)) 
        class <- rep(1, nrow(veg))
    class <- class[!is.na(class)]
    if (is.factor(class)) {
        namlst <- levels(class)
        class <- as.integer(class)
    } else if (is.numeric(class)) {
        namlst <- as.integer(levels(factor(class)))
    }
    if (is.vector(class)) {
        res <- matrix(0, nrow = ncol(veg), ncol = max(class))
        for (i in 1:max(class)) {
            res[, i] <- apply(veg[class == i, ], 2, chop)
        }
        keep <- as.logical(apply(res, 1, max) >= minval)
        res <- res[keep, ]
        tmp <- as.data.frame(res)
        row.names(tmp) <- names(veg)[keep]
        if (!missing(spcord)) {
            tmp <- tmp[rev(order(spcord[keep])), ]
        }
    }
    if (!is.null(namlst)) 
        names(tmp) <- namlst
    tmp
}
