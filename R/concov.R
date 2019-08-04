concov <- function (comm, clustering, digits = 1, width = 5, typical = TRUE,
    thresh = 10)
{
    if (missing(clustering)) {
        const <- apply(comm > 0, 2, sum)/nrow(comm)
        keep <- const >= thresh/100
        impt <- apply(comm, 2, sum)/nrow(comm)
        a <- formatC(as.numeric(const) * 100, width = 2, format = "d")
        b <- formatC(as.numeric(impt), width = width, digits = digits,
            format = "f")
        tmp <- NULL
        tmp <- cbind(tmp, paste(a, "(", b, ")", sep = ""))
        tmp <- tmp[keep]
        tmp <- data.frame(tmp)
        row.names(tmp) <- names(comm)[keep]
        attr(tmp, "call") <- match.call()
        attr(tmp, "comm") <- deparse(substitute(comm))
        attr(tmp, "timestamp") <- date()
        return(tmp)
    } else if (is.logical(clustering)) {
        comm <- comm[clustering, ]
        comm <- comm[, apply(comm > 0, 2, sum) > 0]
        x <- apply(comm > 0, 2, sum)
        y <- apply(comm, 2, sum)/x
        x <- x/nrow(comm)
        keep <- apply(as.matrix(x), 1, max) >= thresh/100
        a <- formatC(as.numeric(x) * 100, width = 2, format = "d")
        b <- formatC(as.numeric(y), width = width, digits = digits,
            format = "f")
        tmp <- NULL
        tmp <- cbind(tmp, paste(a, "(", b, ")", sep = ""))
        tmp <- tmp[keep]
        tmp <- data.frame(tmp)
        row.names(tmp) <- names(comm)[keep]
        names(tmp) <- deparse(substitute(clustering))
        attr(tmp, "call") <- match.call()
        attr(tmp, "comm") <- deparse(substitute(comm))
        attr(tmp, "clustering") <- clustering
        attr(tmp, "timestamp") <- date()
        return(tmp)
    }
    clustering <- clustify(clustering)

    if (length(table(clustering))==1)  {
        const <- apply(comm > 0, 2, sum)/nrow(comm)
        keep <- const >= thresh/100
        impt <- apply(comm, 2, sum)/nrow(comm)
        a <- formatC(as.numeric(const) * 100, width = 2, format = "d")
        b <- formatC(as.numeric(impt), width = width, digits = digits,
            format = "f")
        tmp <- NULL
        tmp <- cbind(tmp, paste(a, "(", b, ")", sep = ""))
        tmp <- tmp[keep]
        tmp <- data.frame(tmp)
        row.names(tmp) <- names(comm)[keep]
        names(tmp) <- deparse(substitute(clustering))
        attr(tmp, "call") <- match.call()
        attr(tmp, "comm") <- deparse(substitute(comm))
        attr(tmp, "clustering") <- clustering
        attr(tmp, "timestamp") <- date()
        return(tmp)
    } else {
        levels <- levels(clustering)
        clustering <- as.integer(clustering)
        x <- const(comm, clustering)
        y <- importance(comm, clustering, typical = typical,
            dots = FALSE)
        tmp <- NULL
        keep <- apply(as.matrix(x), 1, max) >= thresh/100
        for (i in 1:length(table(clustering))) {
            a <- formatC(as.numeric(x[, i]) * 100, width = 2,
                format = "d")
            b <- formatC(as.numeric(y[, i]), width = width, digits = digits,
                format = "f")
            tmp <- cbind(tmp, paste(a, "(", b, ")", sep = ""))
        }
        tmp <- tmp[keep, ]
        tmp <- data.frame(tmp)
        row.names(tmp) <- names(comm)[keep]
        names(tmp) <- levels
        attr(tmp, "call") <- match.call()
        attr(tmp, "comm") <- deparse(substitute(comm))
        attr(tmp, "clustering") <- clustering
        attr(tmp, "timestamp") <- date()
        return(tmp)
    }
    tmp
}

