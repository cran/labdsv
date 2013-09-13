const <- function (taxa, clustering, minval = 0, show = minval, digits = 2, 
    sort = FALSE, spcord = NULL) 
{
    if (inherits(clustering, c("partana", "partition", "clustering"))) 
        clustering <- clustering$clustering
    namlst <- NULL
    if (!is.data.frame(taxa)) 
        taxa <- data.frame(taxa)
    if (missing(clustering)) 
        clustering <- rep(1, nrow(taxa))
    clustering <- clustering[!is.na(clustering)]
    if (is.factor(clustering)) {
        namlst <- levels(clustering)
        clustering <- as.integer(clustering)
    }
    else if (is.numeric(clustering)) {
        if (min(clustering)< 0 || (length(table(clustering)) != max(clustering))) {
            cat('WARNING: renumbering clusters to consecutive integers\n')
            clustering <- match(clustering,sort(unique(clustering)))
        }
        namlst <- as.integer(levels(factor(clustering)))
    }
    if (is.vector(clustering)) {
        res <- matrix(0, nrow = ncol(taxa), ncol = max(clustering))
        x <- apply(taxa,2,function(x){tapply(x>0,clustering,sum)})
        y <- as.numeric(table(clustering))
        res <- x/y
        keep <- as.logical(apply(res, 2, max) >= minval)
        res <- res[, keep]
        tmp <- as.data.frame(t(res))
        row.names(tmp) <- names(taxa)[keep]
        if (!is.null(spcord)) {
            tmp <- tmp[rev(order(spcord[keep])), ]
        }
    }
    if (!is.null(namlst)) 
        names(tmp) <- namlst
    tmp <- format(round(tmp, digits = digits))
    tmp[tmp < show] <- substring(" .  ", 1, digits + 2)
    if (sort) {
        print(tmp)
        repeat {
            plots <- readline(" enter the species: ")
            if (plots == "") 
                break
            else pnt <- readline(" in front of        : ")
            for (i in (strsplit(plots, ",")[[1]])) {
                ord <- 1:nrow(tmp)
                x <- match(i, row.names(tmp))
                if (!is.na(x)) {
                  z <- ord[x]
                  ord <- ord[-x]
                  y <- match(pnt, row.names(tmp))
                  if (!is.na(y)) {
                    if (y > 1) {
                      first <- ord[1:(y - 1)]
                      last <- ord[y:length(ord)]
                      ord <- c(first, z, last)
                    }
                    else {
                      last <- ord[y:length(ord)]
                      ord <- c(z, last)
                    }
                    tmp <- tmp[ord, ]
                    print(tmp)
                  }
                  else {
                    print(paste("species", pnt, "does not exist"))
                  }
                }
                else {
                  print(paste("species", i, "does not exist"))
                }
            }
        }
        return(tmp)
    }
    else {
        return(tmp)
    }
}
