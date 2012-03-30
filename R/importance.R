importance <- function (taxa, clustering, minval = 0, digits = 2, show = minval, 
    sort = FALSE, typical=TRUE, spcord) 
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
        namlst <- as.integer(levels(factor(clustering)))
    }
    if (is.vector(clustering)) {
        res <- matrix(0, nrow = ncol(taxa), ncol = max(clustering))
        x <- apply(taxa,2,function(x){tapply(x,clustering,sum)})
        if (typical) 
           y <- apply(taxa,2,function(x){tapply(x>0,clustering,sum)})
        else
           y <- apply(taxa,2,function(x){tapply(x>=0,clustering,sum)})
        y[x==0] <- 1
        res <- x/y
        keep <- as.logical(apply(res, 2, max) >= minval)
        res <- res[, keep]
        tmp <- as.data.frame(t(res))
        row.names(tmp) <- names(taxa)[keep]
        if (!missing(spcord)) {
            tmp <- tmp[rev(order(spcord[keep])), ]
        }
    }
    if (!is.null(namlst)) 
        names(tmp) <- namlst
    tmp <- format(round(tmp, digits = digits))
    tmp[tmp < show] <- substring(" .  ", 1, nchar(tmp[1, 1]))
    if (sort) {
        cat("\nConstancy Table\n\n")
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
                  ord <- ord[-x]
                  y <- match(pnt, row.names(tmp[ord, ]))
                  if (!is.na(y)) {
                    if (y == 1) {
                      ord <- c(x, ord)
                    }
                    else {
                      first <- ord[1:(y - 1)]
                      last <- ord[y:length(ord)]
                      ord <- c(first, x, last)
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
