dsvls <- function (frame=NULL,opt='full')
{
    if (is.null(frame)) frame<- ls(parent.frame())

    df <- NULL
    dis <- NULL
    ord <- NULL
    clust <- NULL
    stride <- NULL
    ordip <- NULL
    for (i in frame) {
        tmp <- eval(parse(text=i))
        if (inherits(tmp,'data.frame')) df <- c(df,i)
        else if (inherits(tmp,'dist')) dis <- c(dis,i)
        else if (inherits(tmp,'dsvord')) ord <- c(ord,i)
        else if (inherits(tmp,c('clustering','partition','optpart','hclust'))) clust <- c(clust,i)
        else if (inherits(tmp,'stride')) stride <- c(stride,i)
        else if (inherits(tmp,'ordiplot')) ordip <- c(ordip,i)
    }
    if (opt == 'brief') {
        cat('data.frames\n')
        for (i in df) cat(paste('    ',i,'\n'))
        cat('distance/dissimilarity matrices\n')
        for (i in dis) cat(paste('    ',i,'\n'))
        cat('ordinations\n')
        for (i in ord) cat(paste('    ',i,'\n'))
        cat('classifications\n')
        for (i in clust) cat(paste('    ',i,'\n'))
        cat('strides\n')
        for (i in stride) cat(paste('    ',i,'\n'))
        cat('vegan ordiplots\n')
        for (i in ordip) cat(paste('    ',i,'\n'))
    } else {
        if (length(df) > 0) {
            cat('data.frames\n')
            for (i in df) {
                 cat(paste('    ',i,'\n'))
                 tmp <- eval(parse(text=i))
                 cat(paste('        nrow = ',nrow(tmp)),'\n')
                 cat(paste('        ncol = ',ncol(tmp)),'\n')
            }
        }
        if (length(dis) > 0) {
            cat('distance/dissimilarity matrices\n')
            for (i in dis) {
                 cat(paste('    ',i,'\n'))
                 tmp <- eval(parse(text=i))
                 if (!is.null(attr(tmp,'call'))) {
                     str <- c(attr(tmp,'call'))
                     cat(paste('        call     = ',str,'\n'))
                 }
                 cat(paste('        size     = ',attr(tmp,'Size'),'\n'))
                 if (!is.null(attr(tmp,'method')))
                      cat(paste('        method   = ',attr(tmp,'method'),'\n'))
            }
        }
        if (length(ord) > 0) {
        cat('ordinations\n')
            for (i in ord) {
                cat(paste('    ',i,'\n'))
                tmp <- eval(parse(text=i))
                cat(paste('        type  = ',tmp$type,'\n'))
                cat(paste('        dim   = ',ncol(tmp$points)),'\n')
            }
        }
        if (length(ordip) > 0) {
            cat('vegan ordiplot\n')
            for (i in ordip) {
                cat(paste('    ',i,'\n'))
                tmp <- eval(parse(text=i))
                cat(paste('        dim   = ',ncol(tmp$sites),'\n'))
            }
        }
        if (length(clust) > 0) {
        cat('classifications\n')
            for (i in clust) {
                tmp <- eval(parse(text=i))
                cat(paste('    ',i,'\n'))
                if (inherits(tmp,'hclust')) {
                    cat(paste('        dis    = ',tmp$dist.method,'\n'))
                    cat(paste('        method = ',tmp$method,'\n'))
                } else if (inherits(tmp,'partana')) {
                    cat(paste('        dis    = ',attr(tmp,'call')[[3]],'\n'))
                    cat(paste('        numclu = ',attr(tmp,'call')[[2]],'\n'))
                    cat(paste('        numitr = ',tmp$numitr,'\n'))
                    cat(paste('        ratio  = ',round(tmp$ratio[tmp$numitr],2),'\n'))
                } else if (inherits(tmp,'partition')) {
                    cat(paste('        dis    = ',tmp$call[[2]],'\n'))
                    cat(paste('        method = ',attr(tmp,'class')[[1]],'\n'))
                    cat(paste('        numclu = ',tmp$call[[3]],'\n'))
                }
            }
        }
    }
}

