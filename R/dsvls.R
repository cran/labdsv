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
        message('data.frames')
        for (i in df) message(paste('    ',i))
        message('distance/dissimilarity matrices')
        for (i in dis) message(paste('    ',i))
        message('ordinations')
        for (i in ord) message(paste('    ',i))
        message('classifications')
        for (i in clust) message(paste('    ',i,))
        message('strides')
        for (i in stride) message(paste('    ',i))
        message('vegan ordiplots')
        for (i in ordip) message(paste('    ',i))
    } else {
        if (length(df) > 0) {
            message('data.frames')
            for (i in df) {
                 message(paste('    ',i))
                 tmp <- eval(parse(text=i))
                 message(paste('        nrow = ',nrow(tmp)))
                 message(paste('        ncol = ',ncol(tmp)))
            }
        }
        if (length(dis) > 0) {
            message('distance/dissimilarity matrices')
            for (i in dis) {
                 message(paste('    ',i))
                 tmp <- eval(parse(text=i))
                 if (!is.null(attr(tmp,'call'))) {
                     str <- c(attr(tmp,'call'))
                     message(paste('        call     = ',str))
                 }
                 message(paste('        size     = ',attr(tmp,'Size')))
                 if (!is.null(attr(tmp,'method')))
                      message(paste('        method   = ',attr(tmp,'method')))
            }
        }
        if (length(ord) > 0) {
        message('ordinations\n')
            for (i in ord) {
                message(paste('    ',i))
                tmp <- eval(parse(text=i))
                message(paste('        type  = ',tmp$type))
                message(paste('        dim   = ',ncol(tmp$points)))
            }
        }
        if (length(ordip) > 0) {
            message('vegan ordiplot')
            for (i in ordip) {
                message(paste('    ',i))
                tmp <- eval(parse(text=i))
                message(paste('        dim   = ',ncol(tmp$sites)))
            }
        }
        if (length(clust) > 0) {
        message('classifications')
            for (i in clust) {
                tmp <- eval(parse(text=i))
                message(paste('    ',i))
                if (inherits(tmp,'hclust')) {
                    message(paste('        dis    = ',tmp$dist.method))
                    message(paste('        method = ',tmp$method))
                } else if (inherits(tmp,'partana')) {
                    message(paste('        dis    = ',attr(tmp,'call')[[3]]))
                    message(paste('        numclu = ',attr(tmp,'call')[[2]]))
                    message(paste('        numitr = ',tmp$numitr,'\n'))
                    message(paste('        ratio  = ',round(tmp$ratio[tmp$numitr],2)))
                } else if (inherits(tmp,'partition')) {
                    message(paste('        dis    = ',tmp$call[[2]]))
                    message(paste('        method = ',attr(tmp,'class')[[1]]))
                    message(paste('        numclu = ',tmp$call[[3]]))
                }
            }
        }
    }
}

