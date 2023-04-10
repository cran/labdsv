as.dsvord <- function(obj)
{ 
#     ltm2dsv <- function (ltm)
#     {
#         test <- requireNamespace(reo)
#         if (!test) stop('package reo must be installed')
#         out <- list()
#         out$points <- scores(ltm)
#         out$type <- 'LTM'
#         class(out) <- c('dsvord','ltm')
#         out
#     }

    lvs2dsv <- function (lvs,alpha=0.5)
    {
        testcov <- lvs$lv.median %*% t(lvs$lv.coefs.median[, 2:(lvs$num.lv +
                1)])
        do.svd <- svd(testcov, lvs$num.lv, lvs$num.lv)
        choose.lvs <- scale(do.svd$u * matrix(do.svd$d[1:lvs$num.lv]^alpha,
            nrow = lvs$n, ncol = lvs$num.lv, byrow = TRUE), center = TRUE,
            scale = FALSE)
        out <- list()
        out$points <- choose.lvs
        out$type <- 'LVS'
        class(out) <- c('dsvord','lvs')
        out
    }

    tsne2dsv <- function(tsne)
    {
        out <- list()
        out$points <- tsne$Y
        out$type <- 't-SNE'
        out$perplexity <- tsne$perplexity
        out$theta <- tsne$theta
        out$eta <- tsne$eta
        out$KLdiv <- tail(tsne$itercosts,1)
        class(out) <- c('dsvord','tsne')
        attr(out,'call') <- attr(tsne,'call')
        out
    }

    meta2dsv <- function(obj)
    {
        out <- list()
        out$points <- obj$points
        out$type <- 'NMDS'
        class(out) <- c('dsvord','metamds')
        out
    }

    ordip2dsv <- function(obj)
    {
        out <- list()
        out$points <- obj$sites
        if (!is.null(obj$species)) out$species <- obj$species
        tmp <- dimnames(obj$sites)[[2]][1]
        out$type <- substring(tmp,1,nchar(tmp)-1)
        out$stress <- obj$stress * 100
        class(out) <- c('dsvord','ordip')
    }

    dsv2dsv <- function(obj)
    {
        out <- list()
        out$points <- obj$points
        out$type <- class(obj)
        if (inherits(obj,'nmds'))
            out$stress <- obj$stress
        if (inherits(obj,'pco'))
            out$GOF <- obj$GOF
        class(out) <- c('dsvord',class(obj))
        out
    }

    pca2dsv <- function(obj)
    {
        out <- list()
        out$scores <- obj$scores
        out$points <- obj$scores
        out$loadings <- obj$loadings
        out$sdev <- obj$sdev
        out$totdev <- obj$totdev
        class(out) <- c("dsvord","pca")
        out$type <- 'PCA'
        out
    }
   
    mfso2dsv <- function(obj)
    {
        out<- list()
        out$points <- obj$mu
        out$type='MFSO'
        class(out) <- c('dsvord')
        out
    }
    cca2dsv <- function(obj)
    {
        out <- list()
        dims <- ncol(obj$CCA$u)
        out$points <- obj$CCA$wa
        out$type <- toupper(class(obj)[1])
        attr(out,'class') <- c('dsvord','cca')
        out
    }

    
    ca2dsv <- function(obj)
    {
        out <- list()
        dims <- ncol(obj$CA$u)
        out$points <- obj$CA$u
        out$type <- 'CA'
        attr(out,'class') <- c('dsvord','ca')
        out
    }
    dca2dsv <- function(obj)
    {
        out <- list()
        dims <- 4
        out$points <- obj$rproj
        out$type <- 'DCA'
        attr(out,'class') <- c('dsvord','dca')
        out
    }

    if (inherits(obj,c('nmds','pco'))) {
        out <- dsv2dsv(obj)
    } else if (inherits(obj,'ltm.ecol')) {
        #out <- ltm2dsv(obj)
        stop('ltm is not currently supported')
    } else if (inherits(obj,'boral')) {
        out <- lvs2dsv(obj)
    } else if (inherits(obj,'metaMDS')) {
        out <- meta2dsv(obj) 
    } else if ('perplexity' %in% names(obj)) {
        out <- tsne2dsv(obj)
    } else if (inherits(obj,'ordiplot')) {
        out <- ordip2dsv(obj)
    } else if (inherits(obj,'pca')) {
        out<- pca2dsv(obj)
    } else if (inherits(obj,'mfso')) {
        out <- mfso2dsv(obj)
    } else if (inherits(obj,'cca'))  {
        if (is.null(nrow(obj$CCA$u))) {
            out <- ca2dsv(obj)
        } else {
            out <- cca2dsv(obj)
        }
    } else if (inherits(obj,'decorana')) {
        out <- dca2dsv(obj)
    } else {
        stop("object class not recognized")
    }

    attr(out,'call') <- match.call()
    attr(out,'timestamp') <- date()
    out
}

