tsne <- function (dis, k = 2, perplexity = 30, theta = 0.0, eta = 200)
{
    if (!inherits(dis,'dist')) 
        stop("You must pass an object of class 'dist' as the first argument")

    tmp <- Rtsne(dis, dims= k, perplexity = perplexity, theta = theta, eta = eta,
        is_distance = TRUE)
    out=list(points=tmp$Y,type='t-SNE',perplexity=tmp$perplexity,theta=theta,
             eta=eta,KLdiv=tail(tmp$itercosts,1))
    class(out) <- c('dsvord','tsne')
    attr(out,'call') <- match.call()
    attr(out,'timestamp') <- date()
    out
}


besttsne <- function (dis, k = 2, itr = 100, perplexity = 30, theta = 0.0, eta = 200) 
{
    if (!inherits(dis,'dist'))
        stop("You must pass an object of class 'dist' as the first argument")
    if (interactive()) pb <- txtProgressBar(min=0, max=itr, style=3)

    kldiv <- rep(0, itr)
    res <- Rtsne(dis, dims = k, perplexity = perplexity, theta = theta, eta = eta,
        is_distance = TRUE)
    kldiv[1] <- tail(res$itercosts, 1)
    minkld <- kldiv[1]
    best <- 1
    for (i in 2:itr) {
        tmp <- Rtsne(dis, dims = k, perplexity = perplexity, 
            theta = theta, eta = eta, is_distance = TRUE)
        kldiv[i] <- tail(tmp$itercosts, 1)
        if (kldiv[i] < minkld) {
            minkld <- kldiv[i]
            best <- i
            res <- tmp
        }
        if (interactive()) setTxtProgressBar(pb,i)
    }
    if (interactive()) close(pb)
    print(kldiv)
    cat(paste("\nbest result =", best))
    cat(paste("\nwith KL-div =",format(kldiv[best],4),"\n"))
    out <- list(points=res$Y,type='t-SNE')
    class(out) <- c('dsvord','tsne')
    attr(out,'perplexity') <- res$perplexity
    attr(out,'theta') <- res$theta
    attr(out,'eta') <- res$eta
    attr(out,'KL-div') <- kldiv[best]
    attr(out,'call') <- match.call()
    attr(out,'timestamp') <- date()
    out
}

