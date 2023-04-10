nmds <- function(dis,k=2,y=cmdscale(d=dis,k=k),maxit=50,trace=FALSE)
{
    if (!inherits(dis,'dist')) stop("You must pass a dist() obhject as the first argument")
    if (!is.numeric(k)) stop("The second argument must be an integer")

    out <- isoMDS(dis,y=y,k=k,maxit=maxit,trace=trace)
    class(out) <- c("dsvord","nmds")
    attr(out,'call') <- match.call()
    attr(out,'timestamp') <- date()
    out$type <- "NMDS"
    return(out)
}

bestnmds <- function (dis,k=2,itr=20,maxit=100,trace=FALSE,pbar=TRUE)
{
    if (!inherits(dis,'dist')) stop("You must pass a dist() object as the first argument")
    if (!is.numeric(k)) stop("The second argument must be an integer")
    if (!is.numeric(itr)) stop("The third argument must be an integer")
    if (interactive() && pbar) pb <- txtProgressBar(min=0, max=itr, style=3)

    strss <- rep(0,itr)
    out <- nmds(dis,k=k,maxit=maxit,trace=trace)
    strss[1] <- out$stress
    minstr <- out$stress
    best <- 1

    for (i in 2:itr) {
        tmp <- nmds(dis,k=k,y=matrix(runif(k*attr(dis,'Size')),ncol=k),
            maxit=maxit,trace=trace)
        strss[i] <- tmp$stress
        if (tmp$stress < minstr) {
            minstr <- tmp$stress
            best <- i
            out <- tmp
        }
        if (interactive() && pbar) setTxtProgressBar(pb,i)
    }
    if (interactive() && pbar) close(pb)
    print(strss)
    cat(paste("\nbest result =", best))
    cat(paste("\nwith stress =",format(out$stress,4),"\n"))
    class(out) <- c("dsvord","nmds")
    attr(out,'call') <- match.call()
    attr(out,'timestamp') <- date()
    out$type = "NMDS"

    out
}
