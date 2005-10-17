ordtest <- function (ord,var,dim=c(1:ncol(ord$points)),index='euclidean',nitr=1000) 
{
    if (!inherits(ord, c("pco", "nmds", "metaMDS"))) 
        stop('ordtest is only defines for pco, nmds, and metaMDS objects')
    tdist <- 0
    observed <- 0
    reps <- rep(0,nitr-1)
    var <- factor(var)
    for (i in levels(var)) {
        mask <- var == i
        tdist <- tdist + sum(dist(ord$points[mask,dim],index))
    }
    observed <- tdist
    for (i in 1:(nitr-1)) {
        tdist <- 0
        var <- sample(var,length(var),replace=FALSE)
        for (j in levels(var)) {
            mask <- var == j
            tdist <- tdist + sum(dist(ord$points[mask,dim],index))
        }
        reps[i] <- tdist
    }
    reps
    out <- list(obs=observed,p=(sum(reps<=observed)+1)/nitr,
                reps=reps)
    out
}
