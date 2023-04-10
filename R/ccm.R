ccm <- function (model,data) 
{
    if (nrow(model) != nrow(data)) stop("Data.frames do not match")
    if (ncol(model) != ncol(data)) stop("Data.frames do not match")

    shannon <- function(vec) {
        p <- vec/sum(vec)
        p <- p[p!=0]
        res <- -1 * sum(p * log(p))
        res
    }

    numplt <- nrow(model)
    numspc <- ncol(model)
    sim <- rep(0,numplt)
    divers <- matrix(0,nrow=numplt,ncol=2)

    for (i in 1:numplt) {
        tmp <- cbind(as.numeric(model[i,]),
                     as.numeric(data[i,]))
        num <- 2 * sum(apply(tmp,1,min))
        denom <- sum(tmp)
        sim[i] <- num/denom
        divers[i,1] <- shannon(model[i,])
        divers[i,2] <- shannon(data[i,])
    }

    out <- list(sim=sim,diverse=divers)
    out
}
