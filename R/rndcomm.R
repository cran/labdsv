rndcomm <- function(comm,replace=FALSE,species=FALSE,plots=FALSE)
{
    if (species) {
        out <- apply(comm,2,sample,replace=replace)
    }
    if (plots) {
        out <- apply(comm,1,sample,replace=replace)
    }
    if (!species & !plots) {
        tmp <- as.vector(as.matrix(comm))
        out <- as.data.frame(matrix(sample(tmp,replace=replace),ncol=ncol(comm)))
    }
    as.data.frame(out)
}

