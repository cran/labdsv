rndveg <- function(veg,replace=FALSE,species=FALSE,plots=FALSE)
{
    if (species) {
        out <- apply(veg,2,sample)
    }
    if (plots) {
        out <- apply(veg,1,sample)
    }
    if (!species & !plots) {
        tmp <- as.vector(as.matrix(veg))
        out <- as.data.frame(matrix(sample(tmp,replace=replace),ncol=ncol(veg)))
    }
    as.data.frame(out)
}

