rndtaxa <- function(taxa,replace=FALSE,species=FALSE,plots=FALSE)
{
    if (species) {
        out <- apply(taxa,2,sample)
    }
    if (plots) {
        out <- apply(taxa,1,sample)
    }
    if (!species & !plots) {
        tmp <- as.vector(as.matrix(taxa))
        out <- as.data.frame(matrix(sample(tmp,replace=replace),ncol=ncol(taxa)))
    }
    as.data.frame(out)
}

