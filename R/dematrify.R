dematrify <- function (comm, filename, sep = ",", thresh = 0) 
{
    tmp <- which(comm > thresh, arr.ind = TRUE)
    samples <- row.names(tmp)
    species <- names(comm)[tmp[, 2]]
    abund <- comm[tmp]
    ord <- order(tmp[, 1], tmp[, 2])
    result <- data.frame(samples[ord], species[ord], abund[ord])
    names(result) <- c("sample", "species", "abundance")
    attr(result,'call') <- match.call()
    attr(result,'comm') <- deparse(substitute(comm))
    attr(result,'thresh') <- thresh
    if (missing(filename)) {
        return(result)
    } else {
        write.table(file = filename, result, sep = sep, quote = FALSE, 
            row.names = FALSE)
    }
}
