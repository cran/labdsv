gsr <- function (field,old,new)
{
    if (length(old) != length(new)) 
            stop("replacement vectors must be teh same length")
    newfield <- as.character(field)
    if (length(old)==1) {
        newfield[newfield==old] <- new
    } else {
        for (i in 1:length(old)) newfield[newfield==old[i]] <- new[i]
    }

    if (is.factor(field)) newfield <- factor(newfield)
    if (is.numeric(field)) newfield <- as.numeric(newfield)
    return(newfield)
}
