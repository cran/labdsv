abundtrans <- function (comm,code,value)
{
    if (!is.data.frame(comm)) {
        comm <- data.frame(comm)
    }
    if (length(code) != length(value)) {
        stop("code and value vectors must be of the same length")
    }
    if (is.numeric(code)) {
        code <- c(0,code)
    } else {
        code <- c('0',code)
    }
    if (is.numeric(value)) {
        value <- c(0,value)
    } else { 
        value <- c('0',value)
    }
    newcomm <- matrix(NA,nrow=nrow(comm),ncol=ncol(comm))

    for (i in 1:length(code)) newcomm[comm==code[i]] <- value[i]
    newcomm <- data.frame(newcomm)
    names(newcomm) <- names(comm)
    row.names(newcomm) <- row.names(comm)
    if (any(is.na(newcomm))) {
        print("WARNING, not all values specified")
    }
    return(newcomm)
}
