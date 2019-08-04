neighbors <- function (dis,numnbr=1)
{ 
    mat <- as.matrix(dis)
    diag(mat) <- 999

    out <- matrix(NA,nrow=nrow(mat),ncol=numnbr)

    for (i in 1:nrow(mat)) {
        tmp <- rank(mat[i,])
                nbrs <- which(tmp <= numnbr)
        if (length(nbrs) == 0)
            nbrs <- which(mat[i,] == min(mat[i,]))[1:numnbr]
        nbrs <- nbrs[order(mat[i,nbrs])]
        out[i,] <- nbrs[1:numnbr]
    }
    out <- data.frame(out)
    row.names(out) <- attr(dis,'Labels')
    names(out) <- as.character(1:numnbr)
    out
}

