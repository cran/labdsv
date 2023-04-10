isamic <- function (comm,clustering,sort=FALSE) 
{
    tmp <- const(comm,clustering)
    result <- apply(tmp,1,function(x)
        {2*sum(abs(as.numeric(x)-0.5))/ncol(tmp)})
    if (sort) 
        result <- rev(sort(result))
    result
}

isamic.stride <- function(stride,comm)
{
    spc <- ncol(comm)
    clusts <- ncol(stride$clustering)
    res <- matrix(0,nrow=clusts,ncol=spc)

    for (i in 1:clusts) {
        res[i,] <- isamic(comm,stride$clustering[,i])
    }
    res <- data.frame(res)
    names(res) <- names(comm)
    row.names(res) <- stride$seq
    res
}
                   
    
