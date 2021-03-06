isamic <- function (comm,clustering,sort=FALSE) 
{
    tmp <- const(comm,clustering)
    result <- apply(tmp,1,function(x)
        {2*sum(abs(as.numeric(x)-0.5))/ncol(tmp)})
    if (sort) 
        result <- rev(sort(result))
    result
}

