isamic <- function (taxa,clustering,sort=FALSE) 
{
    tmp <- const(taxa,clustering)
    result <- apply(tmp,1,function(x){2*sum(abs(x-0.5))/ncol(tmp)})
    if (sort) 
        result <- rev(sort(result))
    result
}

