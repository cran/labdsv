duarm <- function (veg,class,sort=FALSE) 
{
    tmp <- const(veg,class)
    if (sort) 
        result <- rev(sort(apply(tmp,1,function(x){2*sum(abs(x-0.5))/ncol(tmp)})))
    else
        result <- apply(tmp,1,function(x){2*sum(abs(x-0.5))/ncol(tmp)})
    result <- data.frame(result)
    result
}
