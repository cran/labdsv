vardist <- function (x) 
{
    if (!is.numeric(x)) {
        stop("Only defined for numeric variables")
    }
    y <- cbind(x,x)
    orddist(y)
}
