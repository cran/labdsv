vardist <- function (x) 
{
    if (!is.numeric(x)) {
        stop("Only defined for numeric variables")
    }
    y <- cbind(as.double(x),as.double(x))
    orddist(y)
}
