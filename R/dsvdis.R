dsvdis <- function(x, index, weight = rep(1,ncol(x)), step = 0., diag=FALSE, upper=FALSE)
{
    choices <- c("steinhaus", "sorensen", "ochiai", "ruzicka", "bray/curtis", 
                 "roberts", "chisq", "hellinger")
    i <- pmatch(index, choices)
    if(is.na(i))
        stop(paste(index, "is not a valid index:", paste(choices, 
            collapse = ", ")))
    if (!is.loaded("dsvdis")) {
        dyn.load("labdsv")
    }
    commname <- deparse(substitute(x))
    x <- as.matrix(x)
    y <- matrix(0,nrow=nrow(x),ncol=nrow(x))
    rowsum <- rep(0,nrow(x))
    colsum <- rep(0,ncol(x))
    dis <- .Fortran("dsvdis",
        as.double(x),
        as.double(weight),
        as.integer(nrow(x)),
        as.integer(ncol(x)),
        as.integer(i),
        out = as.double(y),
        as.double(step),
        as.double(rowsum),
        as.double(colsum),
        PACKAGE='labdsv')
    tmp <- matrix(dis$out, nrow = nrow(x))
    tmp2 <- as.dist(tmp)
    class(tmp2) <- 'dist'
    attr(tmp2, "Labels") <- dimnames(x)[[1]]
    attr(tmp2, "Diag") <- diag
    attr(tmp2, "Upper") <- upper
    attr(tmp2, "method") <- choices[i]
    attr(tmp2, "call") <- match.call()
    attr(tmp2, "comm") <- commname
    attr(tmp2, "Size") <- nrow(x)
    return(tmp2)
}

summary.dist <- function (object,...) 
{
    if (!inherits(object,'dist')) 
        stop("You must pass an object of class 'dist'")

    if (!is.null(attr(object, "call"))) {
        str <- c(attr(object, "call"))
        cat(paste("call     = ", str, "\n"))
    }
    cat(paste("size     = ", attr(object, "Size"), 
            "\n"))
    if (!is.null(attr(object, "method"))) 
       cat(paste("method   = ", attr(object, "method"), "\n"))
}

