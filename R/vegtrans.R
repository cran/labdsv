vegtrans <- function (x,a,b)
{
    y <- matrix(-1,nrow=nrow(x),ncol=ncol(x))
    if (length(a) != length(b)) stop ("Transformation vectors must be the same length")
    if (!is.numeric(a) | !is.numeric(b) | !is.numeric(x)) stop
        ("All values must be numeric")
    numplt <- nrow(x)
    numspc <- ncol(x)
    numval <- length(a)
    tmp <-.Fortran("vegtrans",
        as.matrix(x),
        y=y,
        a,
        b,
        as.integer(numplt),
        as.integer(numspc),
        as.integer(numval),
        PACKAGE='labdsv')$y
    if (any(tmp<0)) {
        cat(paste("Value ",veg[tmp==-1],"not converted.  Set to NA\n"))
        tmp[tmp==-1] <- NaN
    }
    tmp <- as.data.frame(tmp)
    names(tmp) <- names(x)
    row.names(tmp) <- row.names(x)
    tmp
}

stdveg <- function (x,pltsum=NULL,spcmax=NULL) 
{
    if (!is.null(pltsum)) {
       tmp <- apply(x,1,sum)
       x <- sweep(x,1,tmp,"/")
       x <- sweep(x,1,pltsum,"*")
    }
    if (!is.null(spcmax)) {
       tmp <- apply(x,2,max)
       x <- sweep(x,2,tmp,"/")
    } 
    x
}

