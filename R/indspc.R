indspc <- function(veg,dis) 
{
    if (class(dis) != 'dist') stop("Must pass a dist object")
    tmp <- 1 - as.matrix(dis)
    out <- rep(0,ncol(veg))
    for (i in 1:ncol(veg)) {
        mask <- veg[,i] > 0
        if (sum(mask) > 1) {
            x <- as.matrix(tmp[mask,mask])
            out[i] <- mean(x[row(x)>col(x)])
        }
        else {
            out[i] <- 0.0
        }
    }
    x <- data.frame(out,apply(veg>0,2,sum))
    names(x) <- c('indval','numocc')
    x
}
