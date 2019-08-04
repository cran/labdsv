pco <- function(dis, k=2)
{
    tmp <-cmdscale(dis,k=k,eig=TRUE)
    class(tmp) <- c("dsvord","pco")
    tmp$type <- "PCO"
    return(tmp)
}
