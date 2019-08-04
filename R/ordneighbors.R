ordneighbors <- function (ord,dis,numnbr=1,ax=1,ay=2,digits=5,length=0.1)
{
    if (!inherits(ord,'dsvord')) 
        stop("The first argument must be an object of class 'dsvord'")

    mat <- as.matrix(dis)
    diag(mat) <- 999
    sum <- 0
    neighbors <- 0

    x <- ord$points[,ax]
    y <- ord$points[,ay]

    for (i in 1:nrow(mat)) {
        tmp <- rank(mat[i,])
        nbrs <- which(tmp <= numnbr)
        if (length(nbrs) == 0) 
            nbrs <- which(mat[i,] == min(mat[i,]))
        for (j in nbrs) {
            arrows(x[i],y[i],x[j],y[j],
                length=length,col=2)
            sum <- sum + sqrt((x[i]-x[j])^2 + (y[i]-y[j])^2)
            neighbors <- neighbors + 1
        }
    }

    meannbr <- sum/neighbors
    meandis <- mean(dist(ord$points))

    cat(paste("Mean distance to neighbor = ",
              round(meannbr,digits),"\n"))
    cat(paste("Mean matrix distance      = ",
              round(meandis,digits),"\n"))
    cat(paste("Ratio                     = ",
              round(meannbr/meandis,digits),"\n"))
    out <- list(meaninbrdist=round(meannbr,digits),
                meanmatdist=round(meandis,digits),
                ratio=as.numeric(round(meannbr/meandis,digits)))
    invisible(out)
}

