rgl.nmds <- function (ord,ax=1,ay=2,az=3,radius=0.01,col=0) 
{
    require(rgl)
    if (ncol(ord$points) < 3) stop("Must be 3-D")
    tmp <- ord$points[,c(ax,ay,az)]

    midp <- c(0,0,0)

    rgl.clear()
    rgl.lines(range(ord$points[,ax]),c(midp[2],midp[2]),c(midp[3],midp[3]))
    rgl.lines(c(midp[1],midp[1]),range(ord$points[,ay]),c(midp[3],midp[3]))
    rgl.lines(c(midp[1],midp[1]),c(midp[2],midp[2]),range(ord$points[,az]))
     
    rgl.texts(1.01 * max(tmp[, 1]), midp[2], midp[3], as.character(ax), adj = 0.5)
    rgl.texts(midp[1], 1.01 * max(tmp[, 2]), midp[3], as.character(ay), adj = 0.5)
    rgl.texts(midp[1], midp[2],1.01 * max(tmp[, 3]), as.character(az), adj = 0.5)

    rgl.points(tmp)
    rgl.spheres(tmp,radius=radius,col=col)
}
