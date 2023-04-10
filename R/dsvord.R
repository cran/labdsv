plot.dsvord <- function(x, ax = 1, ay = 2, col = 1, title = "", pch = 1,
                  xlab = paste(x$type, ax), ylab = paste(x$type, ay), ...)
{
    if (!inherits(x,'dsvord')) 
        stop ("You must provide an object of class dsvord")
    xlab <- paste(x$type,ax)
    ylab <- paste(x$type,ay)

    plot(x$points[, ax], x$points[, ay], asp = 1,
        col = col, xlab = xlab, ylab = ylab,
        pch = pch, main = title, ...)

    invisible()
}

points.dsvord <- function(x, which, ax = 1, ay = 2, col = 2,  pch = 1, 
                          cex=1, breaks=FALSE, ...)
{
    if (!inherits(x,'dsvord'))
        stop("You must supply an object of class 'dsvord'")

    xp <- x$points[which, ax]
    yp <- x$points[which, ay]

    if (is.logical(which)) {
        points(xp, yp, col = col, pch = pch, cex = cex, ...)
    } else if (is.numeric(which)) {
        if (breaks) {
            mask <- !is.na(which)
            cex <- (which-min(which[mask])) / (max(which[mask])-min(which[mask])) * 5
        } else {
            cex <- 1
        }
        points(xp, yp, col = col, pch = pch, cex = cex, ...)
    }
}

hilight.dsvord <- function (ord, overlay, ax=1, ay=2, title="", 
                            cols=c(2,3,4,5,6,7), glyph=c(1,3,5), ...)
{
    if (!inherits(ord,'dsvord')) 
        stop("You must pass an object of class 'dsvord'")
    overlay <- as.integer(clustify(overlay))

    plot(ord,ax=ax,ay=ay,type='n',...)
    title(title)
    layer <- 0
    pass <- 1
    for (i in 1:max(overlay,na.rm=TRUE)) {
        layer <- layer + 1
        if (layer > length(cols)) {
          layer <- 1
          pass <- pass + 1
        }
        col <- cols[layer]
        pch <- glyph[pass]
        points(ord, overlay == i, ax, ay, col = col, pch = pch)
    }
}


plotid.dsvord <- function(ord, ids=seq(1:nrow(ord$points)), ax = 1, 
                          ay = 2, col = 1, ...)
{
    if (!inherits(ord,'dsvord')) 
        stop("You must supply an object of class 'dsvord'")
    identify(ord$points[, ax],ord$points[, ay],ids,col=col)
}


surf.dsvord <- function(ord, var, ax=1, ay=2, thinplate=TRUE, col=2,
     labcex = 0.8, lty = 1, family=gaussian, gamma=1.0, grid=50, ...)
{
    if (!inherits(ord,'dsvord')) 
        stop("You must supply an object of class 'dsvord'")
    if (missing(var)) {
        stop("You must specify a variable to surface")
    }

    x <- ord$points[,ax]
    y <- ord$points[,ay]

    if (any(is.na(var))) {
        cat("Omitting plots with missing values \n")
        x <- x[!is.na(var)]
        y <- y[!is.na(var)]
        var <- var[!is.na(var)]
    }
    if (is.logical(var)) {
        tvar <- as.numeric(var)
        if (thinplate) tmp <- gam(tvar~s(x,y),family=binomial,gamma=gamma)
        else  tmp <- gam(tvar~s(x)+s(y),family=binomial, gamma=gamma)
    } else {
        if (thinplate) tmp <- gam(var~s(x,y),family=family, gamma=gamma)
        else tmp <- gam(var~s(x)+s(y),family=family,gamma=gamma)
    }

    new.x <- seq(min(x),max(x),len=grid)
    new.y <- seq(min(y),max(y),len=grid)
    xy.hull <- chull(x,y)
    xy.hull <- c(xy.hull,xy.hull[1])
    new.xy <- expand.grid(x=new.x,y=new.y)
    inside <- as.logical(pip(new.xy$x,new.xy$y,x[xy.hull],y[xy.hull]))
    fit <- predict(tmp, type="response", newdata=as.data.frame(new.xy))
    fit[!inside] <- NA
    contour(x=new.x,y=new.y,z=matrix(fit,nrow=grid),
        add=TRUE,col=col)
    print(tmp)
    d2  <- (tmp$null.deviance-tmp$deviance)/tmp$null.deviance
    cat(paste("D^2 = ",formatC(d2,width=4),"\n"))
    invisible(tmp)
}

chullord.dsvord <- function (ord, overlay, ax = 1, ay = 2, 
                             cols=c(2,3,4,5,6,7), ltys=c(1,2,3), ...)
{
    if (!inherits(ord,'dsvord'))
        stop("You must pass an object of class 'dsvord'")
    overlay <- as.integer(clustify(overlay))

    pass <- 1
    layer <- 0
    lty <- ltys[pass]
    for (i in 1:max(overlay,na.rm=TRUE)) {
        x <- ord$points[,ax][overlay==i & !is.na(overlay)]
        y <- ord$points[,ay][overlay==i & !is.na(overlay)]
        pts <- chull(x,y)
        layer <- layer + 1
        if (layer > length(cols)) {
          layer <- 1
          pass <- min(pass + 1,length(ltys))
        }
        col <- cols[layer]
        lty = ltys[pass]
        polygon(x[pts],y[pts],col=col,density=0,lty=lty,...)
    }
}


ellip.dsvord <- function (ord, overlay, ax = 1, ay = 2, 
                     cols = c(2, 3, 4, 5, 6, 7), ltys = c(1, 2, 3), ...)
{
    if (!inherits(ord,'dsvord'))
        stop("You must pass an object of class 'dsvord'")
    overlay <- as.integer(clustify(overlay))

    pass <- 1
    layer <- 0
    lty <- ltys[pass]
    for (i in 1:max(overlay, na.rm = TRUE)) {
        x <- ord$points[, ax][overlay == i & !is.na(overlay)]
        y <- ord$points[, ay][overlay == i & !is.na(overlay)]
        pts <- chull(x, y)
        layer <- layer + 1
        if (layer > length(cols)) {
            layer <- 1
            pass <- min(pass + 1, length(ltys))
        }
        col <- cols[layer]
        lty <- ltys[pass]
        x <- as.matrix(cbind(x[pts], y[pts]))
        elp <- ellipsoidhull(x,...)
        lines(predict(elp),col=col,...)
    }
}

density.dsvord <- function (ord, overlay, ax = 1, ay = 2, 
                cols = c(2, 3, 4, 5, 6, 7), ltys = c(1, 2, 3), numitr, ...)
{
    if (!inherits(ord,'dsvord')) 
        stop("You must pass an object of class 'dsvord'")
    overlay <- as.integer(clustify(overlay))

    densi <- function(xpts,ypts,overlay) {
        x <- xpts[overlay==1 & !is.na(overlay)]
        y <- ypts[overlay==1 & !is.na(overlay)]
        pts <- chull(x,y)
        a <- c(x,x[1])
        b <- c(y,y[1])
        inside <- pip(xpts,ypts,a,b)
        test <- pmax(inside,overlay==1)
        out <- sum(overlay)/sum(test)
        return(out)
    }

    out <- list()
    for (i in 1:max(overlay, na.rm = TRUE)) {
        obs <- densi(ord$points[, ax],ord$points[, ay], overlay==i)
        pval <- 0
        for (j in 1:(numitr-1)) {
            rnd <- sample(1:length(overlay),sum(overlay==i),replace=FALSE)
            rndvec <- rep(0,length(overlay))
            rndvec[rnd] <- 1
            tmp <- densi(ord$points[, ax],ord$points[, ay], rndvec)
            if (tmp >= obs) pval <- pval + 1
        }
        pval <- (pval+1)/numitr
        print(paste('d = ',obs,'p = ',pval))
    }

}

gamord.dsvord <- function (ord,var,partial=NULL,family='gaussian',thinplate=TRUE) 
{
    ord <- ord$points

    if (any(is.na(var))) {
        cat("Omitting plots with missing values \n")
        ord <- ord[!is.na(var),]
        var <- var[!is.na(var)]
    }

    size <- ncol(ord)

    if (thinplate) {
        if (!is.null(partial)) indep <- 's(partial) + s('
        else indep <- 's('
        for (i in 1:(size-1)) {
            indep <- paste(indep,'ord[,',i,'],',sep='')
        }
        indep <- paste(indep,'ord[,',size,'])',sep='')
        print(paste('gam(var~',indep,',family=family)')) 
    } else {
        if (!is.null(partial)) indep <- 's(partial) + '
        else indep <- ''
        for (i in 1:(size-1)) {
            indep <- paste(indep,'s(ord[,',i,'])+',sep='')
        }
        indep <- paste(indep,'s(ord[,',size,'])',sep='')
        print(paste('gam(var~',indep,',family=family)'))
    }
    res <- eval(parse(text=paste('gam(var~',indep,',family=family)')))
    res
}

summary.dsvord <- function(object, round = 4, ...)
{
    if (!inherits(object,'dsvord'))
    stop("You must pass an argument of type 'dsvord'")

    cat(paste('type       = ',object$type,'\n'))
    cat(paste('dimensions = ',ncol(object$points),'\n'))
    if (inherits(object,'nmds')) 
        cat(paste('stress = ',object$stress,'\n'))
    if (inherits(object,'pco')) {
        cat(paste('GOF = ',round(object$GOF[1],round),'\n'))
        cat(paste('GOF = ',round(object$GOF[2],round),'\n'))
    }
    if (inherits(object,'tsne')) {
        cat(paste('perplexity = ',object$perplexity,'\n'))
        cat(paste('theta      = ',object$theta,'\n'))
        cat(paste('eta        = ',object$eta,'\n'))
        cat(paste('KL-Div     = ',object$KLdiv,'\n'))
    }
    cat(paste('call       = ',deparse(attr(object,'call')),'\n'))
    cat(paste('created    = ',attr(object,'timestamp'),'\n'))
    if (inherits(object,'pca')) {
        summary.pca(object,...)
    }
}

print.dsvord <- function(x,numpts=50,...)
{
    cat(paste('type       = ',x$type,'\n'))
    cat(paste('dimensions = ',ncol(x$points),'\n'))
    cat(paste("\nCall   ",c(attr(x,'call')),"\n"))
    
    if (nrow(x$points) <= numpts) {
        cat("\nPoints\n")
        print(x$points)
    }
}



