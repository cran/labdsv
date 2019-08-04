thull.dsvord <- function (ord,var,grain,ax=1,ay=2,col=2,
                        grid=51,nlevels=5,
                        levels=NULL,lty=1,numitr=100,...) 
{
    if (!inherits(ord,"dsvord")) {
        stop("You must supply an object of class dsvord")
    }
    if(missing(var)) {
        stop("You must specify a variable to surface")
    }
    if (is.null(var)) {
        stop("No such variable")
    }
    x <- ord$points[,ax]
    y <- ord$points[,ay]
    if (any(is.na(var))) {
        cat("Omitting plots with missing values \n")
        x <- x[!is.na(var)]
        y <- y[!is.na(var)]
        var <- var[!is.na(var)]
    }
    new.x <- seq(min(x),max(x),len=grid)
    new.y <- seq(min(y),max(y),len=grid)
    hull <- matrix(0,nrow=grid,ncol=grid)

    res <- .Fortran('thull',
                    hull=as.double(hull),
                    as.double(new.x),
                    as.double(new.y),
                    as.integer(grid),
                    as.double(x),
                    as.double(y),
                    as.double(var),
                    as.integer(length(x)),
                    as.double(grain),
                    PACKAGE='labdsv')

    if (is.null(levels)) {
        vals <- levels(factor(var))
        levels <- as.numeric(vals)[-1]
    }

    contour(x=new.x,y=new.y,z=matrix(res$hull,nrow=grid),
        add=TRUE,col=col,nlevels=nlevels,levels=levels,lty=lty)
    final <- matrix(res$hull,nrow=grid)
    out <- list(thull=final,x=new.x,y=new.y,ax=x,ay=y,vals=var,
           xlab=paste(ord$type,ax),ylab=paste(ord$type,ay),
           main=deparse(substitute(var)))

    if (numitr > 0) {
        obssum <- sum(final)
        rndsum <- rep(NA,numitr-1)
        for (i in 1:(numitr-1)) {
                res <- .Fortran('thull',
                    hull=as.double(hull),
                    as.double(new.x),
                    as.double(new.y),
                    as.integer(grid),
                    as.double(x),
                    as.double(y),
                    as.double(sample(var,replace=FALSE)),
                    as.integer(length(x)),
                    as.double(grain),
                    PACKAGE='labdsv')
            rndsum[i] <- sum(res$hull) 
        } 
        cat(paste('\nvolume   = ',format(obssum,digits=5),'\nmean     = ',
                   format(mean(rndsum),digit=5),
                   '\nfraction = ',format(obssum/mean(rndsum),digits=5)))
        cat(paste('\np <= ',(sum(rndsum<=obssum)+1)/numitr),'\n')
        out$obs <- obssum
        out$reps <- rndsum
    }
    class(out) <- 'thull'
    attr(out,'call') <- match.call()
    attr(out,'timestamp') <- date()
    invisible(out)
}

plot.thull <- function (x,col=rainbow(20),levels=NULL,cont=TRUE,
          xlab=x$xlab,ylab=x$ylab,main=x$main,...) 
{
    if (!inherits(x,'thull')) 
        stop("You must pass an argument of type 'thull'")
    if (is.null(levels)) {
        vals <- levels(factor(x$vals))
        levels <- as.numeric(vals)[-1]
    }
    image(x$x,x$y,x$thull,col=col,asp=1,xlab=xlab,ylab=ylab,main=main)
    if (cont)
        contour(x$x,x$y,x$thull,levels=levels,nlevels=length(levels),add=TRUE)
}

summary.thull <- function(object,...)
{
    if (!inherits(object,'thull'))
        stop("You must pass an object of class 'thull'")

    cat(paste('\nvolume   = ',format(object$obs,digits=5),'\nmean     = ',
                   format(mean(object$reps),digit=5),
                   '\nfraction = ',format(object$obs/mean(object$reps),digits=5)))
    cat(paste('\np       <= ',format(
              (sum(object$reps<=object$obs)+1)/length(object$reps+1)
              ,digits=2),'\n'))
    cat(paste('\ncall    = ',deparse(attr(object,'call')),'\n'))
    cat(paste('created = ',attr(object,'timestamp'),'\n'))

}

