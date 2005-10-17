disana <- function (x)
{
    if (class(x) == "dist") {
        y <- as.matrix(x)
        triang <- x
    }
    else {
        y <- as.matrix(x)
        triang <- y[row(y) > col(y)]
    }
    is.na(diag(y)) <- TRUE 
    tmin <- apply(y,1,function(z){min(z,na.rm=TRUE)})
    tavg <- apply(y,1,function(z){mean(z,na.rm=TRUE)})
    tmax <- apply(y,1,function(z){max(z,na.rm=TRUE)})
    plots <- NULL
    plot(sort(triang), xlab = "Sorted Value", ylab = "Dissimilarity")
    readline("Press return for next page....")
    plot(sort(tmin), ylim = c(0, max(tmax)), xlab = "Sorted Plot",
            ylab = "Dissimilarity")
    points(sort(tavg), col = 2)
    points(sort(tmax), col = 3)
    readline("Press return for next page....")
    plot(tmin, tavg, xlab = "Minimum Dissimilarity",
            ylab = "Average Dissimilarity")
    lines(c(0.5, 0.5), c(min(tavg), max(tavg)), col = 2)
    yorn <- readline("Do you want to identify individual plots [Y or N] : ")
    if (yorn == 'Y' || yorn == 'y') 
               plots <- identify(tmin,tavg,attr(x,'Labels'))
    res <- list(min = tmin, mean = tavg, max = tmax, plots = plots)
    invisible(res)
}
