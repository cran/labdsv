disana <- function (x)
{
    if (!is.null(class(x))) {
        if (class(x) == "dist") {
            y <- as.matrix(x)
            triang <- x
        }
    }
    else {
        y <- as.matrix(x)
        triang <- y[row(y) > col(y)]
    }
    if (max(y) > 1) {
        cat("Converting distance to dissimilarity (max=1.0) \n")
        triang <- triang/max(y)
        y <- y/max(y)
    }
    diag(y) <- -1
    tmin <- apply(y,1,function(z){min(z[z!=-1])})
    tavg <- apply(y,1,function(z){mean(z[z!=-1])})
    tmax <- apply(y,1,function(z){max(z[z!=-1])})
    plots <- NULL
    plot(sort(triang), xlab = "Sorted Value", ylab = "Dissimilarity")
    readline("Press return for next page....")
    plot(sort(tmin), ylim = c(0, 1), xlab = "Sorted Plot",
            ylab = "Dissimilarity")
    points(sort(tavg), col = 2)
    points(sort(tmax), col = 3)
    readline("Press return for next page....")
    plot(tmin, tavg, xlab = "Minimum Dissimilarity",
            ylab = "Average Dissimilarity")
    lines(c(0.5, 0.5), c(min(tavg), max(tavg)), col = 2)
    yorn <- readline("Do you want to identify individual plots [Y or N]")
    if (yorn == 'Y' || yorn == 'y') plots <- identify(tmin,tavg,row.names(veg))
    res <- list(min = tmin, mean = tavg, max = tmax, plots = plots)
    invisible(res)
}
