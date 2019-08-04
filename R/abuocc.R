abuocc <- function (comm, minabu = 0, panel='all')
{
    if (!is.data.frame(comm))
        comm <- data.frame(comm)
    spc.plt <- apply(comm > minabu, 1, sum)
    plt.spc <- apply(comm > minabu, 2, sum)
    if (minabu == 0) {
        mean.abu <- apply(comm, 2, sum)/plt.spc
    } else {
        mean.abu <- rep(0, ncol(comm))
        for (i in 1:ncol(comm)) {
            mask <- comm[, i] > minabu
            mean.abu[i] <- sum(comm[mask, i])/max(1, plt.spc[i])
        }
    }
    mean.abu[is.na(mean.abu)] <- 0
    if (panel=='all' || panel==1) {
        plot(rev(sort(plt.spc[plt.spc > minabu])), log = "y", 
            xlab = "Species Rank",
            ylab = "Number of Plots", main = "Species Occurrence")
        if (panel == 'all') readline("Press return for next plot ")
    }
    if (panel=='all' || panel==2) {
        plot(rev(sort(spc.plt)), xlab = "Plot Rank", ylab = "Number of Species",
            main = "Species/Plot")
        if (panel=='all') readline("Press return for next plot ")
    }
    if (panel=='all' || panel==3) {
        plot(plt.spc[mean.abu > minabu], mean.abu[mean.abu > minabu],
            log = "y", xlab = "Number of Plots", ylab = "Mean Abundance",
            main = "Abundance vs Occurrence")
        yorn <- readline("Do you want to identify individual species? Y/N : ")
        if (yorn == "Y" || yorn == "y")
            identify(plt.spc[mean.abu > minabu], mean.abu[mean.abu >
                minabu], names(comm)[mean.abu > minabu])
        if (panel=='all') readline("Press return for next plot ")
    }
    if (panel=='all' || panel==4) {
        plot(spc.plt, apply(comm, 1, sum), xlab = "Number of Species/Plot",
            ylab = "Total Abundance")
        yorn <- readline("Do you want to identify individual plots? Y/N : ")
        if (yorn == "Y" || yorn == "y")
            identify(spc.plt, apply(comm, 1, sum), labels = row.names(comm))
    }
    out <- list(spc.plt = spc.plt, plt.spc = plt.spc, mean = mean.abu)
    attr(out,'call') <- match.call()
    attr(out,'comm') <- deparse(substitute(comm))
    attr(out,'timestamp') <- date()
    attr(out,'class') <- 'abuocc'
    invisible(out)
}

print.abuocc <- function(x,...)
{
    cat("\nSpecies Richness\n\n")
    print(x$spc.plt)

    cat("\nSpecies Statistics\n\n")
    tmp <- data.frame(x$plt.spc,x$mean)
    names(tmp) <- c("Occurrences","Mean Abundance")
    print(tmp)
}

