abuocc <- function(veg,minabu=0)
{
    spc.plt <- apply(veg>minabu,1,sum)
    plt.spc <- apply(veg>minabu,2,sum)

    if (minabu==0) {
        mean.abu <- apply(veg,2,sum)/plt.spc
    } else {
        mean.abu <- rep(0,ncol(veg))
        for (i in 1:ncol(veg)) {
            mask <- veg[,i]>minabu
            mean.abu[i] <- sum(veg[mask,i]) / max(1,plt.spc[i])
        }
    }
    mean.abu[is.na(mean.abu)] <- 0

    plot(rev(sort(plt.spc[plt.spc>minabu])),log="y",xlab="Species Rank",ylab="Number of Plots",
        main="Species Occurrence")
    readline("Press return for next plot")

    plot(rev(sort(spc.plt)),xlab="Plot Rank",ylab="Number of Species",
        main="Species/Plot")
    readline("Press return for next plot")

    plot(plt.spc[mean.abu>minabu],mean.abu[mean.abu>minabu],log="y",xlab="Number of Plots",ylab="Mean Abundance",
        main="Abundance vs Occurrence")
    yorn <- readline("Do you want to identify individual species? Y/N :")
    if (yorn == 'Y' || yorn == 'y') 
        identify(plt.spc[mean.abu>minabu],mean.abu[mean.abu>minabu],names(veg))
    readline("Press return for next plot")

    plot(spc.plt,apply(veg,1,sum),xlab="Number of Species/Plot",
        ylab="Total Abundance")
    yorn <- readline("Do you want to identify individual plots? Y/N :")
    if (yorn == 'Y' || yorn == 'y') 
        identify(spc.plt,apply(veg,1,sum),labels=row.names(veg))

    invisible(list(spc.plt=spc.plt,plt.spc=plt.spc,mean=mean.abu))
}

