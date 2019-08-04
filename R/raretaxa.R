raretaxa <- function (comm,min=1,log=FALSE,type='b',panel='all')
{ 
    rare <- apply(comm>0,2,sum) <= min
    occ <- apply(comm[,rare]>0,1,sum)
    abu <- apply(comm[,rare],2,sum)/apply(comm[,rare]>0,2,sum)
    tot <- apply(comm[,rare],1,sum)

    if (panel == 'all' || panel == 1) {
        if (log) {
            plot(rev(sort(occ[occ>0])),log='y',type=type,
                 xlab='Plot',ylab='Rare Species/Plot')
        } else {
            plot(rev(sort(occ[occ>0])),type=type,
                xlab='Plot',ylab='Rare Species/Plot')
        }
        if (panel == 'all') readline('Hit return')
    }

    if (panel == 'all' || panel == 2) {
        if (log) {
            plot(rev(sort(abu)),type=type,log='y',xlab='Species',ylab='Mean Abundance')
        } else {
            plot(rev(sort(abu)),type=type,xlab='Species',ylab='Mean Abundance')
        }
        if (panel == 'all')  readline('Hit return')
    }

    if (panel == 'all' || panel == 3) {
        plot(rev(sort(tot[tot>0])),type=type,log='y',xlab='Plot',ylab='Total Abundance')
    }

    out=list(rare=rare,occurence=occ,abundance=abu,total=tot)
    invisible(out)
}
