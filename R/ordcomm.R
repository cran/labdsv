ordcomm <- function (comm,site)
{
    print(comm)
    repeat {
        plots <- readline(' enter the plots    : ')
        if (plots == "") {
            break
        } else {
            pnt <- as.numeric(readline(' in front of        : '))
        }

        for (i in strsplit(plots,",")[[1]]){
            ord <- 1:nrow(comm)
            x <- match(i,row.names(comm))
            if (!is.na(x)) {
                ord <- ord[-x]
                y <- match(pnt,row.names(comm[ord,]))
                if (!is.na(y)) {
                        if (y==1) {
                            ord <- c(x,ord)
                        } else {
                            first <- ord[1:(y-1)]
                            last <- ord[y:length(ord)]
                            ord <- c(first,x,last)
                        }
                        comm <- comm[ord,]
                        site <- site[ord,]
                        print(comm)
                    } else {
                        print(paste('plot',pnt,'does not exist'))
                    }
                } else {
                    print(paste('plot',i,'does not exist'))
                }
            }
            repeat {
                species <- readline(' enter the species  : ')
                if (species == "") {
                    break
                } else {
                    pnt <- readline(' in front of        : ')
                }
                for (i in strsplit(species,",")[[1]]){
                    ord <- 1:ncol(comm)
                    x <- match(i,names(comm))
                    if (!is.na(x)) {
                        ord <- ord[-x]
                        y <- match(pnt,names(comm[,ord]))
                        if (!is.na(y)) {
                            if (y==1) {
                                ord <- c(x,ord)
                            } else {
                                first <- ord[1:(y-1)]
                                last <- ord[y:length(ord)]
                                print(first)
                                print(last)
                                ord <- c(first,x,last)
                            }
                            comm <- comm[,ord]
                            print(comm)
                        } else {
                            print(paste('species',pnt,'does not exist'))
                        }
                    } else {
                        print(paste('species',i,'does not exist'))
                    }
                }
            }
        }
    out <- list(comm=comm,site=site)
    invisible(out)
}
