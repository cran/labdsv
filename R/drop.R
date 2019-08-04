dropplt <- function (comm,site,which=NULL) 
{
    if (!identical(row.names(comm),row.names(site))) stop('data frames do not match')

    orig_comm <- deparse(substitute(comm))
    orig_site <- deparse(substitute(site))
    if (is.null(which)) {
        keep <- apply(site,1,function(x){!any(is.na(x))})
    } else {
        keep <- 1:nrow(comm)
        keep <- keep[-which]
    }
    comm <- comm[keep,]
    site <- site[keep,]
    res <- list(comm=comm,site=site)
    attr(res,'call') <- match.call()
    attr(res,'orig_comm') <- orig_comm
    attr(res,'orig_site') <- orig_site
    res
}

dropspc <- function (comm,minocc=0,minabu=0) 
{
    comm <- comm[,apply(comm>minabu,2,sum)>minocc]
    attr(comm,'call') <- match.call()
    attr(comm,'minocc') <- minocc
    attr(comm,'minabu') <- minabu
    comm
}

