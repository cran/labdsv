dropplt <- function (taxa,site) 
{
    if (!identical(row.names(taxa),row.names(site))) stop('data frames do not match')

    keep <- apply(site,1,function(x){!any(is.na(x))})
    taxa <- taxa[keep,]
    site <- site[keep,]
    list(taxa=taxa,site=site)
}

dropspc <- function (taxa,min=0) 
{
    taxa <- taxa[,apply(taxa>0,2,sum)>min]
    taxa
}

