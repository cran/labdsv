reconcile <- function (taxa,site)
{
    if (identical(row.names(taxa),row.names(site))) {
        cat("You're good to go\n")
        invisible()
    }
    else {
        extra <- nrow(taxa) - sum(row.names(taxa) %in% row.names(site))
        if (extra > 0) cat(paste("You have",extra,"plots in taxa not in site\n"))
        extra <- nrow(site) - sum(row.names(site) %in% row.names(taxa))
        if (extra > 0) cat(paste("You have",extra,"plots in site not in taxa\n"))
        taxa <- taxa[order(row.names(taxa)),]
        site <- site[order(row.names(site)),]
        taxa <- taxa[row.names(taxa) %in% row.names(site),]
        site <- site[row.names(site) %in% row.names(taxa),]
        out <- list(taxa=taxa,site=site)
    }
}

