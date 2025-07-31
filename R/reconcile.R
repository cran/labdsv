reconcile <- function (comm, site, exlist = 10) 
{
    if (identical(row.names(comm), row.names(site))) {
        message("You're good to go")
    }
    else {
        orig_comm <- deparse(substitute(comm))
        orig_site <- deparse(substitute(site))
        extracomm <- nrow(comm) - sum(row.names(comm) %in% row.names(site))
        if (extracomm > 0) {
            message(paste("You have", extracomm, "plots in comm not in site"))
            if (extracomm <= exlist) 
                print(row.names(comm)[!row.names(comm) %in% row.names(site)])
            message("I'll delete the extra plots in comm in the output")
        }
        extrasite <- nrow(site) - sum(row.names(site) %in% row.names(comm))
        if (extrasite > 0) {
            message(paste("You have", extrasite, "plots in site not in comm"))
            if (extrasite <= exlist) 
                print(row.names(site)[!row.names(site) %in% row.names(comm)])
            message("I'll delete the extra plots in site in the output")
        }
        if (!extracomm && !extrasite) {
            message("Your data.frames have the same sample units but are sorted differently")
            message("I'll fix that")
        }
        if (!extracomm || !extrasite) {
            message("Your edited data.frames now have the same sample units but are sorted differently")
            message("I'll fix that")
        }
        comm <- comm[order(row.names(comm)), ]
        site <- site[order(row.names(site)), ]
        comm <- comm[row.names(comm) %in% row.names(site), ]
        site <- site[row.names(site) %in% row.names(comm), ]
        out <- list(comm = comm, site = site)
        attr(out, "call") <- match.call()
        attr(out, "orig_comm") <- orig_comm
        attr(out, "orig_site") <- orig_site
        invisible(out)
    }
}

