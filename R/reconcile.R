reconcile <- function (comm, site, exlist = 10) 
{
    if (identical(row.names(comm), row.names(site))) {
        cat("You're good to go\n")
    }
    else {
        orig_comm <- deparse(substitute(comm))
        orig_site <- deparse(substitute(site))
        extracomm <- nrow(comm) - sum(row.names(comm) %in% row.names(site))
        if (extracomm > 0) {
            cat(paste("You have", extracomm, "plots in comm not in site\n"))
            if (extracomm <= exlist) 
                print(row.names(comm)[!row.names(comm) %in% row.names(site)])
            cat("I'll delete the extra plots in comm in the output\n")
        }
        extrasite <- nrow(site) - sum(row.names(site) %in% row.names(comm))
        if (extrasite > 0) {
            cat(paste("You have", extrasite, "plots in site not in comm\n"))
            if (extrasite <= exlist) 
                print(row.names(site)[!row.names(site) %in% row.names(comm)])
            cat("I'll delete the extra plots in site in the output\n")
        }
        if (!extracomm && !extrasite) {
            cat("Your data.frames have the same sample units but are sorted differently\n")
            cat("I'll fix that\n")
        }
        if (!extracomm || !extrasite) {
            cat("Your edited data.frames now have the same sample units but are sorted differently\n")
            cat("I'll fix that\n")
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

