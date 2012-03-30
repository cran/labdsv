homoteneity <- function (taxa,clustering) 
{
    if (inherits(clustering, c("partana", "partition", "clustering"))) 
        clustering <- clustering$clustering
    numtyp <- length(table(clustering))
    homo <- rep(NA,numtyp)
    S <- mean(apply(taxa>0,1,sum))
    const <- const(taxa,clustering)
    for (i in 1:numtyp) {
        tmp <- as.numeric(rev(sort(const[,i])))
        homo[i] <- mean(tmp[1:S])
    }
    out <- data.frame(as.character(1:numtyp),homo)
    names(out) <- c('cluster','homoteneity')
    out
}

