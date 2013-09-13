isamic <- function (taxa,clustering,sort=FALSE) 
{
    if (inherits(clustering,c('partana','clustering','partition'))) 
        clustering <- clustering$clustering
    if (is.numeric(clustering)) {
        if (min(clustering)< 0 || (length(table(clustering)) != max(clustering))) {
            cat('WARNING: renumbering clusters to consecutive integers\n')
            clustering <- match(clustering,sort(unique(clustering)))
        }
    }
    tmp <- const(taxa,clustering)
    result <- apply(tmp,1,function(x){2*sum(abs(as.numeric(x)-0.5))/ncol(tmp)})
    if (sort) 
        result <- rev(sort(result))
    result
}

