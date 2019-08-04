clustify <- function (clustering) 
{
    if (inherits(clustering, c("partana", "partition", "clustering"))) {
        clustering <- factor(clustering$clustering)
    }
    else if (is.character(clustering)) {
        clustering <- factor(clustering)
    }
    else if (is.numeric(clustering)) {
        clustering <- factor(clustering)
    }
    else if (is.logical(clustering)) {
        clustering <- factor(clustering)
    }
    else if (!is.factor(clustering)) 
        stop("Cannot understand passed clustering")
    clustering
}

summary.clustering <- function(object,...)
{
    if (!inherits(object,'clustering'))
        stop("You must pass an object of class 'clustering'")
    cat(paste('Number of clusters  = ',length(table(object$clustering)),'\n'))
    print(table(object$clustering))
    cat(paste('\ncall     = ',deparse(attr(object,'call')),'\n'))
    cat(paste('created  = ',attr(object,'timestamp'),'\n')) 
}

print.clustering <- function(x,...)
{
    print(x$clustering)
}
