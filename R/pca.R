pca <- function(mat, cor=FALSE, dim=min(nrow(mat),ncol(mat)))
{
    tmp <- prcomp(mat, retx=TRUE, center=TRUE, scale=cor)
    out <- list()
    out$scores <- tmp$x[,1:dim]
    out$points <- tmp$x[,1:dim]
    out$loadings <- tmp$rotation[,1:dim]
    out$sdev <- tmp$sdev[1:dim]
    out$totdev <- sum(tmp$sdev^2)
    class(out) <- c("dsvord","pca")
    out$type <- 'PCA'
    return(out)
}
                

summary.pca <- function(object, dim=length(object$sdev), ...)
{
    vars <- object$sdev^2
    vars <- vars/object$totdev
    cat("Importance of components:\n")
    print(rbind("Standard deviation" = object$sdev[1:dim],
        "Proportion of Variance" = vars[1:dim],
        "Cumulative Proportion" = cumsum(vars[1:dim])))
}

scores.pca <- function (x,labels=NULL,dim=length(x$sdev),...) 
{
    if (dim>length(x$sdev)) {
        cat("Only",length(x$sdev)," axes available\n")
        dim <- length(x$sdev)
    }
    if (!is.null(labels)) {
        cbind(labels,x$scores[,1:dim])
    } else {
        x$scores[,1:dim]
    }
}

loadings.pca <- function (x, dim=length(x$sdev), digits=3, cutoff=0.1, ...)
{
    if (dim>ncol(x$loadings)) {
        cat("Only",ncol(x$loadings),"axes available\n")
        dim <- ncol(x$loadings)
    }
    cat("\nLoadings:\n")
    cx <- format(round(x$loadings[,1:dim], digits = digits))
    cx[abs(x$loadings[,1:dim]) < cutoff] <- substring("       ",1, nchar(cx[1, 1]))
    print(cx, quote = FALSE)
    invisible()
}

#loadings.default <- function(x, ...)
#{
#    stats::loadings(x, ...)
#}

varplot.pca <- function(x,dim=length(x$sdev),...) 
{
    var <- x$sdev^2
    barplot(var[1:dim],ylab="Variance")
    readline("Hit Return to Continue\n")
    barplot(cumsum(var/x$totdev)[1:dim],ylab="Cumulative Variance")
}

