confus <- function(class,fitted)
{
    cfsmat <- matrix(0,nrow=ncol(fitted),ncol=ncol(fitted))
    rowsum <- rep(0,ncol(fitted))
    colsum <- rep(0,ncol(fitted))
    if (is.logical(class)) class <- as.numeric(factor(class))
    correct <- 0.0
    kappa <- 0.0
    out <- .Fortran("confus",
        as.integer(class),
        fitted,
        as.integer(length(class)),
        as.integer(ncol(fitted)),
        matrix=as.integer(cfsmat),
        as.integer(rowsum),
        as.integer(colsum),
        correct=correct,
        kappa=kappa,
        PACKAGE='labdsv')
    list(matrix=matrix(out$matrix,ncol=ncol(fitted)),
        correct=out$correct,kappa=out$kappa,
        legend="actual as rows, predicted as columns")
}
