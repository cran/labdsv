plot.pco <- function(x, ...)
{
   plot(as.dsvord(x,...))
   cat("\nlabdsv 1.X ordinations are deprecated")
   cat("\nUse 'x <- dsvord(x)' to update them\n\n")
} 

plot.pca <- function(x, ...)
{
   plot(as.dsvord(x,...))
   cat("\nlabdsv 1.X ordinations are deprecated")
   cat("\nUse 'x <- dsvord(x)' to update them\n\n")
}

plot.nmds <- function(x, ...)
{
   plot(as.dsvord(x,...))
   cat("\nlabdsv 1.X ordinations are deprecated")
   cat("\nUse 'x <- dsvord(x)' to update them\n\n")
}

