surf <- function(ord,...)
{
    UseMethod("surf")
}

plotid <- function(ord,...)
{
    UseMethod("plotid")
}

specid <- function(ord,...)
{
    UseMethod("specid")
}

hilight <- function(ord, ...)
{
    UseMethod("hilight")
}

chullord <- function(ord, ...)
{
    UseMethod("chullord")
}

thull <- function(ord, ...)
{
    UseMethod("thull")
}

density <- function(ord, ...)
{
    UseMethod("density")
}

ellip <- function(ord, ...)
{
    UseMethod("ellip")
}

rgl <- function(ord, ...)
{  
  UseMethod("rgl")
}

loadings <- function(pca, ...)
{
  UseMethod("loadings")
}
varplot <- function(pca, ...)
{
    UseMethod("varplot")
}

scores <- function(pca, ...)
{
  UseMethod("scores")
}

calibrate <- function(dsvord, ...)
{
    UseMethod("calibrate")
}
