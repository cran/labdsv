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

loadings <- function(x, ...)
{
  UseMethod("loadings")
}
varplot <- function(x, ...)
{
    UseMethod("varplot")
}

scores <- function(x, ...)
{
  UseMethod("scores")
}

calibrate <- function(ord, ...)
{
    UseMethod("calibrate")
}
