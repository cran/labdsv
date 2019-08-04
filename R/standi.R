samptot <- function(comm)
{
    x <- apply(comm,1,sum)
    comm <- sweep(comm,1,x,'/')
    comm
}

spcmax <- function(comm)
{
    x <- apply(comm,2,max)
    comm <- sweep(comm,2,x,'/')
    comm
}

hellinger <- function(comm)
{
    x <- apply(comm,1,sum)
    comm <- sqrt(sweep(comm,1,x,'/'))
    comm
}

convex <- function(n,b=2,stand=FALSE)
{
    res <- b^(n-1) - b^seq(n-1,0)
    if (stand) res <- res/res[n]
    res
}




    
