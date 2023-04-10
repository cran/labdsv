matrify <- function(data,strata=FALSE,base=100)
{
    if (ncol(data) != 3) stop('data frame must have three column format')
    data <- data[!is.na(data[,1]),]
    data <- data[!is.na(data[,2]),]
    data <- data[!is.na(data[,3]),]
    plt <- data[,1]
    spc <- data[,2]
    abu <- as.numeric(data[,3])
    plt.codes <- levels(factor(plt))
    spc.codes <- levels(factor(spc))
    comm <- matrix(0,nrow=length(plt.codes),ncol=length(spc.codes))
    row <- match(plt,plt.codes)
    col <- match(spc,spc.codes)
    if (!strata) {
        for (i in 1:length(abu)) {
            comm[row[i],col[i]] <- comm[row[i],col[i]] + abu[i]
        }
    } else {
        for (i in 1:length(abu)) {
            if (comm[row[i],col[i]] == 0)  
                comm[row[i],col[i]] <- abu[i]
            else {
                tmp <- 1 - comm[row[i],col[i]]/base
                val <- tmp * (1 - abu[i]/base)
                comm[row[i],col[i]] <- (1 - val) * base
            }
        }
    }            
     
    comm <- data.frame(comm)
    names(comm) <- spc.codes
    row.names(comm) <- plt.codes
    comm
}
