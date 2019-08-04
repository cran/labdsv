refactor <- function (df) 
{
    for (i in 1:ncol(df)) {
        if (is.factor(df[,i])) df[,i] <- factor(df[,i])
    }
    df
}

