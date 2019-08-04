factorize <- function (df) 
{
    for (i in 1:ncol(df)) {
        if (is.character(df[,i])) df[,i] <- factor(df[,i])
    }
    df
}

defactorize <- function(df)
{
    for (i in 1:ncol(df)) {
        if (is.factor(df[,i])) df[,i] <- as.character(df[,i])
    }
    df
}
