GetStatistic <- function(x, n)
{
    return(max(x - c(0 : (n - 1)) / n) + max(c(1 : n) / n - x))
}

kuiper.unif.test <- function(x, nrepl=2000)
{
    DNAME <- deparse(substitute(x))
    sum <- 0
    n <- length(x)
    v <- GetStatistic(x, n)
    for (i in 1 : nrepl)
    {
        z <- runif(n)
        V <- GetStatistic(z, n)
        if (V > v)
        {
            sum <- sum + 1
        }
    }
    p.value <- sum / nrepl
    RVAL <- list(statistic = c(V = v), p.value = p.value,
        method = "Kuiper test for uniformity", data.name = DNAME)
    class(RVAL) <- "htest"
    return(RVAL)
}