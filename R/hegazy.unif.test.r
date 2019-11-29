GetStatistic <- function(x, n, p)
{
    return(sum((abs(x - c(1 : n) / (n + 1))) ^ p) / n)
}

hegazy.unif.test <- function(x, nrepl=2000, p=1)
{
    DNAME <- deparse(substitute(x))
    sum <- 0
    n <- length(x)
    x <- sort(x)
    t <- GetStatistic(x, n, p)
    for (i in 1 : nrepl)
    {
        z <- runif(n)
        z <- sort(z)
        T <- GetStatistic(z, n, p)
        if (T > t)
        {
            sum <- sum + 1
        }
    }
    p.value <- sum / nrepl
    RVAL <- list(statistic = c(T = t), p.value = p.value,
        method = "Hegazy-Green test for uniformity", data.name = DNAME)
    class(RVAL) <- "htest"
    return(RVAL)
}
