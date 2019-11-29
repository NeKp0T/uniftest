GetStatistic <- function(x, n)
{
    d <- (x - c(1 : n) / (n + 1)) / (c(1 : n) * (n - c(1 : n) + 1))
    return(n * n * sum(d ^ 2) - n * (sum(d)) ^ 2)
}

sarkadi.unif.test <- function(x, nrepl=2000)
{
    DNAME <- deparse(substitute(x))
    sum <- 0
    n <- length(x)
    x <- sort(x)
    j <- GetStatistic(x, n)
    for (i in 1 : nrepl)
    {
        z <- runif(n)
        z <- sort(z)
        J <- GetStatistic(z, n)
        if (J > j)
        {
            sum <- sum + 1
        }
    }
    p.value <- sum / nrepl
    RVAL <- list(statistic = c(J = j), p.value = p.value,
        method = "Sarkadi-Kosik test for uniformity", data.name = DNAME)
    class(RVAL) <- "htest"
    return(RVAL)
}
