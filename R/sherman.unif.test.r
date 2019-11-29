GetStatistic <- function(x, n)
{
    return(sum(abs(x[c(2 : (n + 2))] - x[c(1 : (n + 1))] - 1 / (n + 1))) / 2)
}

sherman.unif.test <- function(x, nrepl=2000)
{
    DNAME <- deparse(substitute(x))
    sum <- 0
    n <- length(x)
    x <- c(0, sort(x), 1)
    w <- GetStatistic(x, n)
    for (i in 1 : nrepl)
    {
        z <- runif(n)
        z <- c(0, sort(z), 1)
        W <- GetStatistic(z, n)
        if (W > w)
        {
            sum <- sum + 1
        }
    }
    p.value <- sum / nrepl
    RVAL <- list(statistic = c(W = w), p.value = p.value,
        method = "Sherman test for uniformity", data.name = DNAME)
    class(RVAL) <- "htest"
    return(RVAL)
}