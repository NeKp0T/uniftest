GetStatistic <- function(x, n)
{
    return (sum((x[c(2 : (n + 2))] - x[c(1 : (n + 1))]) ^ 2) +
        sum((x[c(2 : (n + 1))] - x[c(1 : n)]) * (x[c(3 : (n + 2))] - x[c(2 : (n + 1))])))
}

quesenberry.unif.test <- function(x, nrepl=2000)
{
    DNAME <- deparse(substitute(x))
    sum <- 0
    n <- length(x)
    x <- c(0, sort(x), 1)
    q <- GetStatistic(x, n)
    for (i in 1 : nrepl)
    {
        z <- runif(n)
        z <- c(0, sort(z), 1)
        Q <- GetStatistic(z, n)
        if (Q > q)
        {
            sum <- sum + 1
        }
    }
    p.value <- sum / nrepl
    RVAL <- list(statistic = c(Q = q), p.value = p.value,
        method = "Greenwood-Quesenberry-Miller test for uniformity", data.name = DNAME)
    class(RVAL) <- "htest"
    return(RVAL)
}