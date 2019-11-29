kolmogorov.unif.test <- function(x, nrepl=2000, k=0)
{
    DNAME <- deparse(substitute(x))
    sum <- 0
    n <- length(x)
    if (k == 1) {
        d <- max(c(1 : n) / (n + 1) - x)
        for (i in 1 : nrepl)
        {
            z <- runif(n)
            D <- max(c(1 : n) / (n + 1) - z)
            if (D > d) {
                sum <- sum + 1
            }
        }
    }
    if (k == 0) {
        d <- max(abs(c(1 : n) / (n + 1) - x))
        for (i in 1 : nrepl)
        {
            z <- runif(n)
            D <- max(abs(c(1 : n) / (n + 1) - z))
            if (D > d) {
                sum <- sum + 1
            }
        }
    }
    if (k == - 1) {
        d <- max(x - c(1 : n) / (n + 1))
        for (i in 1 : nrepl)
        {
            z <- runif(n)
            D <- max(z - c(1 : n) / (n + 1))
            if (D > d) {
                sum <- sum + 1
            }
        }
    }
    p.value <- sum / nrepl
    RVAL <- list(statistic = c(D = d), p.value = p.value,
        method = "Kolmogorov-Smirnov test for uniformity", data.name = DNAME)
    class(RVAL) <- "htest"
    return(RVAL)
}
