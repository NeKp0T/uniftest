GetNeyman <- function(polynomials, x, n, k)
{
    sum <- 0
    for (i in 1 : k)
    {
        sum <- sum + (sum(sqrt(2) * polynomial.values(polynomials[i + 1], 2 * x - 1)[[1]]) / sqrt(n)) ^ 2
    }
    return(sum)
}

neyman.unif.test <- function(x, nrepl = 2000, k = 5)
{
    DNAME <- deparse(substitute(x))
    n <- length(x)
    x <- sort(x)
    polynomials <- legendre.polynomials(k, normalized = TRUE)
    initNeyman <- GetNeyman(polynomials, x, n, k)

    sum <- 0
    for (i in 1 : nrepl)
    {
        z <- runif(n)
        z <- sort(z)
        currentNeyman <- GetNeyman(polynomials, z, n, k)
        if (currentNeyman > initNeyman)
        {
            sum <- sum + 1
        }
    }
    p.value <- sum / nrepl
    RVAL <- list(statistic = c(N = initNeyman), p.value = p.value, method = "Neyman test for uniformity", data.name = DNAME)
    class(RVAL) <- "htest"
    return(RVAL)
}
