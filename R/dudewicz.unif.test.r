dudewicz.unif.test <- function(x, nrepl=2000, m=length(x) %/% 2)
{
    DNAME <- deparse(substitute(x))
    l <- 0
    n <- length(x)
	statistic <- function(arr) {
		arr <- sort(arr)
		a <- arr[1]
		b <- arr[n]
		arr <- append(rep(a, m), append(arr, rep(b, m)))
    	return(-mean(log(n / 2 / m * (arr[c(1:n) + 2 * m] - arr[c(1:n)]), 2)))
	}
    h <- statistic(x)
	for (i in 1:nrepl)
    {
        z <- runif(n)
        H <- statistic(z)
        if (H > h) l = l + 1
    }
    p.value <- l / nrepl
    RVAL <- list(statistic = c(H = h), p.value = p.value, method = "Dudewicz-van der Meulen test for uniformity", data.name = DNAME)
    class(RVAL) <- "htest"
    return(RVAL)
}