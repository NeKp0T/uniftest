dudewicz.unif.test <- function(x, nrepl=2000, m=length(x) %/% 2) {   
    statistic <- function(arr) {
        n <- length(arr)
        arr <- sort(arr)
        a <- arr[1]
        b <- arr[n]
        arr <- append(rep(a, m), append(arr, rep(b, m)))
        return(-mean(log(n / 2 / m * (arr[c(1:n) + 2 * m] - arr[c(1:n)]), 2)))
    }
    
    h <- statistic(x)
    data.name <- deparse(substitute(x))
    x.length = length(x) 
    
    monte_carlo <- function(statistic.value, statistic.function, point.number, nrepl=nrepl) {
        successes = 0
        for (i in 1 : nrepl) {
            if (statistic.function(runif(point.number)) > statistic.value) {
                successes <- successes + 1
            }
        }
        
        return(successes / nrepl) 
    }
    
    p.value <- monte_carlo(h, statistic, x.length, nrepl)
    RVAL <- list(statistic = c(H = h), p.value = p.value, 
                 method = "Dudewicz-van der Meulen test for uniformity", data.name = data.name)    
    class(RVAL) <- "htest"
    return(RVAL)
}