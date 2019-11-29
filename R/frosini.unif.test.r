frosini.unif.test <- function(x, nrepl=2000)
{   
    statistic <- function(x) {
        n <- length(x)
        return(1 / sqrt(n) * sum(abs(sort(x) - (c(1 : n) - 0.5) / n)))
    }
    
    b <- statistic(x)
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
    
    p.value <- monte_carlo(b, statistic, x.length, nrepl)
    RVAL <- list(statistic = c(B = b), p.value = p.value, 
                 method = "Frosini test for uniformity", data.name = data.name)
    class(RVAL) <- "htest"
    return(RVAL)
}