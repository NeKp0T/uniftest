kek.unif.test <- function(x, nrepl=2000)
{   
    statistic <- function(x) {
        return(sum(qnorm(x) ^ 2))
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
                 method = "Kek test for uniformity", data.name = data.name)
    class(RVAL) <- "htest"
    return(RVAL)
}

kek.unif.test(c(0.1, 0.2, 0.4, 0.3, 0.8, 0.3, 0.6, 0.9))