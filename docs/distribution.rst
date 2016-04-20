============
Distribution
============




Laplace
-------



.. code-block:: R

    ##' Laplace Distribution 
    ##'
    ##' Basic functions that relate to Laplace distribution 
    ##' @name LaplaceDistribution 
    ##' @param x a random variable 
    ##' @param mu para 1 
    ##' @param b para 2 
    ##' @export 
    ##' @author Yi Tang
    toLaplace <- function(x, mu = 0, b = 1){
        ## u <- rank(x, ties = "random") / (1+length(x))
        u <- rank(x) / (1+length(x))
        y <-     mu - b * sign(u - 0.5) * log(1 - 2 * abs(u - 0.5))
        return(y)
    }
    ##' @rdname LaplaceDistribution 
    qLaplace <- function(p, mu = 0, b = 1){
         ## mu is locaton param, b is scale para
        mu - b * sign(p - 0.5) * log(1 - 2 * abs(p - 0.5))
    }
    ##' @rdname LaplaceDistribution 
    pLaplace <- function(x, mu = 0, b = 1){
        ## mu is location para, b is scale para
        1/2 + 1/2 * sign(x - mu) * (1  - exp(- abs(x - mu) / b))
    }
