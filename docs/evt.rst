===
EVT
===




.. code-block:: R

    ##' Transfer Laplace distirbution to original scale 
    ##'
    ##' Given a upper part of x, GPD is used to extrpolate . for y <= qu, empirical transformation. for y > qu, GPD 
    ##' @title EVT
    ##' @param y numeric vector. Laplace distirbution 
    ##' @param x numeric vector, original distirbution 
    ##' @param qu numeric from 0 to 1. 
    ##' @param coef numeric vector of 3 element. GPD parameters 
    ##' @export 
    ##' @author Yi Tang
    y2x <- function(y, x, qu, coef){
        u <- rank(y, ties = "random")/(1+length(y))
        xx <- rep(NA, len = length(u))
        threshold <- quantile(x, qu)
        ind <- u <= qu
        xx[ind] <- quantile(x, u[ind])
        if (any(!ind )){
            sig <- coef[2]
            xi <- coef[3]
            xx[!ind] <- texmex::qgpd(1- (1 - u[!ind]) / (1 - qu), sigma = sig, xi = xi, u = 0) + threshold
        }
        return(xx)
    }

    ##' Convert GPD distribution to GEV distribution 
    ##'
    ##' 
    ##' @title EVT
    ##' @param mu location parameter of GPD 
    ##' @param sigma scale parameter of GPD
    ##' @param xi shape parameter of GPD 
    ##' @param lambda lambda is the average clusters per year
    ##' @return GEV parameter 
    ##' @export 
    ##' @author Yi Tang
    GPD2GEV <- function(mu, sigma, xi, lambda){
        mu = mu + sigma*(lambda^xi - 1) / xi
        sigma = sigma * lambda^xi
        xi = xi
        return(c(mu, sigma, xi))
    }

    ##' Convert return level to return period 
    ##'
    ##' .. content for \details{} ..
    ##' @title EVT 
    ##' @param rl return level 
    ##' @param gev.mle gev parameter 
    ##' @return returnp eriod 
    ##' @export 
    ##' @author Yi Tang
    RL.to.RP <- function(rl, gev.mle){ # function that turns precip level to RP (rearranged)
        rp <-  1 / ( 1 - texmex::pgev(rl, mu = gev.mle[1], sigma = gev.mle[2], xi = gev.mle[3]))
        return(rp) 
    }

    #' automatically choose GPD threshold by coverage 
    optimGPDThreshold <- function(x){
        res <- gpdRangeFit(x, umin = quantile(x, 0.8), umax = quantile(x, 0.995), nint = 100)
        ## plot(res)
        phi <- data.table(th = res$th,
                          mle = res$par[, 1],
                          lb = res$lo[, 1],
                          ub = res$hi[, 1])
        xi <-  data.table(th = res$th,
                          mle = res$par[, 2],
                          lb = res$lo[, 2],
                          ub = res$hi[, 2])
        par <- data.table(rbind(phi, xi), par = rep(c("phi", "xi"), c(nrow(phi), nrow(xi))))
        xi[, n.id := 1:nrow(xi)]
        xi[, n.cover := {
            sum(lb <= xi$mle & ub >= xi$mle)
        }, by = n.id]
        opt.th <- xi[which.max(n.cover), th]
        return(opt.th)
    }
