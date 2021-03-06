===
GIS
===




Cross\_X
--------



.. code-block:: R

    #' Check if point(x, y) cross x = a or y = b line or not.
    #' @example
    #' df <- structure(list(long = c(-34.2078157528796, -36.2074309699417,
    #' -38.2792884737378, -40.2170974851064, -42.1780335768454, -44.1455967338515,
    #' -46.4166770126002, -48.2943755413367, -50.0298281808574, -51.3114969986729,
    #' -52.5441609178788, -53.7237533552569, -54.8823832918566, -55.8138670417713,
    #' -56.722942918336, -57.6053794360869, -58.6228611648525, -59.5945636149565,
    #' -60.591387403632, -61.5473518311293, -62.1877994852336, -62.6515082234799,
    #' -63.0336967876415, -63.1144294155782, -63.3581282050358, -63.7885840015858,
    #' -64.1370270967451, -64.5242218821295, -64.7156587993305, -64.5240440927702,
    #' -63.6592485824048, -61.9361108628757, -59.9644036105799, -57.6850668652962,
    #' -55.6269026718672), lat = c(25.220877237296, 25.0449951402138,
    #' 24.2921987727861, 23.7331891023609, 22.882202932219, 21.9955804595675,
    #' 21.3357658016897, 20.7665536331803, 20.4345722631771, 20.1713918488524,
    #' 19.9607061210464, 19.803542676299, 19.6514829290709, 19.2946576063146,
    #' 19.0185917630829, 18.6544443206195, 18.350027781835, 18.4634817877027,
    #' 18.8968767697435, 19.5874567186624, 20.3871126308597, 21.1001662298256,
    #' 21.6360080794085, 22.214195275253, 22.6741813775321, 23.2123449093717,
    #' 24.0670696428687, 25.3393933451918, 27.2736476853658, 29.0676356902004,
    #' 30.8813383987791, 32.49317150619, 33.1745910761416, 34.2677474173933,
    #' 35.0450609158249)), .Names = c("long", "lat"), row.names = c(NA,
    #' -35L))
    #' x.grid <- seq(-70, -30, by = 10)
    #' res <- CrossX(df$long, df$lat, x.grid)
    #' plot(df$long, df$lat)
    #' abline( v = x.grid, col = 2)
    #' points(res$x, res$y, col = ifelse(res$dir == "negative", 2, 3), pch = 19)
    CrossX <- function(x, y, x.grid){
        cat('\n', 'positive means', '\n from left to right or \n bottom to top\n')

        d <- c(0, diff( findInterval(x, x.grid)))
        if (sum(d != 0) == 0)
            return(NULL)
                                            # negative direction
        res1 <- res2 <- NULL
        ind <- which(d < 0)
        if(length(ind) != 0){
            xx <- x.grid[ findInterval(x[ind], x.grid) + 1]
            if (length(ind) == 1){
                ind <- c(ind - 1, ind)
            } else {
                ind[1] <- ind[1] -1  # otherwise, the first interpolated will be NA.
            }
            res1 <- approx(x[ind], y[ind], xx)
            res1$dir <- "negative"
        }

                                            # positive direction
        ind <- which(d > 0)
        if(length(ind) != 0){
            xx <- x.grid[ findInterval(x[ind], x.grid) ]
            if (length(ind) == 1){
                ind <- c(ind - 1, ind)
            } else {
                ind[1] <- ind[1] -1  # otherwise, the first interpolated will be NA.
            }
            res2 <- approx(x[ind], y[ind], xx)
            res2$dir <- "positive"
        }

        res <- rbind( as.data.table(res1), as.data.table(res2))
        res
    }
