=======
Utilise
=======




detachAllPackages
-----------------



Detach all the libraries except the official start-up libraries. Need
it in development packages.

.. code-block:: R

    ##' Detach all packages 
    ##'
    ##' 
    ##' @title utilities
    ##' @return nothing 
    ##' @export 
    ##' @author Yi Tang
    detachAllPackages <- function() {

        basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")

        package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]

        package.list <- setdiff(package.list,basic.packages)

        if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)

    }

Object Size
-----------



Show all the objects and their size, watch out your R memory usage!

.. code-block:: R

    lsos()

.. code-block:: R

    # improved list of objects
    .ls.objects <- function (pos = 1, pattern, order.by,
                            decreasing=FALSE, head=FALSE, n=5) {
        napply <- function(names, fn) sapply(names, function(x)
                                             fn(get(x, pos = pos)))
        names <- ls(pos = pos, pattern = pattern)
        obj.class <- napply(names, function(x) as.character(class(x))[1])
        obj.mode <- napply(names, mode)
        obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
        obj.size <- napply(names, object.size)
        obj.dim <- t(napply(names, function(x)
                            as.numeric(dim(x))[1:2]))
        vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
        obj.dim[vec, 1] <- napply(names, length)[vec]
        out <- data.frame(obj.type, obj.size, obj.dim)
        names(out) <- c("Type", "Size", "Rows", "Columns")
        if (!missing(order.by))
            out <- out[order(out[[order.by]], decreasing=decreasing), ]
        if (head)
            out <- head(out, n)
        out
    }
    # shorthand
    lsos <- function(..., n=10) {
        .ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
    }

IO
--




.. code-block:: R

    #' save ggplot to a file which is right to be imported in World document. 
    my.png <- function(p, file = deparse(substitute(p))){
        png(paste(file, ".png", sep=""),  width = 12, height = 9, units = 'in', res = 300)
        print(p)
        dev.off()
    }

estimate file size 

.. code-block:: R

    ##'  Estimate the space of output file
    ##'
    ##' more accurate than the object.size function. 
    ##' @title 
    ##' @param a.df a data.table or data.frame
    ##' @param n number of rows used in estimation, if n is less than 1, propoertion of rows used 
    ##' @param ... args passed to write.csv 
    ##' @return a estimated disk space to save a.df, in Mb unit. 
    ##' @export 
    ##' @author Yi Tang
    SizeEst <- function(a.df, n,  ...) {
        file <- tempfile()
        if (n <= 1)
            n <- ceiling(n * nrow(a.df))
        df.subset <- a.df[seq_len(n), ]
        write.csv(df.subset, file = file, ...)
        disk.space <- file.info(file)$size
        file.remove(file)
        disk.space.est <- disk.space  / nrow(df.subset) * nrow(a.df)
        disk.space.est / (2 ^ 20) ## return as MB 
    }

Assorted
--------



.. code-block:: R

    ##'  Tests
    ##'
    ##' Test whether or not two data.table objects are identical. 
    ##' @title 
    ##' @param dt1 
    ##' @param dt2 
    ##' @param verbose 
    ##' @return 
    ##' @export 
    ##' @author Yi Tang
    identical.data.table <- function(dt1, dt2, verbose = TRUE) {
        if (nrow(dt1) != nrow(dt2))
            stop("different number of rows")
        if (ncol(dt1) != ncol(dt2))
            stop("different number of columns")
        if (names(dt1) != names(dt2))
            stop("different column names")
        n <- ncol(dt1)
        sapply(seq_len(n), function(i) {
            cat("\nTest columns", i)
            if(!identical(dt1[[i]], dt2[[i]]))
                stop("col ", i, " is different")
        })
        TRUE
    }
