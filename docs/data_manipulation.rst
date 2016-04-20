=================
Data Manipulation
=================




.. code-block:: R

    rbind.ids <- function(dataX, dataY, cols = NULL, add.id = TRUE){
        require(data.table)
        df.name <- c(deparse(substitute(dataX)), deparse(substitute(dataY)))
        dataX <- as.data.table(dataX)
        dataY <- as.data.table(dataY)
        if (is.null(cols))
            cols <- union(names(dataX), names(dataY))
        tmp <- rbind(dataX[, cols, with=F], dataY[, cols, with = F])
        if (add.id)
            tmp[, join.id := rep( df.name, c(nrow(dataX), nrow(dataY)))]
        tmp
    }

Print the object X in org-mode format. 

.. code-block:: R

    ToOrg <- function(X){
        require(ascii)
        print(ascii(X), type = "org")
    }
