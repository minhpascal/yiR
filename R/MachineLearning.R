##' @author: Yi Tang
##' @date: 2016-04-20 06:39:27

PrepFit <- function(df, y.name) {
    df <- as.data.frame(df)
    names(df)[names(df) == y.name] <- "y.name"
    set.seed(1)
    n <- nrow(df)
    train <- sample(n, n %/% 2)
    X <<- model.matrix(y.name ~ ., data = df[train, ])[,-1]
    Y <<- df[train, ][["y.name"]]
    X.test <<- model.matrix(as.formula(y.name ~ .), data = df[-train, ])[,-1]
    Y.test <<- df[-train, ][["y.name"]]
    train.dat <<- data.table(X, y.name = Y)
    setnames(train.dat, "y.name", y.name)
    test.dat <<- data.table(X.test, y.name = Y.test)
    setnames(test.dat, "y.name", y.name)
}
