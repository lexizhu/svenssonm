#' Contingency Table Generation
#'
#' Generate Contengency table for Svensson's Method
#'
#' @param x a numeric vector of data values, each element range from 1 to level.
#' @param y a numeric vector of data values, must have same length as x.
#' @param level the dimention of the contengency table, the default is 5.
#' @return A contingency table based on \code{x} and \code{y}.
#' @seealso \code{\link{sresult}} for summary of Svensson's method analysis.
#' @examples
#' x <- c (1:5,5:1)
#' y <- c(1:5,1,1,5,4,1)
#' con_ta(x,y,)
#' @export
con_ta <- function(x, y, level = 5) {
    if (length(x) != length(y)) {
        print("Two vectors' length are not equal!")
    } else {
        z <- cbind(y, x)
        t <- numeric(level * level)
        dim(t) <- c(level, level)
        for (i in 1:level) {
            for (j in 1:level) {
                t[i, j] <- dim(subset(z, z[, 1] == level + 1 - i & z[, 2] == j))[1]
            }
        }
        return(t)
    }
}
