#' Percentage Agreement
#'
#' The percentage agreement (PA) which shows the proposion of the subjects who did not change their choices.
#'
#' @param t The contengency table for Svensson's method, a two-dimension matrix.
#' @return \code{pa} gives the PA value, multiply by 100 to get a percentage number.
#' @seealso \code{\link{con_ta}} for generating contengency table. \code{\link{sresult}} for summary of
#' Svensson's method analysis.
#' @examples
#' x <- c (1:5,5:1)
#' y <- c(1:5,1,1,5,4,1)
#' z <- con_ta(x,y,)
#' pa(z)
#' @export
pa <- function(t) {
    l = dim(t)[1]
    dsum = 0
    for (i in 1:l) {
        dsum = t[l + 1 - i, i] + dsum
    }
    pa = dsum/sum(t)
    return(pa)
}
