#' Systematic Change
#'
#' The value and the standard error of relative position (RP), the systematic change in
#' position between the two ordered categorical classification.
#' Also, the value and the standard error of relative concentration (RC), a comprehensive
#' evaluation of the systematic change.
#'
#' @param t The contengency table for Svensson's method, a two-dimension matrix.
#' @name syschange
#' @return \code{rp} and \code{rc} give the RP and RC value. \code{rpse} and \code{rcse}
#' give the standard error of RP and RC.
#' @seealso \code{\link{con_ta}} for generating contengency table. \code{\link{indichange}}
#' for individual change. \code{\link{sresult}} for summary of Svensson's method analysis.
#' @examples
#' x <- c (1:5,5:1)
#' y <- c(1:5,1,1,5,4,1)
#' z <- con_ta(x,y,)
#' rp(z)
#' rpse(z)
#' rc(z)
#' rcse(z)
NULL
# > NULL

#' @rdname syschange
#' @export
rp <- function(t) {
    ch <- function(t) {
        d = dim(t)[1]
        for (i in 1:(floor(d/2))) {
            temp = t[i]
            t[i] = t[d + 1 - i]
            t[d + 1 - i] = temp
        }
        return(t)
    }
    Rsum = ch(as.matrix(apply(t, 1, sum)))  #yi
    Csum = as.matrix(apply(t, 2, sum))  #xi
    Tsum = sum(t)  #n
    Rcum = c(0, cumsum(Rsum)[-dim(t)[1]])  #C(y)i-1
    Ccum = c(0, cumsum(Csum)[-dim(t)[1]])  #C(x)i-1
    P0 = sum(Rsum * Ccum)/Tsum^2
    P1 = sum(Rcum * Csum)/Tsum^2
    RP = P0 - P1
    return(RP)
}

#' @rdname syschange
#' @export
rpse <- function(t) {
    rpk <- function(t) {
        l = nrow(t)
        y = t
        h = matrix(0, l, l)
        for (i in 1:l) {
            for (j in 1:l) {
                if (y[i, j] != 0) {
                  y[i, j] = y[i, j] - 1  #deleted one observation
                  h[i, j] = rp(y)  #rpk: the rp with one observation deleted
                }
                y = t
            }
        }
        return(h)
    }
    n = sum(t)
    rpkvec = c(rpk(t))
    tvec = c(t)  #make the original matrix become vectors
    rpd = sum(rpkvec * tvec)/n  #mean of rpk
    b = grep(0, tvec)  #find the position of all the zeros
    jvarrp = (n - 1)/n * sum(tvec[-b] * (rpkvec[-b] - rpd)^2)
    jserp = sqrt(jvarrp)
    RPSE = (n - 1) * jserp/n
    return(RPSE)
}

#' @rdname syschange
#' @export
rc <- function(t) {
    ch <- function(t) {
        d = dim(t)[1]
        for (i in 1:(floor(d/2))) {
            temp = t[i]
            t[i] = t[d + 1 - i]
            t[d + 1 - i] = temp
        }
        return(t)
    }
    Rsum = ch(as.matrix(apply(t, 1, sum)))  #yi
    Csum = as.matrix(apply(t, 2, sum))  #xi
    Tsum = sum(t)
    Rcum = c(0, cumsum(Rsum)[-dim(t)[1]])  #C(y)i-1
    Rcum1 = cumsum(Rsum)  #C(y)i
    Ccum = c(0, cumsum(Csum)[-dim(t)[1]])  #C(x)i-1
    Ccum1 = cumsum(Csum)  #C(x)i
    P0 = sum(Rsum * Ccum)/Tsum^2
    P1 = sum(Rcum * Csum)/Tsum^2
    M = min(P0 - P0^2, P1 - P1^2)
    RC = sum(Rsum * Ccum * (Tsum - Ccum1) - Csum * Rcum * (Tsum - Rcum1))/(M * Tsum^3)
    return(RC)
}


#' @rdname syschange
#' @export
rcse <- function(t) {
    rck <- function(t) {
        l = nrow(t)
        y = t
        h = matrix(0, l, l)
        for (i in 1:l) {
            for (j in 1:l) {
                if (y[i, j] != 0) {
                  y[i, j] = y[i, j] - 1  #deleted one observation
                  h[i, j] = rc(y)  #rpk: the rp with one observation deleted
                }
                y = t
            }
        }
        return(h)
    }
    n = sum(t)
    rckvec = c(rck(t))
    tvec = c(t)  #make the original matrix become vectors
    rcd = sum(rckvec * tvec)/n  #mean of rpk
    b = grep(0, tvec)  #find the position of all the zeros
    jvarrc = (n - 1)/n * sum(tvec[-b] * (rckvec[-b] - rcd)^2)
    jserc = sqrt(jvarrc)
    RCSE = (n - 1) * jserc/n
    return(RCSE)
}
