#' Individual Change
#'
#' In Svensson's method, the individual change is described by the relative rank variance (RV),
#' the observable part, and the internal rank variance (IV), the unobservable part, together.
#' A measure of the closeness of observations to the rank transformable pattern of change is
#' defined as the augmented correlation coefficient (ralpha) and its p-value.
#'
#' @param t The contengency table for Svensson's method, a two-dimension matrix.
#' @name indichange
#' @return \code{rv} and \code{iv} give the RV and IV value. \code{rvse} gives the standard
#' error of RV. \code{ralpha} and \code{pralpha} give the augmented correlation coefficient
#' and the corresponding p-value.
#' @importFrom stats pnorm
#' @seealso \code{\link{con_ta}} for generating contengency table. \code{\link{syschange}}
#' for systematic change.
#' @examples
#' x <- c (1:5,5:1)
#' y <- c(1:5,1,1,5,4,1)
#' z <- con_ta(x,y,)
#' rv(z)
#' rvse(z)
#' iv(z)
#' ralpha(z)
#' pralpha(z)
NULL
# > NULL

#' @rdname indichange
#' @export
rv <- function(t) {
    ch <- function(t) {
        d = dim(t)[1]
        for (i in 1:(floor(d/2))) {
            temp = t[i]
            t[i] = t[d + 1 - i]
            t[d + 1 - i] = temp
        }
        return(t)
    }
    Rij1 <- function(t) {
        l = dim(t)[1]
        d = numeric(l * l)
        dim(d) = c(l, l)
        Csum = apply(t, 2, sum)
        Ccum = cumsum(Csum)
        for (i in 1:l) {
            for (j in 1:l) {
                if (t[i, j] == 0) {
                  d[i, j] = 0
                } else {
                  d[i, j] = Ccum[j] - sum(t[1:i, j]) + (t[i, j] + 1)/2
                }
            }
        }
        return(d)
    }
    Rij2 <- function(t) {
        l = dim(t)[1]
        d = numeric(l * l)
        dim(d) = c(l, l)
        RRsum = c(ch(as.matrix(apply(t, 1, sum))))
        RRcum = cumsum(RRsum)
        for (i in 1:l) {
            for (j in 1:l) {
                if (t[i, j] == 0) {
                  d[i, j] = 0
                } else {
                  d[i, j] = RRcum[l + 1 - i] - sum(t[i, j:l]) + (t[i, j] + 1)/2
                }
            }
        }
        return(d)
    }
    RV = sum((Rij1(t) - Rij2(t))^2 * t) * 6/sum(t)^3
    return(RV)
}



#' @rdname indichange
#' @export
rvse <- function(t) {
    rvk <- function(t) {
        l = nrow(t)
        y = t
        h = matrix(0, l, l)
        for (i in 1:l) {
            for (j in 1:l) {
                if (y[i, j] != 0) {
                  y[i, j] = y[i, j] - 1  #deleted one observation
                  h[i, j] = rv(y)  #rpk: the rp with one observation deleted
                }
                y = t
            }
        }
        return(h)
    }
    n = sum(t)
    rvkvec = c(rvk(t))
    tvec = c(t)  #make the original matrix become vectors
    rvd = sum(rvkvec * tvec)/n  #mean of rpk
    b = grep(0, tvec)  #find the position of all the zeros
    jvarrv = (n - 1)/n * sum(tvec[-b] * (rvkvec[-b] - rvd)^2)
    jserv = sqrt(jvarrv)
    RVSE = (n - 1)^2 * jserv/(n^2)
    return(RVSE)
}

#' @rdname indichange
#' @export
iv <- function(t) {
    IV = sum(t^3 - t)/sum(t)^3
    return(IV)
}

#' @rdname indichange
#' @export
ralpha <- function(t) {
    n = sum(t)
    ralpha = 1 - n^3 * rv(t)/((n^3 - n) - n^3 * iv(t))
    return(ralpha)
}

#' @rdname indichange
#' @export
pralpha <- function(t) {
    z = (ralpha(t) - 1) * sqrt(sum(t))
    p = pnorm(-abs(z))
    return(p)
}


