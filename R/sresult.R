#' Summary for Svensson's Method
#'
#' List all the results for Svensson's Method. Including percentage agreement, systematic change
#' and individual change.
#'
#' @param t The contingency table for Svensson's method, a two-dimension matrix.
#' @return \code{sresult} lists the results for Svensson's method. PA for percentage agreement,
#' RP for relative position, SE(RP) and CI(RP) for the corresponding standard error and 95\% confidence interval,
#' RC for relative concentration, SE(RC) and CI(RC) for the corresponding standard error and 95\% confidence interval,
#' RV for relative rank variance, SE(RV) and CI(RV) for the corresponding standard error and 95\% confidence interval,
#' IV for internal rank variance, R.Alpha for augmented correlation coefficient, P.R.Alpha for the corresponding p-value
#' (significant level 0.05).
#' @seealso \code{\link{con_ta}} for generating contingency table.
#' @examples
#' x <- c (1:5,5:1)
#' y <- c(1:5,1,1,5,4,1)
#' z <- con_ta(x,y,)
#' sresult(z)
#' @export
sresult = function(t) {
    pa = pa(t)

    RP = rp(t)
    RPSE = rpse(t)
    RPCI = c((RP - 1.96 * RPSE), (RP + 1.96 * RPSE))

    RC = rc(t)
    RCSE = rcse(t)
    RCCI = c((RC - 1.96 * RCSE), (RC + 1.96 * RCSE))

    RV = rv(t)
    RVSE = rvse(t)
    RVCI = c((RV - 1.96 * RVSE), (RV + 1.96 * RVSE))

    IV = iv(t)

    R.Alpha = ralpha(t)
    P.R.Alpha = pralpha(t)

    return(list("PA" = pa, "RP" = RP, "SE(RP)" = RPSE, "CI(RP)" = RPCI,
        "RC" = RC, "SE(RC)" = RCSE, "CI(RC)" = RCCI, "RV" = RV, "SE(RV)" = RVSE, "CI(RV)" = RVCI,
        "IV" = IV, "R.Alpha" = R.Alpha, "P.R.Alpha" = P.R.Alpha))
}
