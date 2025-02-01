##' Fits simple gravity model
##'
##' @param dt data.table of flows
##' @param offset logical, if TRUE frompop and topop enter into model
##'     as offset, meaning their coefficients are constrained to 1
##' @return fit
##' @export fit_gravity
##' @author Konstantin Hoffie
fit_gravity <- function(dt, offset) {
    if(offset == TRUE) {
        fit <- stats::glm(flows ~ offset(log(frompop)) +
                       offset(log(topop)) + log(distance),
                   family = stats::poisson, data = dt)
    }
    if(offset == FALSE) {
        fit <- stats::glm(flows ~ log(frompop) +
                       log(topop) + log(distance),
                   family = stats::poisson, data = dt)
    }
    return(fit)
}

## fit_zeroinfl <- function(dt) {
##     fit <- pscl::zeroinfl(actual ~ log(frompop) + log(topop) + log(distance),
##                           dist = "poisson", link = "logit", data = dt #, start = c(1, 1, -2)
##                           )
##     return(fit)
## }

