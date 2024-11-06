##' Corrects flows for administrative changes
##'
##' @param f flows data.table
##' @param c corrections data.table
##' @import data.table
##' @export correct_flows
##' @return data.table
##' @author Konstantin Hoffie
correct_flows <- function(f, c) {
    ## according correct.csv 3201 and 3253 did not exist in 2001
    ## anymore. In flows they only appear as origin, not as
    ## destination. Probably they are coded wrongly. For now, I simply
    ## remove them.

    f <- f[! f[year == 2001 & origin %in% c(3201, 3253)],
                   on = .(origin, destination, year, age_group)]
    f <- f[, correct_flows_(.SD, c[year == .BY$year]), keyby = .(year, age_group)]
    f[, origin := as.integer(origin)]
    f[, destination := as.integer(as.character(destination))]
    return(f)
}

correct_flows_ <- function(f, c) {
    ags_old <- flow <- NULL
    ## if speed is an issue, the following 5 lines should be moved outside the function
    fm <- data.table::dcast(f, origin ~ destination, value.var = "flow", fill = 0)
    fm <- as.matrix(fm[, -1])
    cm <- data.table::dcast(c, ags_old ~ ags_new, value.var = "conv_p", fill = 0)
    cm <- as.matrix(cm[, -1])
    rownames(cm) <- c[, unique(ags_old)]

    fnewm <- t(cm) %*% fm %*% cm
    
    fnew <- data.table::melt(data.table(fnewm, keep.rownames = TRUE), id.vars = "rn")
    data.table::setnames(fnew, c("rn", "variable", "value"), c("origin", "destination", "flow"))
    fnew[, flow := as.integer(round(flow))]
    return(fnew)
}
