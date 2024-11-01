##' Creates a design matrix from the INKAR data.table. Variables are
##' selected and turned into wide format. Currently from a shapefile
##' area is merged as additional column. Although this should actually
##' be in the data already.
##'
##' The columns are extracted using the inkar ID. These names are then
##' turned into valid column names. Additionally all numerical zeros
##' are set to 0.1. Then it is possible to log transform data that are
##' numerically exactly zero as well. All columns are then log
##' transformed.
##'
##' Also the variable X299, population density has missing values in
##' the original INKAR data. Since population density is just
##' population over area and this data is complete, I fill the missing
##' values.
##' 
##' @title Create design matrix from INKAR data.table
##' @param ink data.table, INKAR
##' @param ids character, ID'S of INKAR variables that are extracted
##' @param rb character Raumbezug, here districts
##' @param zb numeric, Zeitbezug, which years are used
##' @param log logical. If TRUE, all columns are logarithmized.
##' @return data.table
##' @import data.table
##' @import inkaR
##' @export create_design_mat
##' @author Konstantin Hoffie
create_design_mat <- function(ink, ids, rb, zb, log = FALSE) {
    ## I don't need chape anymore since I do not join area
    . <- AGS <- X299 <- X4 <- X9 <- NULL
    out <- inkaR::get_inkar_vars(ink,
                                 ids,
                                 rb = rb,
                                 zb = zb, ## should be optional
                                 name_col = "ID",
                                 wide = TRUE)
    ## out <- merge(out, shp[, .(AGS, area)],
    ##              by = "AGS",
    ##              all.x = TRUE)
    make_valid_colnames(out)
    if(299 %in% ids) {
        out[is.na(X299), "X299" := X4 / X9]
    }
    ##     setnames(out, "area", "Xarea")
    zeros_to_non_zeros(out, id_vars = c("AGS", "Zeitbezug"))
    if (log == TRUE) {
        columns_to_log(out, omit = c("AGS", "Zeitbezug"))
    }
    return(out)
}

make_valid_colnames <- function(dt) {
    ## still needed?
    data.table::setnames(x = dt,
                     old = colnames(dt),
                     new = make.names(colnames(dt)))
    return(NULL)
}

zeros_to_non_zeros <- function(dt, id_vars = NULL) {
    ## This looks very complicated. In the end I only want to
    ## transform zeros in the predicors to 0.1 or something to be able
    ## to use the logarithm. This might not need to be a function
    ## because I can do it once when creating the design matrix
    value <- variable <- N <- NULL
    if (is.null(id_vars)) {
        stop("Please specify id variables in dt.")
    }
    zeros <- data.table::melt(dt, id.vars = id_vars)
    zeros <- zeros[value == 0, .N, keyby = variable]
    if (nrow(zeros) == 0) {
        mes <- "No zeros found in data. Data is kept as it is"
        message(mes)
    }
    if (nrow(zeros) > 0) {
    cols <- as.character(zeros[, unique(variable)])
    mes <-  sprintf("Columns %s have in total %s zeros",
                    paste0(cols, collapse = " "),
                    zeros[, sum(N)])
    message(mes)
    dt[, (cols) := lapply(.SD, function(x) fifelse(x == 0, 0.1, x)),
       .SDcols = cols]
        message(sprintf(" %s zeros -> 0.1", zeros[, sum(N)]))
    }
    return(NULL)
}

columns_to_log <- function(dt, to_log = NULL, omit = NULL) {
    ## used to transform columns in flows to log. But if I do
    ## transformation earlier, when variables are extracted from
    ## INKAR, I don't nee this function I think
    if (is.null(to_log) && is.null(omit)) {
        stop("Please specify either which columns to logarithmize or which to omit.")
    }
    if (!is.null(to_log) && is.null(omit)) {
        cols <- to_log
    }
    if (!is.null(omit) && is.null(to_log)) {
        cols <- base::setdiff(colnames(dt), omit)
    }
    bad <- dt[, lapply(.SD, function(x) any(x <= 0)), .SDcols = cols]
    bad <- as.logical(bad)
    if (sum(bad) > 0) {
        mes <- sprintf("There are values <= 0 in columns %s",
                       paste(cols[bad], collapse = " "))
        stop(mes)
    }
    dt[, (cols) := lapply(.SD, log), .SDcols = cols]
    return(dt)
}
