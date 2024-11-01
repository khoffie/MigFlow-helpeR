##' Extracts INKAR variables
##'
##' @param ink data.table, INKAR
##' @param ids integer, IDs to extract
##' @param rb character, Raumbezug: Spatial entities for which to
##'     extract variables
##' @param zb integer, Zeitbezug, years for which to extract
##'     variabeles
##' @param wide logical, if TRUE, data is returned in wide format. If
##'     FALSE, long format is used instead
##' @return data.table
##' @import data.table
##' @export create_design_mat
##' @author Konstantin
create_design_mat <- function (ink, ids, rb, zb, wide = TRUE) {
    out <- get_inkar_vars(ink, ids, rb = rb, zb = zb, wide = wide)
    if(wide == TRUE) {
        make_valid_colnames(out)
    }
    
    ## if (299 %in% ids) {
    ##     out[is.na(X299), `:=`("X299", X4/X9)]
    ## }
    ## zeros_to_non_zeros(out, id_vars = c("AGS", "Zeitbezug"))
    return(out)
}

##' Extracts variables from INKAR in a either long or wide format.
##'
##' @title Extract INKAR variables
##' @param inkar data.table inkar data
##' @param ids numeric, variable IDs
##' @param rb character, "Raumbezug" like: "Bundesländer", "Kreise"
##'     etc.
##' @param zb numeric, Zeitbezug
##' @param wide logical, if TRUE, data are returned in wide format, if
##'     false, in long format.
##' @return data.table
##' @import data.table
##' @author Konstantin
get_inkar_vars <- function(inkar, ids, rb, zb, wide) {
    ## lol this coding is so bad. would be much better to just read
    ## the name_col
#### Extrahiert Prediktoren aus dem INKAR Datensatz.

    ## I should catch the warning:
    ##    Aggregate function missing, defaulting to 'length'
    ## convert to error and provide a more descriptive error message
    Raumbezug <- . <- Wert <- NULL
    region <- year <- ID <- NULL
    ink <- inkar[Raumbezug %in% rb & year %in% zb & ID %in% ids, ]
    ink <- ink[, .(region, rb = Raumbezug, id = ID, year, value = Wert)]
    if (wide == TRUE) {
        ink <- data.table::dcast(ink, region + rb + year ~ id,
                                 value.var = "value")
        make_valid_colnames(ink)
    }
    return(ink)
}

make_valid_colnames <- function(dt) {
    ## still needed?
    data.table::setnames(x = dt,
                     old = colnames(dt),
                     new = make.names(colnames(dt)))
    return(NULL)
}
