##' Calculates net migration from flow data.table
##'
##' @param dt flow data.table
##' @param col character, column to compute net migration from,
##'     typically flows or predictions.
##' @param by character, additional grouping variables besides o and
##'     d. Typically agegroup and year.
##' @param o character, origin identifier
##' @param d character, destination identifier
##' @param long logical, should data be returned in long format?
##' @param type character, if specified a column called type is added
##'     with entries as specified by type. Useful to identify
##'     different tables
##' @return data.table of net migration
##' @import data.table
##' @export calculate_net
##' @author Konstantin Hoffie
calculate_net <- function(dt, col, by, o = "fromdist", d = "todist",
                          long = FALSE, type = NULL) {
    outflow <- i.outflow <- inflow <- . <- net <- NULL
    dt_in <- dt[, .(inflow = sum(get(col))), keyby = c(d, by)]
    dt_out <- dt[, .(outflow = sum(get(col))), keyby = c(o, by)]
    dt_in[dt_out, outflow := i.outflow, on = c(stats::setNames(o, d), by)]
    dt_in[, net := inflow - outflow]
    data.table::setnames(dt_in, d, "region")
    if(!is.null(type)) {
        dt_in[, type := type]
        id_vars <- c("type", "region", by)
    }
    if(is.null(type)) {
        id_vars <- c("region", by)
    }
    if(long == TRUE) {
        dt_in <- data.table::melt(dt_in, id.vars = id_vars)
    }
    return(dt_in)
}

## order_models <- function(dt) {
##     model_names <- dt[, unique(model)]
##     lvls <- data.table(models = model_names)
##     lvls[, id := tstrsplit(model_names, "_")[[2]]]
##     lvls <- lvls[order(as.numeric(id)), .(models)]
##     dt[, model := factor(model, levels = lvls[, models])]
##     return(NULL)
## }

##' Recodes German agegroups to English
##'
##' @param dt data.table
##' @return NULL
##' @import data.table
##' @export rec_ages
##' @author Konstantin Hoffie
rec_ages <- function(dt) {
  agegroup <- i.new <- . <- old <- NULL
  lbls <- data.table::data.table(old = c("unter18", "über65"),
                     new = c("below18", "above65"))
  dt[lbls, agegroup := i.new, on = .(agegroup = old)]
  return(NULL)
}

##' Appends missing rows to flow data
##'
##' Flow data obtained from the federal statistical office only has
##' rows if flows are > 0. add_missing_flows adds rows that are
##' missing and sets flows for those rows to 0.
##' @param flows data.table with flows
##' @param regions character, unique regions
##' @param agegroups character, unique agegroups
##' @param years numeric, unique years
##' @return flow data.table with missing rows
##' @import data.table
##' @export add_missing_flows
##' @author Konstantin Hoffie
add_missing_flows <- function(flows, regions, agegroups, years) {
    ### in flows data all 0 flows are missing. We add them now to make
    ### sure all origins have the same destinations for all age groups and
### vice versa
    all_keys <- data.table::CJ(fromdist = regions,
                   todist = regions,
                   agegroup = agegroups,
                   year = years)
    data.table::setkeyv(all_keys, colnames(all_keys))
    data.table::setkeyv(flows, colnames(all_keys))
    flows <- flows[all_keys]
    flows[is.na(flows), flows := 0]
    return(flows)
}

##' Creates a vector if probabilities from a low a high and a step
##' value. Makes sure lowest is 0 and highest 1. Intended for use in cut_qs()
##'
##' @param low numeric, lowest probability which is greater 0.
##' @param high numeric, highest probability which is smaller 1.
##' @param step numeric, step size
##' @return numeric vector
##' @export cut_probs
##' @author Konstantin Hoffie
cut_probs <- function(low = .01, high = .99, step = .2) {
    p_low <- low
    p_high <- high
    ps <- seq(0, 1, step)
    ps <- unique(sort(c(0, p_low, p_high, ps, 1)))
    return(ps)
}

##' Cuts a numeric vector based on quantiles. Either wih evenly sized
##' quantiles or with hand chosen ones.
##'
##' @param x numeric vector
##' @param num_quantiles integer, optional
##' @param probs vector of quantiles
##' @param labels character, optional vector to label resulting vector
##' @param lab logical, if FALSE vector has integers instead of labels
##' @return numeric, cutted vector
##' @export cut_qs
##' @importFrom stats quantile
##' @author Konstantin Hoffie
cut_qs <- function(x, num_quantiles = NULL, probs = cut_probs(), labels = NULL, lab = FALSE) {
    if(!is.null(probs)) {
        qs <- stats::quantile(x,  probs = probs,  na.rm = TRUE)
    }
    if(!is.null(num_quantiles)) {
        qs <- stats::quantile(x,  probs = seq(0, 1, length.out = num_quantiles + 1),
                   na.rm = TRUE)
    }
    qs <- unique(qs)
    if (is.null(labels)) {
        labels <- paste0("Q", seq(length(qs) - 1))
    }
    if (lab == TRUE) {
        y <- cut(x, breaks = qs, labels = labels, include.lowest = TRUE)
    }
    if (lab == FALSE)  {
        y <- cut(x, breaks = qs, include.lowest = TRUE)
    }
  
  return(y)
}

## logistic <- function(x) {
##     y <- 1 / (1 + exp(-x))
##     return(y)
## }
