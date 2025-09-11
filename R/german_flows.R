##' Cleans preprocessed flows and creates one table
##'
##' @param raw path for raw data
##' @param cleanfile file name for clean data
##' @return NULL, writes to disk
##' @import data.table
##' @export german_flows
##' @author Konstantin
german_flows <- function(raw, cleanfile) {
    origin <- destination <- NULL
    dt <- rbindlist(lapply(file.path(raw, list.files(raw)), fread))
    dt <- duplicated_rows(dt)
    dt <- fix_ags_berlin_hamburg(dt)
    dt <- rec_ages__(dt)
    dt <- agegroup_all(dt)
    dt <- swap_od(dt)
    dt <- dt[origin != destination]
    dt <- rm_lateresettlers(dt)
    fwrite(dt, cleanfile)
    message(sprintf("%s written to disk", cleanfile))
}

duplicated_rows <- function(dt) {
    flow <- N <- . <- flows <- NULL
    ## Theoretically, every combination of origin, destination,
    ## age_group and year should be in the data only once. Because if
    ## it is in there twice or more often, than this just makes for a
    ## larger flow
    keys <- c("origin", "destination", "age_group", "year")
    show <- flows[, .N, keyby = keys][N > 1][, .N, keyby = year]
    ## message("duplicated origin-destination-age_group-year keys per year")
    ## message(show[, .(year)])
    ## message(show[, .(N)])
    dt <- dt[year != 2003] #
    warning("Year 2003 removed from data due to many data problems, check later.")
    ## Sum over them, albeit from 2003 only very few
    ## duplicatde. Summing assumes all rows, even the dublicated and
    ## even the ones where even the flow is dublicated, are
    ## valid. What difference does this make compared to taking only
    ## one flows[, .SD[1], keyby = keys][, sum(flow)]
    dt <- dt[, .(flow = sum(flow)), keyby = keys]
    return(dt)
}

fix_ags_berlin_hamburg <- function(dt) {
    . <- flow <- origin <- destination <- NULL
    ## there are some ags to fix. From 2000Hamburg I discovered that there
    ## are different AGS for every quarter of Hamburg, I remap those
    ags_hamburg <- 2101:2718
    dt[origin %in% ags_hamburg, "origin" := 2000]
    dt[destination %in% ags_hamburg, "destination" := 2000]
    ags_berlin <- c(11001:11012, 11100:11223)
    dt[origin %in% ags_berlin, "origin" := 11000]
    dt[destination %in% ags_berlin, "destination" := 11000]

    ## Since many different keys are now the same, we need to sum
    ## again
    keys <- c("origin", "destination", "age_group", "year")
    dt <- dt[, .(flow = sum(flow)), keyby = keys]
    return(dt)
}

rec_ages__ <- function(dt) {
    age_group <- i.new <- . <- NULL
    dt <- dt[age_group != "AG", ] ## dont know whats up here
    ## only in 2006, seems to be more or less the sum of the other age groups
    dt <- dt[age_group != "insgesamt", ] 
    old <- c("18 bis unter 25", "25 bis unter 30", "30 bis unter 50",
         "50 bis unter 65", "65 und mehr", "65 und \u00E4lter")
    new <- c("18-25", "25-30", "30-50", "50-65", "\u00FCber65", "\u00FCber65")
    rec <- data.table(old, new)
    dt[rec, age_group := i.new, on = .(age_group = old)]
    dt[, "age_group" := gsub(" ", "", age_group)] ## do in preparation
    if(dt[, uniqueN(age_group)] != 6) {
        stop("Number of age groups not 6")
    }
    return(dt)
}

agegroup_all <- function(dt) {
    . <- flow <- ..keys <- i.flow <- age_group <- NULL
    ### It is useful to have an age group consisting of all age groups
    dt_all_ages <- dt[, .(flow = sum(flow)), keyby = c("origin", "destination", "year")]
    dt_all_ages[, "age_group" := "all"]
    keys <- c("origin", "destination", "year", "age_group")
    setkeyv(dt, keys)
    setkeyv(dt_all_ages, keys)
    keys_all <- rbind(dt[, ..keys], dt_all_ages[, ..keys])
    dt <- dt[keys_all]
    setkeyv(dt, keys)
    dt[dt_all_ages, "flow":= i.flow]
    if(dt[age_group != "all", sum(flow)] != dt[age_group == "all", sum(flow)]) {
        stop("Age group all not sum of others!")}
    return(dt)
}

swap_od <- function(dt) {
    ## In original data origin and destination are swapped, so we swap
    ## them back
    setnames(dt, c("origin", "destination"), c("destination", "origin"))
    setcolorder(dt, c("origin", "destination"))
    return(dt)
}

rm_lateresettlers <- function(dt) {
  fromdist <- year <- NULL
  dt <- dt[! (fromdist == 3159 & year <= 2005)] ## Göttingen
  dt <- dt[! (fromdist == 3459 & year == 2000)] ## Osnabrück Kreis
  dt <- dt[! (fromdist == 8237 & year == 2000)] ## Freudenstadt
  ## Unna, second-order effects, because many from Göttingen and
  ## Osnabrück-Kreis moved there
  dt <- dt[! (fromdist == 5978 & year <= 20005)]
  return(dt)
}
