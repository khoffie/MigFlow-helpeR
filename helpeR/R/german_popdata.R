##' Cleans German population data 
##'
##' @return NULL, writes to disk
##' @import data.table
##' @import sf
##' @export german_popdata
##' @author Konstantin
german_popdata <- function() {
    . <- region <- age_group <- NULL
    dt1 <- clean_pop("./data/raw", "12411-03-02-4.csv")
    dt2 <- clean_pop("./data/raw", "12411-03-03-4.csv")
    ## three years are in both tables and there are slight
    ## differences. Arbitrarily, we use years from last table
    dt <- rbind(dt1[year < 2011], dt2)
    ## there are some missings and as a simple imputation, we use the last observation to fill missings
    cols <- c("all", "german", "foreign")
    dt[, c(cols) := lapply(.SD, function(x) nafill(x, "nocb")),
       keyby = .(region, age_group), .SDcols = cols]
    message("Missings imputed using constants: Next observation carried backwards.")
    fwrite(dt, "./data/clean/germanpop.csv")
    message("germanpop.csv written to disk")
}

clean_pop <- function(path, file = c("12411-03-02-4.csv", "12411-03-03-4.csv")) {
    region_type <- region <- . <- age_group <- NULL
    all_b <- ger_b <- for_b <- AGS <- NULL
    
    shp <- data.table(sf::read_sf("./data/raw/shapes/districts_ext.shp"))
    
    file <- match.arg(file)
    dt <- read_pop(path, file)
    new_cols <- c("year", "region", "region_type", "age_group",
                  "all_b", "all_m", "all_f",
                  "ger_b", "ger_m", "ger_f",
                  "for_b", "for_m", "for_f")
    setnames(dt, colnames(dt), new_cols)

    dt <- rec_ages_(dt, file)

    dt <- dt[-c(1,2), ] ## colnames in rows
    dt <- dt[region_type != "Deutschland"] ## rm since interested in districts
    dt[, year := as.integer(substring(year, 7, 10))]
    dt[, region := as.integer(region)]
    dt[region == 2, region := 2000] ## Hamburg
    dt[region == 11, region := 11000] ## Berlin

    nc <- new_cols[-c(1:4)]
    dt[, c(nc) := lapply(.SD, as.integer), .SDcols = nc]
    
    dt <- dt[, .(year, region, age_group, all_b, ger_b, for_b)]
    dt <- dt[region %in% shp[, AGS]] # filter all non districts

    dt <- dt[, c("all_b", "ger_b", "for_b") := lapply(.SD, sum),
             keyby = .(year, region, age_group), .SDcols = c("all_b", "ger_b", "for_b")]

    dt <- dt[, .SD[1], keyby = .(year, region, age_group)]
    dt <- dt[, .(region, year, age_group, all = all_b, german = ger_b, foreign = for_b)]

    return(dt)
}

rec_ages_ <- function(dt, file) {
    . <- age_group <- new_age <- i.new_age <- old_age <- NULL
    last <- ifelse(file == "12411-03-02-4.csv", 2, 6)
    rec <- dt[, .(old_age = unique(age_group))]
    a <- c("unter18", "18-25", "25-30", "30-50", "50-65", "\u00fcber65" )
    new_ages <- c(NA, rep(a[1], 5), rep(a[2], 2), a[3], rep(a[4], 4), rep(a[5], 3), rep(a[6], last), "all")
    rec[, new_age := new_ages]
    dt[rec, age_group := i.new_age, on = .(age_group = old_age)]    
    return(dt)
}

read_pop <- function(path, file) {
    read_pop_ <- function(path, file) {
        dt <- fread(file.path(path, file), skip = 6,
                    encoding = "Latin-1", na.strings = c("-", "", "."),
                    strip.white = TRUE, fill = FALSE)
        return(dt)
    }
    ## first linenumbers not part of data
    cond <- ifelse(file == "12411-03-02-4.csv", "184005", "153877")
    cond <- sprintf("Stopped early on line %s", cond)
    dt <- withCallingHandlers({
        read_pop_(path, file)
    }, warning = function(w) {
        ## data ends there and only meta information follows
        if (grepl(cond, conditionMessage(w))) {
            invokeRestart("muffleWarning")
        } else {
            warning(w)
        }
    })
    return(dt)
}
