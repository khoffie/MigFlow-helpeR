## data obtained from https://www.regionalstatistik.de/genesis/online

## In general, other sources for population data exists, like
## INKAR. Many do not differentiate between foreigners and Germans
## though. This can lead to considerable bias since foreigners are not
## distributed uniformly across Germany.

cleanup <- function() {
    . <- age_group <- new_age <- region <- all_b <- ger_b <- for_b <- NULL
    shp <- AGS <- i.new_age <- old_age <- NULL
    file <- file.path("./data/raw", "12411-03-03-4.csv")
    dt <- fread(file, skip = 6, encoding = "Latin-1", na.strings = c("-", "", " "))

    new_cols <- c("year", "region", "region_type", "age_group",
                  "all_b", "all_m", "all_f",
                  "ger_b", "ger_m", "ger_f",
                  "for_b", "for_m", "for_f")
    setnames(dt, colnames(dt), new_cols)

    ## create table to recode agegroups to match age groups in migration data
    rec_age <- dt[, .(old_age = unique(age_group))]
    a <- c("unter18", "18-25", "25-30", "30-50", "50-65", "über65" )
    new_ages <- c(NA, rep(a[1], 5), rep(a[2], 2), a[3], rep(a[4], 4), rep(a[5], 3), rep(a[6], 6), "all")
    rec_age[, new_age := new_ages]

    dt <- dt[-c(1,2), ] ## colnames in rows
    dt[, year := as.integer(substring(year, 7, 10))]
    dt[, region := as.integer(region)]
    dt[region == 2, region := 2000] ## Hamburg
    dt[region == 11, region := 11000] ## Berlin
    dt[, new_cols[-c(1:4)] := lapply(.SD, as.integer), .SDcols = new_cols[-c(1:4)]]

    dt2 <- dt[, .(year, region, age_group, all_b, ger_b, for_b)]
    dt2 <- dt2[region %in% shp[, AGS]]
    dt2[rec_age, age_group := i.new_age, on = .(age_group = old_age)]
    dt2 <- dt2[, c("all_b", "ger_b", "for_b") := lapply(.SD, sum),
               keyby = .(year, region, age_group), .SDcols = c("all_b", "ger_b", "for_b")]
    dt2 <- dt2[, .SD[1], keyby = .(year, region, age_group)]
    dt2 <- dt2[, .(region, year, age_group, all = all_b, german = ger_b, foreign = for_b)]
    fwrite(cleanup(dt2), file.path("./data", "agefor.csv"))
    return(NULL)
}



