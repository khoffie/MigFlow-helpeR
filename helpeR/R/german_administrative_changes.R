##' Cleans file to correct German administrative changes. The file was
##' obtained from the BBSR
##'
##' @param raw file raw data
##' @param clean file clean data
##' @import data.table
##' @import readxl
##' @export german_administrative_changes
##' @return NULL
##' @author Konstantin
german_administrative_changes <- function(raw, clean) {
    changes <- excel_sheet_reader(raw)
    dt <- one_table(changes)
    dt <- omit_baseyear(dt)
    dt <- recode_ags(dt)
    fwrite(dt, clean)
}

excel_sheet_reader <- function(filename) {
  sheets <- excel_sheets(filename)
  x <- lapply(sheets, function(X) read_excel(filename, sheet = X))
  names(x) <- sheets
  return(x)
}
one_table <- function(changes) {
    ..keep <- NULL
    lapply(changes, setDT)
    
    for (i in seq_along(changes)) {
        changes[[i]][, "year" := names(changes)[i]]
    }
    ## before we can combine the lists to one table we need to make sure
    ## they have the same column names
    for (i in seq_along(changes)) {
        dt <- changes[[i]]
        setnames(dt, colnames(dt)[1], "ags_old")
        setnames(dt, colnames(dt)[2], "name_old")
        old <- "fl\u00E4chen-\r\nproportionaler\r\nUmsteige-schl\u00FCssel"
        setnames(dt, old, "conv_f")
        old <- "bev\u00F6lkerungs- \r\nproportionaler \r\nUmsteige- \r\nschl\u00FCssel"
        setnames(dt, old, "conv_p")
        old <- "Kreise\r\n 31.12.2019"
        setnames(dt, old, "ags_new")
        old <- "Kreisname 2019"
        setnames(dt, old, "name_new")
    }
    keep <- c("ags_old", "name_old", "conv_f", "conv_p",
              "ags_new", "name_new", "year")
    for (i in seq_along(changes)) {
        changes[[i]] <- changes[[i]][, ..keep]
    }
    changes <- rbindlist(changes)
    return(changes)
}

omit_baseyear <- function(dt) {
    ### 2019 is the base year. That is, changes in regions of former
    ### years are expressed relative to 2019. year is currently
    ### expessed as 1990-2019 for example. year can be better handled
    ### if -2019 is omitted, for joins etc.
    dt[, "year" := tstrsplit(year, "-", keep = 1)]
    return(dt)
}

recode_ags <- function(dt) {
    ags_old <- ags_new <- NULL
    ### sometimes district are expessed with four digits, the last
    ### three being 0. We remove them, so that joins with other tables
    ### work
    dt[, "ags_old" := as.integer(gsub("000$", "", as.character(ags_old)))]
    dt[, "ags_new" := as.integer(gsub("000$", "", as.character(ags_new)))]
    dt[ags_old == 2000000, "ags_old" := 2000]
    dt[ags_new == 2000000, "ags_new" := 2000]
    dt[ags_old == 11000000, "ags_old" := 11000]
    dt[ags_new == 11000000, "ags_new" := 11000]
    return(dt)
}
