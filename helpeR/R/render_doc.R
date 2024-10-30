##' Renders .Rmd and .tex files
##'
##' @param path character, path to file
##' @param file character, file name
##' @param pdf logical, if TRUE, .Rmd is rendered to pdf instead html. Tex files are not affected.
##' @param toc logical, if TRUE, from .Rmd a table of contents is generated. Tex files are not affected.
##' @param rm_main logail, if TRUE, _main.Rmd is removed after rendering .Rmd
##' @return nothing
##' @import bookdown
##' @import tinytex
##' @export render_doc
##' @author Konstantin
render_doc <- function(path, file = "index.Rmd", pdf = TRUE, toc = TRUE, rm_main = TRUE) {
    ### not so clever because no objects in global environment created
    old_path <- getwd()
    on.exit(setwd(old_path))
    setwd(path)
    if(grepl(".Rmd", file)) {
        render_rmd(path, file, pdf, toc, rm_main)
    }
    if(grepl(".tex", file)) {
        render_tex(path, file)
    }
}

render_rmd <- function(path, file, pdf, toc, rm_main) {
        if (rm_main == TRUE) {
            if (base::file.exists(file.path(path, "_main.Rmd"))) {
                base::file.remove(file.path(path, "_main.Rmd"))
            }
        }
        if (pdf == TRUE & toc == TRUE) {
            bookdown::render_book(file, bookdown::pdf_book(toc = TRUE, number_sections = TRUE))
        }
        if (pdf == TRUE & toc == FALSE) {
            bookdown::render_book(file, bookdown::pdf_book(toc = FALSE, number_sections = FALSE))
        }
        if (pdf == FALSE) {
            bookdown::render_book(file)
        }
}

render_tex <- function(path, file, bib_engine = "biber") {
    tinytex::pdflatex(file.path(path, file), bib_engine = "biber")
}

## render_doc("~/Documents/GermanMigration/writeup/", "report.Rmd")
## render_doc("~/Documents/GermanMigration/writeup/", "math.tex")
