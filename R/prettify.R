##' Prettifies a plot using economist design
##'
##' Based on ggthemes and custom modifications a ggplot object is made
##' more visually appealing by applying a theme based on The
##' Economist.
##' @param plt ggplot object
##' @param main character, title of plot
##' @param sub character, Subtitle
##' @param leg_title character, title fo legend
##' @param rescale character, on of "color", "fill", "both". Applies
##'     economist color theme accordingly
##' @param rm_grid logical, if TRUE, grid is removed completely
##' @param rm_axes logical removes axes
##' @param caption character, caption for plot in bottom right corner
##' @param scale_x logical, if TRUE x-axis is assumed to show years
##'     and is made better readable
##' @param show_cap logical, if TRUE, caption is shown
##' @param year_max numerical max year in data. Used to prettify
##'     x-axis if scale_x == TRUE
##' @return ggplot object
##' @import ggplot2
##' @import ggthemes
##' @importFrom ggtext element_markdown
##' @export prettify
##' @author Konstantin Hoffie
prettify <- function(plt,
                     main,
                     sub = NULL,
                     leg_title = NULL,
                     rescale = c("color", "fill", "both", "none"),
                     rm_grid = FALSE,
                     rm_axes = FALSE,
                     caption = NULL,
                     scale_x = FALSE,
                     show_cap = TRUE,
                     year_max = NULL) {
    rescale <- match.arg(rescale)
    plt2 <- plt +
        ggthemes::theme_economist() +
        panel_scale() +
        ggtitle(main) +
        ylab("") +
        xlab("")
        plt2 <- plt2 + labs(caption = make_cap(text = caption, show = show_cap)) 
    if(rm_grid == TRUE) {
        plt2 <- plt2 + theme(panel.grid = element_blank())
    }
    if(scale_x == TRUE) {
        plt2 <- year_scale(plt2, year_max)
    }
    if(rescale == "color") {
        plt2 <- plt2 + ggthemes::scale_color_economist(leg_title)
    }
    if(rescale == "both") {
        plt2 <- plt2 + ggthemes::scale_fill_economist(leg_title)
        plt2 <- plt2 + ggthemes::scale_color_economist()

    }
    if(rescale == "fill") {
        plt2 <- plt2 + ggthemes::scale_fill_economist(leg_title)
    }
    if(is.null(sub) == FALSE) {
            plt2 <- plt2 +
                labs(subtitle = sub)
    }
    if(rm_axes == TRUE) {
        plt2 <- plt2 +
            theme(axis.text = element_blank(),
                  axis.ticks = element_blank())     
    }
    return(plt2)
}

year_scale <- function(plt, y_max) {
    if(y_max == 2017) {
        plt <- plt + scale_x_continuous(breaks=c(2000, 2005,2010,2015, 2017),
                           labels=c("2000","05", "10", "", "17"))
    }
    if (y_max == 2019) {
        plt <- plt + scale_x_continuous(breaks = c(1995, 2000, 2005, 2010, 2015, 2019),
                           labels = c(1995, 2000, 05, 10, 15, 19))
    }
    if (y_max == 2022) {
        plt <- plt + scale_x_continuous(breaks = c(1995, 2000, 2005, 2010, 2015, 2020, 2022),
                           labels = c(1995, 2000, 05, 10, 15, "", 22))
    }
    return(plt)
}

panel_scale <- function() {
    theme(panel.spacing.x = unit(2, "lines"),
          strip.text.x = ggtext::element_markdown(vjust = 1))
}

make_cap <- function(text = NULL, show) {
    source <- "Quelle: Melderegister, eigene Berechnungen"
    if(is.null(text) & show == TRUE) {
        cap <- source
    }
    if(is.null(text) & show == FALSE) {
        cap <- ""
    }
    if(!is.null(text) & show == TRUE) {
        cap <- sprintf("%s\n%s", text, source)
    }
    if(!is.null(text) & show == FALSE) {
        cap <- text
    }
    return(cap)
}

