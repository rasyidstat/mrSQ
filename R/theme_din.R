#' ggplot2 theme using DIN font
#'
#' @md
#' @param version default is version 1 which is based on theme_ipsum,
#' version 2 is based on theme_minimal
#' @param panel_grid `TRUE`, `FALSE`
#' @param legend default is `NULL` (to fill, remember quadran position: 1, 2, 3, 4)
#' @param plot_margin margin(5, 5, 5, 5)
#' @param font default is DIN font
#' @param ... other parameters
#'
#' @export
theme_din <- function (version = 1,
                       panel_grid = TRUE,
                       legend = NULL,
                       plot_margin = margin(5, 5, 5, 5),
                       font = "DIN", ...) {

  if (version == 1) {
    ret <- hrbrthemes::theme_ipsum(base_family=font,
                                   plot_margin=plot_margin,
                                   plot_title_margin=7.5,
                                   caption_face="plain", ...)
    if (panel_grid == TRUE){
      ret <- ret + theme(panel.grid.minor = element_blank())
    }
  }
  else if (version == 2) {
    ret <- ggplot2::theme_classic(base_family=font,
                                  base_size=11.5)
    ret <- ret + theme(axis.line = element_line(size = 1, colour = "#414042"),
                       axis.ticks = element_line(size = 1, colour = "#414042"),
                       axis.text = element_text(size = 11.5),
                       axis.title.x = element_text(size = 9, hjust = 1),
                       axis.title.y = element_text(size = 9, hjust = 1),
                       plot.caption = element_text(size = 9, margin = margin(t = 10)),
                       plot.margin = plot_margin,
                       plot.title = element_text(size = 18, margin = margin(b = 7.5)),
                       plot.subtitle = element_text(size = 12, margin = margin(b = 12)))
  } else {
    message("Error : version should be 1 or 2")
  }

  if (is.null(legend)) {
    ret <- ret
  }
  else if (legend == 1) {
    ret <- ret + theme(legend.justification = c(1,1),
                       legend.position = c(1,1))
  }
  else if (legend == 2) {
    ret <- ret + theme(legend.justification = c(0,1),
                       legend.position = c(0,1))
  }
  else if (legend == 3) {
    ret <- ret + theme(legend.justification = c(0,0),
                       legend.position = c(0,0))
  }
  else if (legend == 4) {
    ret <- ret + theme(legend.justification = c(1,0),
                       legend.position = c(1,0))
  }

  ret
}
