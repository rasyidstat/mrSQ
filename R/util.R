#' Clean Text for Title View
#'
#' @md
#' @param txt text included
#'
#' @export
#' @examples
#' clean_title("muhammad rasyid ridha")
clean_title <- function(txt) {
  gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", tolower(txt), perl=TRUE)
}

#' Round Numbers (2 digits behind comma)
#'
#' @md
#' @param x input
#'
#' @export
#' @examples
#' round_df(100.1231212)
round_df <- function(x, digits = 2) {
  num_col <- sapply(x, class) == "numeric"
  x[num_col] <- round(x[num_col], digits)
  x
}

#' Datetime to HMS for data visualization
#'
#' @md
#' @param x datetime input (POSIXct)
#'
#' @export
to_hms <- function(x) {
  x <- as.POSIXct(x)
  x <- format(x, "%H:%M:%S")
  x <- as.POSIXct(paste("1970-01-01", x))
  x
}
