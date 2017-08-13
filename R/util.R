#' Clean Text for Title View
#'
#' @md
#' @param txt text included
#'
#' @export
clean_title <-function(txt) {
  gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", tolower(txt), perl=TRUE)
}
