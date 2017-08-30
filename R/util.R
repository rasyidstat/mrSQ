#' Clean Text for Title View
#'
#' @md
#' @param txt text included
#'
#' @export
#' @examples
#' clean_title("muhammad rasyid ridha")
#' # [1] "Muhammad Rasyid Ridha"
clean_title <- function(txt) {
  gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", tolower(txt), perl=TRUE)
}
