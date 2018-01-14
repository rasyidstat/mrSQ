#' Set SQLite database location and name
#'
#' @md
#' @param path folder path of the database
#' @param db_name name of the database
#'
#' @export
sqlite_start <- function (path = "~/OneDrive/Magnum Opus/project/data/db",
                          db_name = "db_qs.sqlite") {
  options(mrsq_db_dir = path)
  options(mrsq_db_name = db_name)
}

#' Initialize SQLite database
#'
#' @md
#' @param path folder path of the database
#' @param db_name name of the database
#'
#' @export
sqlite_init <- function (path = getOption("mrsq_db_dir"),
                         db_name = getOption("mrsq_db_name")) {
  RSQLite::dbConnect(RSQLite::SQLite(), sprintf("%s/%s", path, db_name))
}

#' Read table from SQLite database
#'
#' @md
#' @param query query statement
#' @param path folder path of the database
#' @param db_name name of the database
#'
#' @export
sqlite_query <- function (query,
                          path = getOption("mrsq_db_dir"),
                          db_name = getOption("mrsq_db_name")) {
  con <- RSQLite::dbConnect(RSQLite::SQLite(), db_path)
  res <- RSQLite::dbGetQuery(con, query)
  res <- tibble::as_tibble(res)
  res
}

#' Get first 10 rows of table on SQLite database
#'
#' @md
#' @param tbl_name name of the table
#' @param n number of limits
#' @param path folder path of the database
#' @param db_name name of the database
#'
#' @export
sqlite_glance <- function (tbl_name, n = 10,
                           path = getOption("mrsq_db_dir"),
                           db_name = getOption("mrsq_db_name")) {
  db_path <- sprintf("%s/%s", path, db_name)
  con <- RSQLite::dbConnect(RSQLite::SQLite(), db_path)
  if (n == "all") {
    res <- RSQLite::dbGetQuery(con, sprintf("select * from %s", tbl_name))
  } else {
    res <- RSQLite::dbGetQuery(con, sprintf("select * from %s limit %s",
                                            tbl_name, n))
  }
  res <- tibble::as_tibble(res)
  res
}

#' Show tables on SQLite database
#'
#' @md
#' @param pattern regex pattern
#' @param path folder path of the database
#' @param db_name name of the database
#'
#' @export
sqlite_table <- function (pattern = NULL,
                          path = getOption("mrsq_db_dir"),
                          db_name = getOption("mrsq_db_name")) {
  db_path <- sprintf("%s/%s", path, db_name)
  con <- RSQLite::dbConnect(RSQLite::SQLite(), db_path)
  res <- RSQLite::dbListTables(con)
  if (!is.null(pattern)) {
    res <- res[grepl(pattern, res)]
  }
  res
}

#' Write data into table on SQLite database
#'
#' @md
#' @param df data.frame to insert
#' @param tbl_name name of the table
#' @param append TRUE
#' @param overwrite FALSE
#' @param date TRUE date convert to character
#' @param path folder path of the database
#' @param db_name name of the database
#'
#' @export
sqlite_write <- function (df, tbl_name, append = TRUE, overwrite = FALSE, date = TRUE,
                          path = getOption("mrsq_db_dir"),
                          db_name = getOption("mrsq_db_name")) {
  db_path <- sprintf("%s/%s", path, db_name)
  con <- RSQLite::dbConnect(RSQLite::SQLite(), db_path)
  if (date == TRUE) {
    inx <- sapply(df, function(x) inherits(x, "Date") || inherits(x, "POSIXt"))
    df[inx] <- lapply(df[inx], as.character)
  }
  RSQLite::dbWriteTable(con, tbl_name, df,
                        row.names = FALSE,
                        append = append,
                        overwrite = overwrite)
  return(TRUE)
}

#' Drop table on SQLite database
#'
#' @md
#' @param tbl_name name of the table
#' @param path folder path of the database
#' @param db_name name of the database
#'
#' @export
sqlite_drop <- function (tbl_name, force = FALSE,
                         path = getOption("mrsq_db_dir"),
                         db_name = getOption("mrsq_db_name")) {
  db_path <- sprintf("%s/%s", path, db_name)
  con <- RSQLite::dbConnect(RSQLite::SQLite(), db_path)
  if (!grepl("_", tbl_name) | force == TRUE) {
    RSQLite::dbRemoveTable(con, tbl_name)
    message(sprintf("%s is removed", tbl_name))
  } else {
    warning(sprintf("%s is not removed, use `force = TRUE`", tbl_name))
  }
}

#' Execute query on SQLite database
#'
#' @md
#' @param query query statement
#' @param path folder path of the database
#' @param db_name name of the database
#'
#' @export
sqlite_exec <- function (query,
                         path = getOption("mrsq_db_dir"),
                         db_name = getOption("mrsq_db_name")) {
  db_path <- sprintf("%s/%s", path, db_name)
  con <- RSQLite::dbConnect(RSQLite::SQLite(), db_path)
  RSQLite::dbSendQuery(con, query)
}

#' Disconnect from SQLite database
#'
#' @md
#' @param path folder path of the database
#' @param db_name name of the database
#'
#' @export
sqlite_dc <- function (path = getOption("mrsq_db_dir"),
                       db_name = getOption("mrsq_db_name")) {
  db_path <- sprintf("%s/%s", path, db_name)
  con <- RSQLite::dbConnect(RSQLite::SQLite(), db_path)
  RSQLite::dbDisconnect(con)
  return(TRUE)
}



