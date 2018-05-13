#' Read table from PostgreSQL database
#'
#' @md
#' @param query query statement
#' @param path folder path of the database
#' @param db_name name of the database
#'
#' @export
pq_query <- function (query,
                      dbname = "rasyidridha",
                      host = "localhost",
                      port = "5432",
                      user = NULL,
                      password = NULL) {
  con <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(),
                                dbname = dbname,
                                host = host,
                                port = port,
                                user = user,
                                password = password)
  res <- RPostgreSQL::dbGetQuery(con, query)
  res <- tibble::as_tibble(res)
  RPostgreSQL::dbDisconnect(con)
  res
}

#' Get first 10 rows of table on PostgreSQL database
#'
#' @md
#' @param tbl_name name of the table
#' @param n number of limits
#' @param path folder path of the database
#' @param db_name name of the database
#'
#' @export
pq_glance <- function (tbl_name,
                       n = 10,
                       dbname = "rasyidridha",
                       host = "localhost",
                       port = "5432",
                       user = NULL,
                       password = NULL) {
  con <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(),
                                dbname = dbname,
                                host = host,
                                port = port,
                                user = user,
                                password = password)
  if (n == "all") {
    res <- RPostgreSQL::dbGetQuery(con, sprintf("select * from %s", tbl_name))
  } else {
    res <- RPostgreSQL::dbGetQuery(con, sprintf("select * from %s limit %s",
                                                tbl_name, n))
  }
  res <- tibble::as_tibble(res)
  RPostgreSQL::dbDisconnect(con)
  res
}

#' Show tables on PostgreSQL database
#'
#' @md
#' @param pattern regex pattern
#' @param path folder path of the database
#' @param db_name name of the database
#'
#' @export
pq_table <- function (pattern = NULL,
                      dbname = "rasyidridha",
                      host = "localhost",
                      port = "5432",
                      user = NULL,
                      password = NULL) {
  con <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(),
                                dbname = dbname,
                                host = host,
                                port = port,
                                user = user,
                                password = password)
  res <- RPostgreSQL::dbListTables(con)
  if (!is.null(pattern)) {
    res <- res[grepl(pattern, res)]
  }
  RPostgreSQL::dbDisconnect(con)
  res
}

#' Write data into table on PostgreSQL database
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
pq_write <- function (df, tbl_name, append = TRUE, overwrite = FALSE, date = TRUE,
                      dbname = "rasyidridha",
                      host = "localhost",
                      port = "5432",
                      user = NULL,
                      password = NULL) {
  con <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(),
                                dbname = dbname,
                                host = host,
                                port = port,
                                user = user,
                                password = password)
  RPostgreSQL::dbWriteTable(con, tbl_name, df,
                            row.names = FALSE,
                            append = append,
                            overwrite = overwrite)
  RPostgreSQL::dbDisconnect(con)
  return(TRUE)
}

#' Drop table on PostgreSQL database
#'
#' @md
#' @param tbl_name name of the table
#' @param path folder path of the database
#' @param db_name name of the database
#'
#' @export
pq_drop <- function (tbl_name, force = FALSE,
                     dbname = "rasyidridha",
                     host = "localhost",
                     port = "5432",
                     user = NULL,
                     password = NULL) {
  con <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(),
                                dbname = dbname,
                                host = host,
                                port = port,
                                user = user,
                                password = password)
  if (!grepl("_", tbl_name) | force == TRUE) {
    RPostgreSQL::dbRemoveTable(con, tbl_name)
    RPostgreSQL::dbDisconnect(con)
    message(sprintf("%s is removed", tbl_name))
  } else {
    RPostgreSQL::dbDisconnect(con)
    warning(sprintf("%s is not removed, use `force = TRUE`", tbl_name))
  }
}

#' Execute query on PostgreSQL database
#'
#' @md
#' @param query query statement
#' @param path folder path of the database
#' @param db_name name of the database
#'
#' @export
pq_exec <- function (query,
                     dbname = "rasyidridha",
                     host = "localhost",
                     port = "5432",
                     user = NULL,
                     password = NULL) {
  con <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(),
                                dbname = dbname,
                                host = host,
                                port = port,
                                user = user,
                                password = password)
  res <- RPostgreSQL::dbSendQuery(con, query)
  RPostgreSQL::dbDisconnect(con)
  res
}

#' Disconnect from PostgreSQL database
#'
#' @md
#' @param path folder path of the database
#' @param db_name name of the database
#'
#' @export
pq_dc <- function (dbname = "rasyidridha",
                   host = "localhost",
                   port = "5432",
                   user = NULL,
                   password = NULL) {
  con <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(),
                                dbname = dbname,
                                host = host,
                                port = port,
                                user = user,
                                password = password)
  RPostgreSQL::dbDisconnect(con)
  return(TRUE)
}



