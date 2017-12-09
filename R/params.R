# db folder location
.dbPath <- function() {
  "B:/Cloud/OneDrive/Magnum Opus/project/data/db"
}

# db file location
.dbLoc <- function() {
  sprintf("%s/db_qs.sqlite", .dbPath())
}
