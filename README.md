# mrsq
Rasyid Ridha's personal R package used for:

- SQLite database connection
- Utility R functions

## Installation

```
# install.packages("devtools")
devtools::install_github("rasyidstat/mrsq")
```

## Usage

You can start using `mrsq` by initializing SQLite database.

```
library(mrsq)
sqlite_start(path = "~/Test", db_name = "test.sqlite")
sqlite_init(path = "~/Test", db_name = "test.sqlite")
sqlite_write(mtcars, "test_mtcars")
```
