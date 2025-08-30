production <- FALSE
library(shiny)
library(DBI)
library(RSQLite) # swap with RPostgres/MySQL if needed


# ---- Database connection (example with SQLite) ----
if (production){
  con <- dbConnect(RSQLite::SQLite(), "data/FinnPrio_DB_v01.db")
} else {
  con <- dbConnect(RSQLite::SQLite(), "data/FinnPrio_DB_v01_test.db")
}
