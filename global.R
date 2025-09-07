production <- FALSE
library(shiny)
library(shinyjs)
library(shinyFiles)
library(shinyalert)
library(DBI)
library(RSQLite) # swap with RPostgres/MySQL if needed
library(tidyverse)
library(glue)

op <- options(digits.secs = 0)
volumes <- c(Home = fs::path_home(), "My Computer" = "/")  # Customize as needed