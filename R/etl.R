
### ----------------------------------------------------------------- ###
### RETRIEVE, CLEAN AND TRANSFORM DATA FROM ACTIVITYINFO ----
### ----------------------------------------------------------------- ###

library(dplyr)
library(activityinfo)
library(readxl)
library("ggplot2")
library(shiny)
library(knitr)
library(shinyWidgets)
library(tidyverse)
library("tidytext")
library(DT)
source("R/etl-methods.R")

database.id <- "c7qgckzkykaan9v5"

db.resources <- database_resources(database.id)

## exclude FOLDER and P.IDs (partner) in order to select FORM and SUB_FORM:
form.ids <- subset(db.resources,
                   type == "SUB_FORM" & visibility != "REFERENCE",
                   "id", drop = TRUE)

all.form.tbl <- do.call(rbind, lapply(form.ids, function(x) {
  cat(sprintf("[%d] ", which(form.ids == x)))
  make_question_response_tbl(x)
}))

