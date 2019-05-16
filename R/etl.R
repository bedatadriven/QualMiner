
### ----------------------------------------------------------------- ###
### RETRIEVE, CLEAN AND TRANSFORM DATA FROM ACTIVITYINFO ----
### ----------------------------------------------------------------- ###

source(file.path("R", "global-header.R"))
source(file.path("R", "etl-methods.R"))
source(file.path("R", "api-calls.R"))

## 'ECUADOR_MONITOREO' database:
database.id <- "10297"

db.resources <- database_resources(database.id)

## exclude FOLDER and P.IDs (partner) in order to select FORM and SUB_FORM:
form.ids <- subset(db.resources,
                   type != "FOLDER" & visibility != "REFERENCE",
                   "id", drop = TRUE)

all.form.tbl <- do.call(rbind, lapply(form.ids, function(x) {
  make_question_response_tbl(x)
}))

## merge folder and form information with (sub-)form table:
wide.db.resources <- wide_format_db_resources(db.resources)
all <- merge(all.form.tbl, wide.db.resources, by.x = "id", by.y = "idSubForms")

## bring some colnames to the left:
cols <- c("idFolder", "labelFolder", "idForms", "labelForms", "labelSubForms")
all <- all[, c(cols, setdiff(names(all), cols)) ]

## rename canton names based on this
## https://en.wikipedia.org/wiki/Provinces_of_Ecuador
colnames(all)[colnames(all) == "cantonName"] <- "capital"
colnames(all)[colnames(all) == "cantonParentName"] <- "province"

## Writing data to disk
## Use JSON format bcs CSV isn't very suitable for storing textual data (e.g.
## quotes, commas etc. cause errors).
all.form.json <- jsonlite::toJSON(all, pretty = TRUE)
writeLines(all.form.json, con = TEXT.DATA.PATH)

