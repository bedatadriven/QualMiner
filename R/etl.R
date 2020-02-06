
### ----------------------------------------------------------------- ###
### RETRIEVE, CLEAN AND TRANSFORM DATA FROM ACTIVITYINFO ----
### ----------------------------------------------------------------- ###

debugSource(file.path("R", "global-header.R"))
debugSource(file.path("R", "etl-methods.R"))

## 'ECUADOR_MONITOREO' database:
database.id <- "d0000010297"

db.resources <- database_resources(database.id)

## exclude FOLDER and P.IDs (partner) in order to select FORM and SUB_FORM:
form.ids <- subset(db.resources,
                   type != "FOLDER" & visibility != "REFERENCE",
                   "id", drop = TRUE)

all.form.tbl <- do.call(rbind, lapply(form.ids, function(x) {
  cat(sprintf("[%d] ", which(form.ids == x)))
  make_question_response_tbl(x)
}))

## merge folder and form information with (sub-)form table:
wide.db.resources <- wide_format_db_resources(db.resources)
all <- merge(all.form.tbl, wide.db.resources, by.x = "id", by.y = "idSubForms")

## merge databaseName:
all <- merge(all, unique(db.resources[c("databaseName", "databaseId")]), by.x = "databaseId", by.y = "databaseId")

## add number of individuals enter data per sub-form:
#sub_form_ids <- unique(all$id)
#reportingUsersPerRecord <- get_unique_users_from_records(sub_form_ids, all)

#allUs <- merge(all, reportingUsersPerRecord, by = c("id", "recordId"))
allUs <- all
allUs$reportingUsers <- NA


## rename canton names based on this
## https://en.wikipedia.org/wiki/Provinces_of_Ecuador
## Rename, order and drop some columns:
cols <- list(
  databaseId     =   "databaseId",
  databaseName   =   "databaseName",
  folderId       =   "idFolder",
  folderName     =   "labelFolder",
  formId         =   "idForms",
  formName       =   "labelForms",
  subFormId      =   "id",
  subFormName    =   "labelSubForms",
  recordId       =   "recordId",
  Month          =   "Month",
  code           =   "code",
  question       =   "question",
  response       =   "response",
  required       =   "required",
  type           =   "type",
  partnerName    =   "partnerName",
  canton         =   "cantonName",
  province       =   "cantonParentName",
  description    =   "description",
  reportingUsers =   "reportingUsers"
)
for (i in seq_along(cols)) {
  colnames(allUs)[colnames(allUs) == cols[[i]]] <- names(cols)[i]
}
allUs <- allUs[names(cols)]

write.csv(allUs, file = "export.csv")

## Writing data to disk
## Use JSON format bcs CSV isn't very suitable for storing textual data (e.g.
## quotes, commas etc. cause errors).
all.form.json <- jsonlite::toJSON(allUs, pretty = TRUE)
writeLines(all.form.json, con = TEXT.DATA.PATH)

