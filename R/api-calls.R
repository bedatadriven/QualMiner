
## ActivityInfo API calls
## =======================
##
## Example:
##   https://v4.activityinfo.org/app#form/a0234050776/table
## Form schema:
##   https://v4.activityinfo.org/resources/form/a0234050776/schema
## Sub-form schema:
##   https://v4.activityinfo.org/resources/form/cjs1c31xm2/schema
## Form query table:
##   https://v4.activityinfo.org/resources/form/a0234050776/query/rows
## Sub-form query table:
##   https://v4.activityinfo.org/resources/form/cjs1c31xm2/query/rows
##
## Some useful methods from the `activityinfo` package:
## sub.form <- activityinfo::getFormRecordTable("cjs1c31xm2")
## form <- activityinfo::getFormRecordTable("a1103026941")
## form.schema <- activityinfo::getFormSchema("a1103026941")
## db.partners <- activityinfo::getPartnersDataFrame("10297")
##
## -----------------------------------------------------------------------------
## Endpoints for v4
## ===================
## We cannot use activityInfo-R client (at the moment) because it mixes v3 and
## v4 resources unfortunately.
## - https://www.activityinfo.org/resources/database/{databaseId}
## - https://www.activityinfo.org/resources/form/{formId}/schema
## - https://www.activityinfo.org/resources/form/{formId}/query/rows
##
## these resources respectively corresponding to:
## - getDatabaseTree()*
## - getFormSchema()
## - queryTable()
##
## [*] missing from the R package.

get_database_tree <- function(databaseId) {
  activityinfo::getResource(paste("database", databaseId, sep = "/"))
}

#' Pull schema of form or sub-form
get_form_schema <- function(formId) {
  activityinfo::getResource(paste("form", formId, "schema", sep = "/"))
}

get_query_table <- function(formId) {
  activityinfo::getResource(paste("form", formId, "query/rows", sep = "/"))
}

get_record_history <- function(formId, recordId) {
  activityinfo::getResource(paste("form", formId, "record", recordId, "history", sep = "/"))
}

