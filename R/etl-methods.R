database_resources <- function(databaseId) {
  database <- getDatabaseTree(databaseId)
  activities <- do.call(rbind, lapply(database$resources, function(x) {
    data.frame(
      id = x$id,
      parentId = x$parentId,
      label = x$label,
      type = x$type,
      visibility = x$visibility,
      stringsAsFactors = FALSE
    )
  }))
  res <- data.frame(
    databaseName = database$label,
    databaseId = database$databaseId,
    activities,
    stringsAsFactors = FALSE
  )
  res
}

#' Flatten form schema as a table
#'
#' @param form.schema A list returned by \code{getFormSchema}.
#' @param coded.only A logical to indicate if only fields with codes should be included.
flatten_form_schema <- function(form.schema, coded.only = TRUE) {

  form <- form.schema

  ## pop elements list
  form.sans.elements <- form[-which(names(form) == "elements")]
  if (is.null(form.sans.elements[["parentFormId"]])) {
    form.sans.elements[["parentFormId"]] <- NA_character_
  }
  if (is.null(form.sans.elements[["subFormKind"]])) {
    form.sans.elements[["subFormKind"]] <- NA_character_
  }

  elements <- do.call(rbind, lapply(seq_along(form$elements), function(j) {
    element <- form$elements[[j]]
    ## turn NULL to NA:
    nulls <- sapply(element, is.null)
    element[nulls] <- NA_character_
    ## add 'key' if not exists:
    if (!"key" %in% names(element)) {
      element[["key"]] <- NA_character_
    }
    ## exclude typeParameters sub-list (if exists):
    if ("typeParameters" %in% names(element)) {
      element <- element[-which(names(element) == "typeParameters")]
    }
    data.frame(unclass(element),
               stringsAsFactors = FALSE)
  }))

  res <- data.frame(
    as.data.frame(form.sans.elements, stringsAsFactors = FALSE),
    elements,
    stringsAsFactors = FALSE
  )

  if (isTRUE(coded.only)) {
    ## remove rows where code is NA:
    res <- res[!is.na(res$code),]
  }

  ### prettify data --------------------------------------- ###
  ## drop columns:
  remove.cols <-
    c(
      "schemaVersion",
      "subFormKind",
      "label",
      "id.1",
      "relevanceCondition",
      "visible",
      "key"
    )
  res <- res[, !(names(res) %in% remove.cols)]
  ## reorder columns:
  first.cols <- c("databaseId", "id")
  res <- res[, c(first.cols, setdiff(names(res), first.cols))]
  ## rename columns:
  colnames(res)[which(colnames(res) == "id")] <- "formId"
  colnames(res)[which(colnames(res) == "label.1")] <- "question"

  data.frame(res, stringsAsFactors = FALSE)
}

#' Get elements of query
#'
#' @param formId form ID as character.
#' @param field.code.names character vector containing form field code names
#'   (subsetted by the same formId).
#' @details For the 2020 database we are only querying subforms. The fields
#' with a code (as contained in \code{field.code.names} include fields which
#' are common to most, if not all, subforms in the database. These common fields
#' provide metadata for fields with quantitative and narrative information.
#' @noRd
get_query_element <- function(formId, field.code.names) {
  stopifnot(is.character(formId))
  stopifnot(is.character(field.code.names))

  queryTable <- activityinfo:::getResource(paste("form", formId, "query/rows", sep = "/"))

  if (length(queryTable) > 0) {
    cat("getting query table: \033[1m", formId, "\033[0m\n")
  } else {
    ## gracefully return for empty tables:
    cat("query table empty: \033[1m", formId, "\033[0m\n")
    return(NULL)
  }

  ## can always get the first element because schema is the same:
  tbl <- do.call(rbind, lapply(seq_along(queryTable), function(i) {
    qt <- queryTable[[i]]

    ## first convert NULL to NA:
    nulls <- sapply(qt, is.null)
    if (length(nulls)) {
      qt[nulls] <- NA_character_
    }

    # common names are fields (in the subform) which are not quantity
    # or quantitative fields, but provide additional information such
    # as the reporting month,activity type, and donor:
    common.names <- list(
      Month = "Month$|\\w*Mes$",
      Activity = "\\w*Act$",
      Donor = "\\w*Donor(\\.Agency)?$", # reference field!
      PRRM = "\\w*PRRM$",
      COVID = "\\w*COVID$",
      Implementation = "\\w*Impl$",
      Meta = "\\w*Meta$",
      Modality = "\\w*Mod$",
      Comment = "\\w*FREE_TEXT$|\\w*TEXTO_LIBRE$"
    )
    found.common.codes <- sapply(common.names, function(x) {
      grep(pattern = x, field.code.names, ignore.case = TRUE, value = TRUE)
    }, USE.NAMES = FALSE)

    qnames <- names(qt)
    # only retrieve data from fields with codes and where is code is not marked
    # as being a common field among multiple subforms:
    code.names <- setdiff(qnames[qnames %in% field.code.names], found.common.codes)

    qt.sub <- qt[code.names]

    if (!length(qt.sub) > 0) {
      return(NULL)
    }

    ## create dataframe:
    qt.sub.df <- data.frame(qt.sub, stringsAsFactors = FALSE)

    ## convert into long format:
    qt.t <- data.frame(code = names(qt.sub), response = unname(unlist(qt.sub)))

    # the query result always includes the record identifier:
    fix.names <- list(recordId = "@id")
    # relative names are (generally) fields in a parent form:
    relative.names <- list(
      userName = "Parent\\.\\w*User\\.Name$",
      partnerName = "Partner\\.label|Parent\\.Socio\\.Name$",
      cantonName = "Cant.n\\.name",
      cantonParentName = "Cant.n\\.parent\\.name"
    )
    all.nms <- c(fix.names, relative.names, common.names)
    extract <- lapply(all.nms, function(pattern) {
      i <- grep(pattern, names(qt), ignore.case = TRUE)
      if (length(i) == 0L) return(NA_character_)
      if (length(i) == 1L) return(qt[[i]])
      if (length(i) > 1L) return(qt[[i[1]]])
    })

    res <- cbind(data.frame(extract, stringsAsFactors = FALSE), qt.t)

    ## rename columns:
    #colnames(res)[which(colnames(res) == "X.id")] <- "recordId"
    #for (ni in seq_along(relative.names)) {
    #  colnames(res)[grep(base::as.character(found.rel.names)[ni], colnames(res))] <- names(relative.names)[ni]
    #}
    ## reorder columns:
    first.cols <- c("recordId", "Month")
    res <- res[, c(first.cols, setdiff(names(res), first.cols))]

    res
  }))

  if (!length(tbl) > 0) {
    return(NULL)
  }

  ## table row order:
  tbl <- tbl[order(tbl[["Month"]]), ]

  data.frame(formId, tbl, stringsAsFactors = FALSE)
}

make_question_response_tbl <- function(formId) {

  # pull form and sub-form schemas:
  form.schema <- getFormSchema(formId)

  # form schema as data.frame:
  fields <- flatten_form_schema(form.schema, coded.only = FALSE)

  fields.form.ids <- unique(fields$formId)

  field.questions <- fields[fields$formId == formId,]
  field.responses <- get_query_element(formId, field.questions$code)

  if (is.null(field.responses)) {
    return(data.frame())
  }

  merged <- merge(field.questions, field.responses, by = c("formId", "code"))

  ## reorder columns:
  first.cols <- c("databaseId", "formId", "recordId", "Month",
                  "code", "question", "description", "response")
  merged <- merged[, c(first.cols, setdiff(names(merged), first.cols))]
  ## rename columns:
  colnames(merged)[which(colnames(merged) == "formId")] <- "id"

  merged
}

#' Transform database resources into wide-format
#'
#' @param x data.frame returned by \code{database_resources}.
#' @noRd
wide_format_db_resources <- function(x) {
  subforms <- x[x$type == "SUB_FORM", c("parentId", "id", "label")]
  forms <- x[x$type == "FORM", c("parentId", "id", "label")]
  folder <- x[x$type == "FOLDER", c("id", "label")]

  ff <- merge(folder, forms, by.x = "id", by.y = "parentId", suffixes = c("Folder", "Forms"))
  long <- merge(ff, subforms, by.x = "idForms", by.y = "parentId", suffixes = c("", "SubForms"))

  ## rename columns:
  colnames(long)[which(colnames(long) == "id")] <- "idFolder"
  colnames(long)[which(colnames(long) == "label")] <- "labelSubForms"

  ## reorder columns:
  long <- long[c("idFolder", "labelFolder", "idForms", "labelForms", "idSubForms", "labelSubForms")]
  long
}

get_unique_users_from_records <- function(subFormIds, table.all) {
  cat("Pulling record history... (This may take a while)\n")
  tbl <- do.call(rbind, lapply(seq_along(subFormIds), function(s) {
    sfId <- subFormIds[s]
    recordIds <- unique(table.all[table.all$id == sfId, "recordId"])
    recordIds.len <- length(recordIds)
    noEmailsInRecords <- do.call(rbind, lapply(seq_along(recordIds), function(i) {
      recId <- recordIds[i]
      ## informative progress text sent to console:
      cat(sprintf("\r%s",
                  paste(
                    paste0("[\033[1m", sfId, "\033[0m]"),
                    "getting record history",
                    paste0("(", i, "/", recordIds.len, ")")
                  )))
      history <- getRecordHistory(sfId, recId)
      entries <- history[["entries"]]
      ## count `userEmail` as 'unique users':
      emails <- unique(sapply(seq_along(entries), function(e) entries[[e]][["userEmail"]]))
      data.frame(
        id = sfId,
        recordId = recId,
        reportingUsers = I(list(emails)),
        stringsAsFactors = FALSE
      )
    }))
    cat("\n")
    noEmailsInRecords
  }))
  cat("Pulling completed!\n")
  tbl
}

