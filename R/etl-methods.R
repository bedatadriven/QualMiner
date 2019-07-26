
database_resources <- function(databaseId) {
  database <- get_database_tree(databaseId)
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
#' @param form.schema A list returned by \code{get_form_schema}.
flatten_form_schema <- function(form.schema) {

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
    data.frame(element,
               stringsAsFactors = FALSE)
  }))

  res <- data.frame(
      as.data.frame(form.sans.elements, stringsAsFactors = FALSE),
      elements,
      stringsAsFactors = FALSE
    )

  ## remove rows where code is NA:
  res <- res[!is.na(res$code), ]

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
#' @noRd
get_query_element <- function(formId, field.code.names) {
  stopifnot(is.character(formId))
  stopifnot(is.character(field.code.names))

  queryTable <- get_query_table(formId)

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
    qt[nulls] <- NA_character_

    qnames <- names(qt)
    code.names <- qnames[qnames %in% field.code.names]

    qt.sub <- qt[code.names]

    if (!length(qt.sub) > 0) {
      return(NULL)
    }

    ## create dataframe:
    qt.sub.df <- data.frame(qt.sub, stringsAsFactors = FALSE)

    ## convert into long format:
    qt.t <- local({
      p <- data.frame(response = t(qt.sub.df), stringsAsFactors = FALSE)
      p[["code"]] <- rownames(p)
      rownames(p) <- NULL
      p[c("code", "response")]
    })

    ## also add those names which should be apparent at all times:
    fix.names <- c("Month", "@id")
    ## a note: some names contain Spanish ó (U+00F3) don't know why...
    relative.names <- list(
      partnerName = "Partner.label",
      cantonName = "Cant[o|ó]n.name",
      cantonParentName = "Cant[o|ó]n.parent.name"
    )
    found.rel.names <- sapply(base::as.character(relative.names), function(x) {
      names(qt)[grep(x, names(qt))]
    }, USE.NAMES = FALSE)
    req.names <- c(fix.names, found.rel.names)

    res <- cbind(data.frame(qt[req.names], stringsAsFactors = FALSE), qt.t)

    ## rename columns:
    colnames(res)[which(colnames(res) == "X.id")] <- "recordId"
    for (ni in seq_along(relative.names)) {
      colnames(res)[grep(base::as.character(found.rel.names)[ni], colnames(res))] <- names(relative.names)[ni]
    }
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
  form.schema <- get_form_schema(formId)

  # form schema as data.frame:
  fields <- flatten_form_schema(form.schema)

  fields.form.ids <- unique(fields$formId)

  field.questions <- fields[fields$formId == formId, ]
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
