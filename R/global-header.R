
library(activityinfo)

#' Check version and throw a warning about R version
#'
#' If this was a package, that could be done by specifying the R version in the
#' Depends field. It is important to warn the users about R version
#' incompatibility issues.
#'
#' @noRd
check_R_version <- function() {
  rv <- R.version
  current.version <- paste(rv$major, rv$minor, sep = ".")
  tested.versions <- c("3.5.3", "3.6.0")
  if (!current.version %in% tested.versions) {
    warning(
      paste(
        "The R version you're using isn't tested and you may have compatibilty issues (or you may not).",
        sprintf(
          "Please use one of the %s versions instead of %s to overcome this issue.",
          paste(tested.versions, collapse = ", "),
          current.version
        ),
        "You can ignore this message if everything works as usual.",
        sep = "\n"
      ),
      call. = FALSE)
  } else {
    invisible(NULL)
  }
}
check_R_version()

### ----------------------------------------------------------------- ###
### GLOBAL VARIABLES & OPTIONS ----
### ----------------------------------------------------------------- ###

TEXT.DATA.PATH <- file.path("data", "all-form-table.json")

## Set this option due to error in curl http/2 layer. Uncomment this option if
## you don't have such problems with your current R curl version.
## See also: https://github.com/cloudyr/googleCloudStorageR/issues/71 and
## https://github.com/jeroen/curl/issues/156
httr::set_config(httr::config(http_version = 0))

### ----------------------------------------------------------------- ###
### GGPLOT THEMES ----
### ----------------------------------------------------------------- ###

theme_ecuador1 <- function() {
  ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title = element_text(color = "black", face = "bold"),
      plot.subtitle = element_text(face = "italic"),
      plot.caption = element_text(size = 7),
      strip.text = element_text(color = "white", face = "bold"),
      strip.background = element_rect(
        color="black", fill="#0072bc", size=1.1, linetype="solid"
      )
    )
}

### ----------------------------------------------------------------- ###
### RMARKDOWN HELPERS ----
### ----------------------------------------------------------------- ###

#' A wrapper to knitr::kable that truncates the data frame/tibble columns
#'
#' It's better to truncate values e.g. when a data.frame have a column
#' containing long text.
#'
#' @param tbl a data frame
#' @param which column name(s) whose content to be truncated
#' @importFrom knitr kable
kable_truncate <- function(tbl, which, trunc.level = 50) {
  if(!all(which %in% names(tbl))) {
    stop("column name(s) not found in the table")
  }
  for (i in seq_along(which)) {
    col <- which[i]
    tbl[[col]] <- sapply(tbl[[col]], function(el) {
      if (nchar(el) > trunc.level) {
        paste0(trimws(strtrim(el, as.integer(trunc.level))), "...")
      } else {
        el
      }
    })
  }
  knitr::kable(tbl)
}

#' Create RMarkdown tabset programmatically
#'
#' @param main main title where a tabbed section starts.
#' @param tabs tabset names
#' @param body the function body run on each tabset body.
#' @details
#' It's important that the call must be in a chunk which has a \code{echo=FALSE}
#' and \code{results='asis'} option.
#' @references
#' \url{https://bookdown.org/yihui/rmarkdown/html-document.html#tabbed-sections}
create_tabset <- function(main, tabs, body) {
  stopifnot(.has_hash(main))
  hash.length <- .length_hash(main)
  tab.titles <- sprintf("%s %s", .make_hash(hash.length + 1L), tabs)
  cat(main, "{.tabset}", "\n\n")
  for (i in seq_along(tabs)) {
    cat(tab.titles[i])
    cat("\n\n")
    print(eval(parse(text=body[i])))
    cat("\n\n")
  }
  cat("\n")
}

.has_hash <- function(text) {
  if (length(grep("^#+", text)) > 0) TRUE else FALSE
}

#' Count number of hash symbols in the main title:
.length_hash <- function(text) {
  lengths(regmatches(text, gregexpr("#", text)))
}

.make_hash <- function(length) {
  paste(rep("#", length), collapse = "")
}

