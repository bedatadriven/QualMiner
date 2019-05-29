
### ----------------------------------------------------------------- ###
### GGPLOT THEMES ----
### ----------------------------------------------------------------- ###

#' \code{\link{extrafont}} package is required for custom fonts.
theme_ecuador1 <- function() {
  suppressPackageStartupMessages(require("extrafont", quietly = TRUE))
  ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title = element_text(color = "black", face = "bold"),
      plot.subtitle = element_text(face = "italic"),
      plot.caption = element_text(size = 7),
      axis.text.x = element_text(family = "Roboto Mono", size = 10),
      axis.text.y = element_text(family = "Roboto Mono", size = 10),
      strip.text = element_text(color = "white", face = "bold"),
      strip.background = element_rect(
        color = "black",
        fill = "#0072bc",
        size = 1.1,
        linetype = "solid"
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
  stopifnot(.has_hash(main))
  hash.length <- .length_hash(main)
  tab.titles <- sprintf("%s %s", .make_hash(hash.length + 1L), tabs)
  cat(main, "{.tabset}", "\n\n")
  for (i in seq_along(tabs)) {
    cat(tab.titles[i])
    cat("\n\n")
    print(eval(parse(text=body[i])))
    cat("\n\n")
    cat("***\n") # <hr> tag for rmarkdown..
    cat("\n")
  }
  cat("\n")
}

