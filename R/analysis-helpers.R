
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
      legend.position = "bottom",
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

