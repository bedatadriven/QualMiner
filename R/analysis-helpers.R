
### ----------------------------------------------------------------- ###
### GGPLOT THEMES ----
### ----------------------------------------------------------------- ###

#' Theme color codes constructor
#'
#' Get HEX color codes by entering the color name. Modify the list in the
#' function body in order to add/remove colors.
#'
#' @param x color name.
#' @noRd
theme_color_codes <- function(x = NULL) {
  colors <- list(
    `UNHCR blue` = "#0072bc",
    `Almost black` = "#242934"
  )
  if (is.null(x))
    return(colors)

  colors[[x]]
}

#' \code{\link{extrafont}} package is required for custom fonts.
theme_ecuador1 <- function(border = FALSE) {

  suppressPackageStartupMessages(require("extrafont", quietly = TRUE))

  border.theme <- if (!border) {
    ggplot2::theme(
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      panel.border = element_blank()
    )
  } else {
    ggplot2::theme(NULL)
  }

  ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title = element_text(
        color = theme_color_codes("Almost black"),
        face = "bold"
      ),
      plot.subtitle = element_text(face = "italic"),
      plot.caption = element_text(size = 7),
      legend.position = "bottom",
      axis.text.x = element_text(family = "Roboto Mono", size = 10),
      axis.text.y = element_text(family = "Roboto Mono", size = 10),
      strip.text = element_text(color = "white", face = "bold"),
      strip.background = element_rect(
        color = theme_color_codes("Almost black"),
        fill = theme_color_codes("UNHCR blue"),
        size = 1.1,
        linetype = "solid"
      )
    ) +
    border.theme
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

