
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
    `Almost black` = "#242934",
    Taffy = "#F987C5"
  )
  if (is.null(x))
    return(colors)

  colors[[x]]
}

theme_ecuador1 <- function(border = FALSE, panel_spacing = 1.5) {

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
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(size = 10),
      strip.text = element_text(color = "white", face = "bold"),
      strip.background = element_rect(
        color = theme_color_codes("Almost black"),
        fill = theme_color_codes("UNHCR blue"),
        size = 1.1,
        linetype = "solid"
      ),
      panel.spacing = unit(panel_spacing, "lines")
    ) +
    border.theme
}

### ----------------------------------------------------------------- ###
### GT HELPERS ----
### ----------------------------------------------------------------- ###

gt_condensed_style <- function(data) {
  gt::tab_style(
    data,
    style = gt::cell_text(
      size = gt::px(14)
    ),
    locations = gt::cells_data()
  ) %>%
    gt::tab_options(
      heading.title.font.size = gt::px(14),
      row.padding = gt::px(6),
      table.width = gt::pct(100)
    )
}

### ----------------------------------------------------------------- ###
### RMARKDOWN HELPERS ----
### ----------------------------------------------------------------- ###

#' Create RMarkdown tabset programmatically
#'
#' @param main main title where a tabbed section starts.
#' @param description a descriptive text, paragraph etc.
#' @param tabs tabset names
#' @param body the function body run on each tabset body.
#' @details
#' It's important that the call must be in a chunk which has a \code{echo=FALSE}
#' and \code{results='asis'} option.
#' @references
#' \url{https://bookdown.org/yihui/rmarkdown/html-document.html#tabbed-sections}
create_tabset <- function(main, description, tabs, body) {
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
  cat(description, "\n\n")
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

### ----------------------------------------------------------------- ###
### RCOLORBREWER HELPERS ----
### ----------------------------------------------------------------- ###

#' Sixteen distinct colors
color_set_16 <- function() {
  c(
    RColorBrewer::brewer.pal(name = "Set1", n = 8),
    RColorBrewer::brewer.pal(name = "Accent", n = 8)
  )
}

#' Quoted from RColorBrewer documentation page:
#'
#' << There are 3 types of palettes, sequential, diverging, and qualitative.
#'
#' 1. Sequential palettes are suited to ordered data that progress from low to
#' high. Lightness steps dominate the look of these schemes, with light colors
#' for low data values to dark colors for high data values.
#'
#' 2. Diverging palettes put equal emphasis on mid-range critical values and
#' extremes at both ends of the data range. The critical class or break in the
#' middle of the legend is emphasized with light colors and low and high
#' extremes are emphasized with dark colors that have contrasting hues.
#'
#' 3. Qualitative palettes do not imply magnitude differences between legend
#' classes, and hues are used to create the primary #' visual differences
#' between classes. Qualitative schemes are best suited to representing nominal
#' or categorical data. >>
#' @references \code{link[RColorBrewer]{RColorBrewer}}
#' @name BrewerColorTypes
NULL

#' @rdname BrewerColorTypes
brewer_sequential_color_names <- function() {
  c("Blues",
    "BuGn",
    "BuPu",
    "GnBu",
    "Greens",
    "Greys",
    "Oranges",
    "OrRd",
    "PuBu",
    "PuBuGn",
    "PuRd",
    "Purples",
    "RdPu",
    "Reds",
    "YlGn",
    "YlGnBu",
    "YlOrBr",
    "YlOrRd")
}

#' @rdname BrewerColorTypes
brewer_diverging_color_names <- function() {
  c("BrBG",
    "PiYG",
    "PRGn",
    "PuOr",
    "RdBu",
    "RdGy",
    "RdYlBu",
    "RdYlGn",
    "Spectral")
}

### ----------------------------------------------------------------- ###
### PACKAGE HELPERS ----
### ----------------------------------------------------------------- ###

#' Check all required packages to render this RMarkdown document
#' @noRd
check_required_packages <- function() {
  ## define packages here:
  packages <- list(
    cran = c(
      "devtools",
      "knitr",
      "jsonlite",
      "tidyverse",
      "conflicted",
      "tidytext",
      "tsibble",
      "scales",
      "treemapify",
      "hunspell",
      "corpus",
      "wordcloud",
      "cowplot",
      "bookdown",
      "refinr"
    ),
    github = list(
      "sass" = "rstudio/sass",
      "gt" = "rstudio/gt",
      "mmy" = "strboul/mmy",
      "inspectdf" = "alastairrushworth/inspectdf"
    )
  )
  which.cran <- packages$cran %in% installed.packages()
  cran.msg <- if (!all(which.cran)) {
    sprintf("  install.packages(c(%s))",
            paste("'", packages$cran[!which.cran], "'", sep = "", collapse = ", "))
  } else {
    NULL
  }
  which.github <- names(packages$github) %in% installed.packages()
  github.msg <- if(!all(which.github)) {
    sprintf("  devtools::install_github(c(%s))",
            paste("'", base::as.character(packages$github)[!which.github], "'", sep = "", collapse = ", "))
  } else {
    NULL
  }
  if (!(all(which.cran) && all(which.github))) {
    stop(
      paste(
        "Not all packages found in the system.",
        "  Please run the script(s) below to install the required packages and then you can proceed:\n",
        cran.msg,
        github.msg,
        sep = "\n"
      )
    )
  }
  invisible(packages$cran)
}

