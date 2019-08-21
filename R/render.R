
render_bookdown <- function() {
  body <- paste(
    'wd <- getwd()',
    'if (basename(wd) != "analysis") {',
    'setwd(file.path(wd, "analysis"))',
    '}',
    'bookdown::render_book("index.Rmd", "bookdown::gitbook")',
    sep = "\n"
  )
  body <- paste("'", body, "'", sep = "\n")
  system2("Rscript", c("-e", body))
}

