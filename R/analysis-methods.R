
#' Get word count
#'
#' @param x character vector which length can be one or more than one.
#' @param no.punc a logical argument to remove punctuation.
#' @param show.names show text names? If this option selected, the call returns
#'   a named numeric vector.
#'
#' @examples \dontrun{
#' sample <- "  ¿Es    esta una oración?"
#' word_count(sample)
#' ## Multiple elements in vector:
#' data <- c("Hoy el clima es agradable.", "¡Oye! No quise decir esto.", "naranja")
#' word_count(data)
#' }
#' @noRd
word_count <- function(x, no.punc = TRUE, show.names = FALSE) {

  stopifnot(is.character(x))

  if (no.punc) {
    x <- gsub("[[:punct:]]", "", x)
  }

  vapply(x, function(text) {
    ## split by space:
    splitted <- unlist(strsplit(text, "[[:space:]]+"))
    ## get words number of chars more than zero:
    nchars <- vapply(splitted, nchar, numeric(1))
    length(splitted[nchars > 0])
  }, numeric(1), USE.NAMES = show.names)
}
