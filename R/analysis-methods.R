
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

#' A list of Spanish stop words
#'
#' @param colname choose a column name for the data.frame. Default is
#'   \emph{word}.
#' @references
#' \url{https://github.com/stopwords-iso/stopwords-es}
get_es_stopwords <- function(colname = "word") {
  link <- "https://raw.githubusercontent.com/bedatadriven/QualMiner/master/data-raw/stopwords-es.json"
  res <- data.frame(jsonlite::fromJSON(link), stringsAsFactors = FALSE)
  names(res) <- colname
  res
}

#' A list of Spanish sentiment words
#'
#' @references
#' \url{https://sites.google.com/site/datascienceslab/projects/multilingualsentiment}
get_es_sentiments <- function() {
  link.positive <- "https://raw.githubusercontent.com/bedatadriven/QualMiner/master/data-raw/positive_words_es.txt"
  link.negative <- "https://raw.githubusercontent.com/bedatadriven/QualMiner/master/data-raw/negative_words_es.txt"
  data.positive <- readLines(link.positive)
  data.negative <- readLines(link.negative)
  sent <- rbind(
    data.frame(word = data.positive, sentiment = "positive", stringsAsFactors = FALSE),
    data.frame(word = data.negative, sentiment = "negative", stringsAsFactors = FALSE)
  )
  sent <- sent[order(sent$word), ]
  rownames(sent) <- NULL
  sent
}

