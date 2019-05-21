
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

