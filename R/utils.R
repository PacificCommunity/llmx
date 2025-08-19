#' Package utilities and helpers
#' @keywords internal
"_PACKAGE"

# Global variables to avoid R CMD check notes
utils::globalVariables(c(".", "agencyID", "version", "name", "description", "id"))

#' @importFrom stats setNames
NULL

#' Check if OpenAI API key is available (legacy function)
#' @keywords internal
check_openai_api_key <- function() {
  openai_key <- Sys.getenv("OPENAI_API_KEY")
  if (openai_key == "") {
    cli::cli_abort(c(
      "OpenAI API key not found.",
      "i" = "Set your API key with {.code Sys.setenv(OPENAI_API_KEY = 'your-key')}"
    ))
  }
  invisible(TRUE)
}

#' Validate that required packages are available
#' @param packages Character vector of package names
#' @keywords internal
check_packages <- function(packages) {
  missing <- packages[!sapply(packages, requireNamespace, quietly = TRUE)]
  if (length(missing) > 0) {
    cli::cli_abort(c(
      "Required packages not available: {.pkg {missing}}",
      "i" = "Install with {.code install.packages(c({paste0('\"', missing, '\"', collapse = ', ')}))})"
    ))
  }
  invisible(TRUE)
}