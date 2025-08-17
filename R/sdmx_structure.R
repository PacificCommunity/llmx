#' Extract SDMX Data Structure Definition Metadata
#'
#' Extracts metadata from an SDMX Data Structure Definition (DSD) including
#' dimensions, attributes, measures, and codelists.
#'
#' @param dsd_url Character. URL to the SDMX DSD.
#' @param cache Logical. Whether to cache the DSD locally (default: TRUE).
#'
#' @return A list containing:
#'   - `dimensions`: Data frame with dimension information
#'   - `attributes`: Data frame with attribute information  
#'   - `measures`: Data frame with measure information
#'   - `dsd_id`: DSD identifier
#'   - `agency_id`: Agency identifier
#'   - `codelists`: List of available codelists
#'
#' @export
#' @examples
#' \dontrun{
#' # Extract OECD QNA structure
#' qna_meta <- extract_dsd_metadata(
#'   "https://stats.oecd.org/restsdmx/sdmx.ashx/GetDataStructure/QNA"
#' )
#' print(qna_meta$dimensions)
#' }
extract_dsd_metadata <- function(dsd_url, cache = TRUE) {
  
  check_packages("rsdmx")
  
  cli::cli_inform("Fetching DSD from: {.url {dsd_url}}")
  
  # Read the DSD with error handling
  dsd <- tryCatch({
    rsdmx::readSDMX(dsd_url)
  }, error = function(e) {
    cli::cli_abort(c(
      "Failed to read SDMX DSD from URL",
      "x" = "Error: {e$message}",
      "i" = "Check URL validity and network connection"
    ))
  })
  
  if (length(dsd@datastructures) == 0) {
    cli::cli_abort("No data structures found in the DSD")
  }
  
  ds <- dsd@datastructures[[1]]
  
  # Extract dimensions safely
  dimensions <- tryCatch({
    dims <- ds@DataStructureComponents@DimensionList@Dimension
    if (length(dims) > 0) {
      purrr::map_dfr(dims, ~ {
        tibble::tibble(
          id = .x@id %||% NA_character_,
          concept_ref = .x@conceptRef %||% NA_character_,
          codelist = if (!is.null(.x@LocalRepresentation)) {
            .x@LocalRepresentation@Enumeration %||% NA_character_
          } else {
            NA_character_
          },
          position = .x@position %||% NA_integer_
        )
      })
    } else {
      tibble::tibble(id = character(), concept_ref = character(), 
                    codelist = character(), position = integer())
    }
  }, error = function(e) {
    cli::cli_warn("Could not extract dimensions: {e$message}")
    tibble::tibble(id = character(), concept_ref = character(), 
                  codelist = character(), position = integer())
  })
  
  # Extract attributes safely
  attributes <- tryCatch({
    attrs <- ds@DataStructureComponents@AttributeList
    if (length(attrs) > 0 && length(attrs@Attribute) > 0) {
      purrr::map_dfr(attrs@Attribute, ~ {
        tibble::tibble(
          id = .x@id %||% NA_character_,
          concept_ref = .x@conceptRef %||% NA_character_,
          attachment_level = .x@attachmentLevel %||% NA_character_,
          codelist = if (!is.null(.x@LocalRepresentation)) {
            .x@LocalRepresentation@Enumeration %||% NA_character_
          } else {
            NA_character_
          }
        )
      })
    } else {
      tibble::tibble(id = character(), concept_ref = character(),
                    attachment_level = character(), codelist = character())
    }
  }, error = function(e) {
    cli::cli_warn("Could not extract attributes: {e$message}")
    tibble::tibble(id = character(), concept_ref = character(),
                  attachment_level = character(), codelist = character())
  })
  
  # Extract measures safely
  measures <- tryCatch({
    meas <- ds@DataStructureComponents@MeasureList
    if (length(meas) > 0 && length(meas@Measure) > 0) {
      purrr::map_dfr(meas@Measure, ~ {
        tibble::tibble(
          id = .x@id %||% NA_character_,
          concept_ref = .x@conceptRef %||% NA_character_
        )
      })
    } else {
      tibble::tibble(id = character(), concept_ref = character())
    }
  }, error = function(e) {
    cli::cli_warn("Could not extract measures: {e$message}")
    tibble::tibble(id = character(), concept_ref = character())
  })
  
  structure(
    list(
      dsd_url = dsd_url,
      dsd_id = ds@id %||% "unknown",
      agency_id = ds@agencyID %||% "unknown",
      version = ds@version %||% "1.0",
      dimensions = dimensions,
      attributes = attributes,
      measures = measures,
      extracted_at = Sys.time()
    ),
    class = "llmx_sdmx_metadata"
  )
}

#' Compare Two SDMX Structure Definitions
#'
#' Compares two SDMX DSD metadata objects to identify structural differences
#' and commonalities for mapping purposes.
#'
#' @param source_meta An SDMX metadata object (from `extract_dsd_metadata()`)
#' @param target_meta An SDMX metadata object (from `extract_dsd_metadata()`)
#'
#' @return A list containing comparison results:
#'   - `dimensions`: Comparison of dimensions
#'   - `attributes`: Comparison of attributes
#'   - `measures`: Comparison of measures
#'   - `mapping_complexity`: Assessment of mapping difficulty
#'
#' @export
#' @examples
#' \dontrun{
#' source_meta <- extract_dsd_metadata("source_dsd_url")
#' target_meta <- extract_dsd_metadata("target_dsd_url") 
#' comparison <- compare_sdmx_structures(source_meta, target_meta)
#' print(comparison$dimensions)
#' }
compare_sdmx_structures <- function(source_meta, target_meta) {
  
  if (!inherits(source_meta, "llmx_sdmx_metadata") || 
      !inherits(target_meta, "llmx_sdmx_metadata")) {
    cli::cli_abort("Both arguments must be SDMX metadata objects")
  }
  
  # Compare dimensions
  dim_comparison <- list(
    source_only = setdiff(source_meta$dimensions$id, target_meta$dimensions$id),
    target_only = setdiff(target_meta$dimensions$id, source_meta$dimensions$id),
    common = intersect(source_meta$dimensions$id, target_meta$dimensions$id),
    total_source = nrow(source_meta$dimensions),
    total_target = nrow(target_meta$dimensions)
  )
  
  # Compare attributes
  attr_comparison <- list(
    source_only = setdiff(source_meta$attributes$id, target_meta$attributes$id),
    target_only = setdiff(target_meta$attributes$id, source_meta$attributes$id),
    common = intersect(source_meta$attributes$id, target_meta$attributes$id),
    total_source = nrow(source_meta$attributes),
    total_target = nrow(target_meta$attributes)
  )
  
  # Compare measures  
  meas_comparison <- list(
    source_only = setdiff(source_meta$measures$id, target_meta$measures$id),
    target_only = setdiff(target_meta$measures$id, source_meta$measures$id),
    common = intersect(source_meta$measures$id, target_meta$measures$id),
    total_source = nrow(source_meta$measures),
    total_target = nrow(target_meta$measures)
  )
  
  # Assess mapping complexity
  complexity_score <- calculate_mapping_complexity(
    dim_comparison, attr_comparison, meas_comparison
  )
  
  structure(
    list(
      source_dsd = source_meta$dsd_id,
      target_dsd = target_meta$dsd_id,
      dimensions = dim_comparison,
      attributes = attr_comparison,
      measures = meas_comparison,
      mapping_complexity = complexity_score,
      compared_at = Sys.time()
    ),
    class = "llmx_sdmx_comparison"
  )
}

#' Calculate mapping complexity score
#' @keywords internal
calculate_mapping_complexity <- function(dim_comp, attr_comp, meas_comp) {
  
  # Simple scoring algorithm
  dim_score <- length(dim_comp$source_only) + length(dim_comp$target_only)
  attr_score <- length(attr_comp$source_only) + length(attr_comp$target_only) 
  meas_score <- length(meas_comp$source_only) + length(meas_comp$target_only)
  
  total_score <- dim_score * 3 + attr_score * 2 + meas_score * 1
  
  complexity <- dplyr::case_when(
    total_score == 0 ~ "identical",
    total_score <= 3 ~ "simple", 
    total_score <= 8 ~ "moderate",
    total_score <= 15 ~ "complex",
    TRUE ~ "very_complex"
  )
  
  list(
    score = total_score,
    level = complexity,
    dimensions_diff = dim_score,
    attributes_diff = attr_score,
    measures_diff = meas_score
  )
}

# Null-default operator
`%||%` <- function(x, y) if (is.null(x)) y else x