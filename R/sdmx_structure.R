#' Auto-detect SDMX endpoint from agency ID
#' 
#' @param agency_id Character. Agency identifier
#' @return SDMXEndpoint object or NULL if no match found
#' @keywords internal
auto_detect_endpoint <- function(agency_id) {
  # Load available endpoints
  endpoints_data <- sdmxdata::endpoints
  
  # Direct match first
  if (agency_id %in% endpoints_data$id) {
    return(sdmxdata::get_endpoint(agency_id))
  }
  
  # Pattern matching for complex agency IDs (e.g., OECD.ELS.JAI -> OECD)
  for (i in seq_len(nrow(endpoints_data))) {
    endpoint_id <- endpoints_data$id[i]
    # Check if agency_id starts with the endpoint id (case insensitive)
    if (grepl(paste0("^", endpoint_id), agency_id, ignore.case = TRUE)) {
      return(sdmxdata::get_endpoint(endpoint_id))
    }
  }
  
  return(NULL)
}

#' Extract SDMX Data Structure Definition Metadata
#'
#' Extracts metadata from an SDMX Data Structure Definition (DSD) including
#' dimensions, attributes, measures, and codelists using the sdmxdata package.
#'
#' @param endpoint Character. SDMX endpoint URL or endpoint object.
#' @param agency_id Character. Agency identifier (e.g., "OECD").
#' @param dataflow_id Character. Dataflow identifier (e.g., "QNA").
#' @param version Character. Version of the dataflow (default: "latest").
#' @param cache Logical. Whether to cache the DSD locally (default: TRUE).
#'
#' @return A list containing:
#'   - `dimensions`: Data frame with dimension information including codes
#'   - `attributes`: Data frame with attribute information  
#'   - `measures`: Data frame with measure information
#'   - `dsd_id`: DSD identifier
#'   - `agency_id`: Agency identifier
#'   - `name`: Dataflow name
#'   - `description`: Dataflow description
#'   - `raw_structure`: Complete sdmxdata structure object
#'
#' @export
#' @examples
#' \dontrun{
#' # Extract Pacific Data Hub trade structure
#' trade_meta <- extract_dsd_metadata(
#'   endpoint = "https://stats-sdmx-disseminate.pacificdata.org/rest",
#'   agency_id = "SPC",
#'   dataflow_id = "DF_TRADE_FOOD",
#'   version = "2.0"
#' )
#' print(trade_meta$dimensions)
#' }
extract_dsd_metadata <- function(endpoint = NULL, agency_id, dataflow_id, version = "latest", cache = TRUE) {
  
  check_packages("sdmxdata")
  
  cli::cli_inform("Fetching DSD for: {agency_id}:{dataflow_id}({version})")
  
  # Auto-detect endpoint if not provided
  if (is.null(endpoint)) {
    endpoint <- auto_detect_endpoint(agency_id)
    if (!is.null(endpoint)) {
      cli::cli_inform("Auto-detected endpoint: {endpoint$name}")
    } else {
      cli::cli_warn("Could not auto-detect endpoint for agency: {agency_id}")
    }
  }
  
  # Get dataflow structure using sdmxdata
  dfs <- tryCatch({
    sdmxdata::get_dataflow_structure(
      endpoint = endpoint,
      agencyID = agency_id,
      id = dataflow_id,
      version = version,
      cache = cache
    )
  }, error = function(e) {
    cli::cli_abort(c(
      "Failed to get SDMX dataflow structure",
      "x" = "Error: {e$message}",
      "i" = "Check endpoint, agency ID, and dataflow ID",
      "i" = "Available endpoints: {paste(sdmxdata::endpoints$id, collapse = ', ')}"
    ))
  })
  
  # Extract dimensions
  dimensions <- if (length(dfs$dimensions) > 0) {
    purrr::map_dfr(dfs$dimensions, ~ {
      tibble::tibble(
        id = .x$id %||% NA_character_,
        name = .x$name %||% NA_character_,
        description = .x$description %||% NA_character_,
        position = .x$position %||% NA_integer_,
        type = .x$type %||% NA_character_,
        role = .x$role %||% NA_character_,
        codes = list(.x$codes)
      )
    })
  } else {
    tibble::tibble(
      id = character(), name = character(), description = character(),
      position = integer(), type = character(), role = character(),
      codes = list()
    )
  }
  
  # Extract attributes
  attributes <- if (length(dfs$attributes) > 0) {
    purrr::map_dfr(dfs$attributes, ~ {
      tibble::tibble(
        id = .x$id %||% NA_character_,
        name = .x$name %||% NA_character_,
        description = .x$description %||% NA_character_,
        text_format = .x$text_format %||% NA_character_,
        codes = list(.x$codes)
      )
    })
  } else {
    tibble::tibble(
      id = character(), name = character(), 
      description = character(), text_format = character(),
      codes = list()
    )
  }
  
  # Extract measures
  measures <- if (!is.null(dfs$measure)) {
    tibble::tibble(
      id = dfs$measure$id %||% NA_character_,
      name = dfs$measure$name %||% NA_character_,
      description = dfs$measure$description %||% NA_character_,
      text_format = dfs$measure$text_format %||% NA_character_
    )
  } else {
    tibble::tibble(
      id = character(), name = character(), 
      description = character(), text_format = character()
    )
  }
  
  structure(
    list(
      endpoint = endpoint,
      dsd_id = dfs$id,
      agency_id = dfs$agencyID,
      version = dfs$version,
      name = dfs$name,
      description = dfs$description,
      dimensions = dimensions,
      attributes = attributes,
      measures = measures,
      extracted_at = Sys.time(),
      raw_structure = dfs
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
#' source_meta <- extract_dsd_metadata(
#'   endpoint = "https://stats-sdmx-disseminate.pacificdata.org/rest",
#'   agency_id = "SPC", dataflow_id = "DF_TRADE_FOOD", version = "2.0"
#' )
#' target_meta <- extract_dsd_metadata(
#'   endpoint = "https://stats-sdmx-disseminate.pacificdata.org/rest", 
#'   agency_id = "SPC", dataflow_id = "DF_CPI", version = "1.0"
#' )
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