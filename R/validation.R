#' Validate SDMX-CSV Data Format
#'
#' Validates that a data frame conforms to SDMX-CSV specifications and
#' the target SDMX structure requirements.
#'
#' @param data Data frame to validate
#' @param sdmx_metadata SDMX metadata object from `extract_dsd_metadata()`
#' @param strict Logical. Whether to apply strict validation (default: TRUE)
#' @param return_details Logical. Whether to return detailed validation results (default: FALSE)
#'
#' @return If `return_details = FALSE`, returns TRUE/FALSE. If `return_details = TRUE`,
#'   returns a list with validation results and details.
#'
#' @export
#' @examples
#' \dontrun{
#' # Basic validation
#' is_valid <- validate_sdmx_csv(my_data, sdmx_meta)
#' 
#' # Detailed validation
#' validation <- validate_sdmx_csv(my_data, sdmx_meta, return_details = TRUE)
#' print(validation$issues)
#' }
validate_sdmx_csv <- function(data, 
                             sdmx_metadata, 
                             strict = TRUE,
                             return_details = FALSE) {
  
  if (!inherits(sdmx_metadata, "llmx_sdmx_metadata")) {
    cli::cli_abort("sdmx_metadata must be from {.fn extract_dsd_metadata}")
  }
  
  if (!is.data.frame(data)) {
    cli::cli_abort("data must be a data frame")
  }
  
  issues <- list()
  warnings <- list()
  
  # Check required columns
  required_dims <- sdmx_metadata$dimensions$id[!is.na(sdmx_metadata$dimensions$id)]
  required_measures <- sdmx_metadata$measures$id[!is.na(sdmx_metadata$measures$id)]
  required_cols <- c(required_dims, required_measures)
  
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    issues <- append(issues, list(list(
      type = "missing_columns",
      message = glue::glue("Missing required columns: {paste(missing_cols, collapse = ', ')}"),
      severity = "error",
      columns = missing_cols
    )))
  }
  
  extra_cols <- setdiff(names(data), c(required_cols, sdmx_metadata$attributes$id))
  if (length(extra_cols) > 0 && strict) {
    warnings <- append(warnings, list(list(
      type = "extra_columns", 
      message = glue::glue("Extra columns not in SDMX structure: {paste(extra_cols, collapse = ', ')}"),
      severity = "warning",
      columns = extra_cols
    )))
  }
  
  # Validate dimension values (if codelists available)
  dimension_issues <- validate_dimension_values(data, sdmx_metadata)
  issues <- append(issues, dimension_issues)
  
  # Check for missing values in required dimensions
  for (dim_col in required_dims) {
    if (dim_col %in% names(data)) {
      na_count <- sum(is.na(data[[dim_col]]))
      if (na_count > 0) {
        issues <- append(issues, list(list(
          type = "missing_values",
          message = glue::glue("Dimension '{dim_col}' has {na_count} missing values"),
          severity = "error",
          column = dim_col,
          count = na_count
        )))
      }
    }
  }
  
  # Validate measure values
  for (measure_col in required_measures) {
    if (measure_col %in% names(data)) {
      if (!is.numeric(data[[measure_col]])) {
        issues <- append(issues, list(list(
          type = "invalid_measure_type",
          message = glue::glue("Measure '{measure_col}' must be numeric"),
          severity = "error", 
          column = measure_col
        )))
      }
    }
  }
  
  # Check data structure (should be in long format)
  structure_issues <- validate_data_structure(data, sdmx_metadata)
  issues <- append(issues, structure_issues)
  
  # Compile results
  has_errors <- any(sapply(issues, function(x) x$severity == "error"))
  has_warnings <- length(warnings) > 0 || any(sapply(issues, function(x) x$severity == "warning"))
  
  is_valid <- !has_errors
  
  if (return_details) {
    return(structure(
      list(
        is_valid = is_valid,
        has_warnings = has_warnings,
        issues = issues,
        warnings = warnings,
        validated_at = Sys.time(),
        n_rows = nrow(data),
        n_cols = ncol(data)
      ),
      class = "llmx_validation_result"
    ))
  } else {
    if (!is_valid) {
      error_messages <- sapply(issues[sapply(issues, function(x) x$severity == "error")], 
                              function(x) x$message)
      cli::cli_abort(c(
        "SDMX-CSV validation failed:",
        error_messages
      ))
    }
    
    if (has_warnings) {
      warning_messages <- sapply(warnings, function(x) x$message)
      cli::cli_warn(warning_messages)
    }
    
    return(is_valid)
  }
}

#' Validate dimension values against codelists
#' @keywords internal
validate_dimension_values <- function(data, sdmx_metadata) {
  issues <- list()
  
  # This would require codelist validation - placeholder for now
  # In a full implementation, you'd fetch and validate against actual codelists
  
  return(issues)
}

#' Validate overall data structure
#' @keywords internal  
validate_data_structure <- function(data, sdmx_metadata) {
  issues <- list()
  
  # Check if data appears to be in wide format (multiple measure columns)
  potential_measure_cols <- names(data)[sapply(data, is.numeric)]
  n_potential_measures <- length(potential_measure_cols)
  
  expected_measures <- nrow(sdmx_metadata$measures)
  
  if (n_potential_measures > expected_measures + 2) {  # Allow some flexibility
    issues <- append(issues, list(list(
      type = "wide_format_detected",
      message = "Data appears to be in wide format. SDMX-CSV requires long format.",
      severity = "warning",
      potential_measures = potential_measure_cols
    )))
  }
  
  return(issues)
}

#' Print method for validation results
#' @param x An object of class "llmx_validation_result"
#' @param ... Additional arguments (unused)
#' @export
print.llmx_validation_result <- function(x, ...) {
  cli::cli_h1("SDMX-CSV Validation Results")
  
  status_icon <- if (x$is_valid) "✓" else "✗"
  status_color <- if (x$is_valid) "green" else "red"
  
  cli::cli_text("{.{status_color} {status_icon}} Overall Status: {if (x$is_valid) 'VALID' else 'INVALID'}")
  cli::cli_text("Data: {.val {x$n_rows}} rows × {.val {x$n_cols}} columns")
  cli::cli_text("Validated: {.timestamp {x$validated_at}}")
  
  if (length(x$issues) > 0) {
    cli::cli_h2("Issues Found")
    purrr::walk(x$issues, ~ {
      icon <- if (.x$severity == "error") "✗" else "!"
      color <- if (.x$severity == "error") "red" else "yellow"
      cli::cli_text("{.{color} {icon}} {.x$message}")
    })
  }
  
  if (length(x$warnings) > 0) {
    cli::cli_h2("Warnings")
    purrr::walk(x$warnings, ~ {
      cli::cli_text("{.yellow !} {.x$message}")
    })
  }
  
  if (x$is_valid && length(x$issues) == 0 && length(x$warnings) == 0) {
    cli::cli_text("{.green All validation checks passed!}")
  }
  
  invisible(x)
}

#' Suggest SDMX Mapping Based on Data Analysis
#'
#' Analyzes source data and suggests potential mappings to SDMX dimensions
#' and attributes based on column names, data types, and content patterns.
#'
#' @param data_analysis Result from `analyze_data_structure()`
#' @param sdmx_metadata SDMX metadata from `extract_dsd_metadata()`
#' @param similarity_threshold Numeric. Minimum similarity score for suggestions (0-1, default: 0.3)
#'
#' @return A data frame with mapping suggestions
#'
#' @export
#' @examples
#' \dontrun{
#' data_analysis <- analyze_data_structure("my_data.csv")
#' sdmx_meta <- extract_dsd_metadata("https://example.org/dsd")
#' suggestions <- suggest_sdmx_mapping(data_analysis, sdmx_meta)
#' print(suggestions)
#' }
suggest_sdmx_mapping <- function(data_analysis, 
                                sdmx_metadata,
                                similarity_threshold = 0.3) {
  
  if (!inherits(data_analysis, "llmx_data_analysis")) {
    cli::cli_abort("data_analysis must be from {.fn analyze_data_structure}")
  }
  
  if (!inherits(sdmx_metadata, "llmx_sdmx_metadata")) {
    cli::cli_abort("sdmx_metadata must be from {.fn extract_dsd_metadata}")
  }
  
  source_cols <- data_analysis$columns
  
  # Create target components list
  targets <- dplyr::bind_rows(
    sdmx_metadata$dimensions |> dplyr::mutate(component_type = "dimension"),
    sdmx_metadata$attributes |> dplyr::mutate(component_type = "attribute"),
    sdmx_metadata$measures |> dplyr::mutate(component_type = "measure")
  )
  
  # Generate suggestions for each source column
  suggestions <- purrr::map_dfr(seq_len(nrow(source_cols)), ~ {
    source_col <- source_cols[.x, ]
    
    # Calculate similarity scores with target components
    target_scores <- purrr::map_dfr(seq_len(nrow(targets)), ~ {
      target <- targets[.x, ]
      
      # Name similarity (simple string distance)
      name_sim <- calculate_name_similarity(source_col$column, target$id)
      
      # Type compatibility
      type_compat <- calculate_type_compatibility(source_col, target)
      
      # Content pattern matching
      content_score <- calculate_content_similarity(source_col, target, data_analysis)
      
      # Combined score
      combined_score <- (name_sim * 0.4) + (type_compat * 0.3) + (content_score * 0.3)
      
      tibble::tibble(
        source_column = source_col$column,
        target_id = target$id,
        target_type = target$component_type,
        target_concept = target$concept_ref %||% target$id,
        name_similarity = name_sim,
        type_compatibility = type_compat,
        content_similarity = content_score,
        overall_score = combined_score
      )
    })
    
    # Return top suggestions above threshold
    target_scores |>
      dplyr::filter(overall_score >= similarity_threshold) |>
      dplyr::arrange(dplyr::desc(overall_score)) |>
      dplyr::slice_head(n = 3)  # Top 3 suggestions per source column
  })
  
  # Add source column metadata
  suggestions |>
    dplyr::left_join(
      source_cols |> dplyr::select(column, type, is_categorical, is_numeric, unique_count),
      by = c("source_column" = "column")
    ) |>
    dplyr::arrange(dplyr::desc(overall_score)) |>
    dplyr::select(source_column, target_id, target_type, target_concept, 
                 overall_score, type, is_categorical, is_numeric, everything())
}

#' Calculate name similarity between source and target
#' @keywords internal
calculate_name_similarity <- function(source_name, target_name) {
  # Simple similarity based on common substrings
  source_lower <- tolower(source_name)
  target_lower <- tolower(target_name)
  
  # Exact match
  if (source_lower == target_lower) return(1.0)
  
  # Substring matches
  if (grepl(target_lower, source_lower) || grepl(source_lower, target_lower)) {
    return(0.8)
  }
  
  # Common words
  source_words <- strsplit(source_lower, "[_\\s]+")[[1]]
  target_words <- strsplit(target_lower, "[_\\s]+")[[1]]
  
  common_words <- intersect(source_words, target_words)
  if (length(common_words) > 0) {
    return(0.6 * length(common_words) / max(length(source_words), length(target_words)))
  }
  
  return(0.0)
}

#' Calculate type compatibility
#' @keywords internal
calculate_type_compatibility <- function(source_col, target) {
  # Measures should be numeric
  if (target$component_type == "measure") {
    return(if (source_col$is_numeric) 1.0 else 0.1)
  }
  
  # Dimensions are usually categorical or codes
  if (target$component_type == "dimension") {
    if (source_col$is_categorical) return(0.9)
    if (source_col$unique_count <= 100) return(0.7)
    return(0.3)
  }
  
  # Attributes can be various types
  if (target$component_type == "attribute") {
    return(0.7)  # Neutral compatibility
  }
  
  return(0.5)
}

#' Calculate content pattern similarity
#' @keywords internal
calculate_content_similarity <- function(source_col, target, data_analysis) {
  # This is a placeholder for more sophisticated content analysis
  # In a full implementation, you might:
  # - Check for date patterns for TIME_PERIOD dimensions
  # - Check for country codes for REF_AREA dimensions  
  # - Check for currency codes for CURRENCY attributes
  # - Use regex patterns for known SDMX code formats
  
  # Simple pattern matching based on column names
  source_name <- tolower(source_col$column)
  target_id <- tolower(target$id)
  
  # Time patterns
  if (grepl("time|date|period|year|month", target_id) && 
      grepl("time|date|period|year|month", source_name)) {
    return(0.9)
  }
  
  # Area/country patterns  
  if (grepl("area|country|region", target_id) &&
      grepl("area|country|region|location", source_name)) {
    return(0.9)
  }
  
  # Value/observation patterns
  if (grepl("value|obs", target_id) && source_col$is_numeric) {
    return(0.8)
  }
  
  return(0.0)
}