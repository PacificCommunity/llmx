#' Analyze Data Structure for SDMX Mapping
#'
#' Analyzes the structure of a data file (CSV, Excel) to understand its format
#' and prepare for SDMX mapping suggestions. Enhanced with sophisticated
#' data profiling including temporal pattern detection, hierarchical relationships,
#' and data quality assessment.
#'
#' @param file_path Character. Path to the data file (CSV or Excel).
#' @param sheet Character. Sheet name for Excel files (default: first sheet).
#' @param sample_rows Integer. Number of rows to sample for analysis (default: 1000).
#' @param encoding Character. File encoding (default: "UTF-8").
#' @param detect_hierarchies Logical. Detect hierarchical relationships (default: TRUE).
#' @param detect_temporal_patterns Logical. Detect temporal patterns (default: TRUE).
#' @param quality_assessment Logical. Perform data quality assessment (default: TRUE).
#'
#' @return A list containing:
#'   - `columns`: Data frame with enhanced column information
#'   - `data_types`: Detected data types for each column
#'   - `sample_data`: Sample of the actual data
#'   - `summary_stats`: Comprehensive statistics for numeric columns
#'   - `unique_values`: Sample unique values for categorical columns
#'   - `temporal_patterns`: Detected temporal patterns and date formats
#'   - `hierarchical_relationships`: Detected hierarchical column relationships
#'   - `data_quality`: Comprehensive data quality assessment
#'   - `sdmx_readiness`: Assessment of SDMX mapping readiness
#'
#' @export
#' @examples
#' \dontrun{
#' analysis <- analyze_data_structure("data/my_data.csv")
#' print(analysis$columns)
#' }
analyze_data_structure <- function(file_path, 
                                  sheet = NULL, 
                                  sample_rows = 1000L,
                                  encoding = "UTF-8",
                                  detect_hierarchies = TRUE,
                                  detect_temporal_patterns = TRUE,
                                  quality_assessment = TRUE) {
  
  # Validate inputs
  if (!file.exists(file_path)) {
    cli::cli_abort("File not found: {.path {file_path}}")
  }
  
  # Determine file type and read data
  file_ext <- tolower(tools::file_ext(file_path))
  
  data <- switch(file_ext,
    "csv" = readr::read_csv(file_path, 
                           locale = readr::locale(encoding = encoding),
                           show_col_types = FALSE),
    "xlsx" = ,
    "xls" = readxl::read_excel(file_path, sheet = sheet),
    cli::cli_abort("Unsupported file type: {.val {file_ext}}")
  )
  
  # Sample data if needed
  if (nrow(data) > sample_rows) {
    cli::cli_inform("Sampling {sample_rows} rows from {nrow(data)} total rows")
    data <- dplyr::slice_sample(data, n = sample_rows)
  }
  
  # Analyze column structure
  columns_info <- data |>
    dplyr::summarise(
      dplyr::across(dplyr::everything(), list(
        type = ~ class(.x)[1],
        na_count = ~ sum(is.na(.x)),
        unique_count = ~ dplyr::n_distinct(.x, na.rm = TRUE)
      )),
      .groups = "drop"
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::everything(),
      names_to = c("column", "metric"),
      names_sep = "_(?=type|na_count|unique_count)$",
      values_to = "value"
    ) |>
    tidyr::pivot_wider(
      names_from = metric,
      values_from = value
    ) |>
    dplyr::mutate(
      na_proportion = as.numeric(na_count) / nrow(data),
      is_categorical = unique_count <= 20 & type %in% c("character", "factor"),
      is_numeric = type %in% c("numeric", "double", "integer"),
      is_date = type %in% c("Date", "POSIXct", "POSIXt")
    )
  
  # Get sample unique values for categorical columns
  categorical_cols <- columns_info |>
    dplyr::filter(is_categorical) |>
    dplyr::pull(column)
  
  unique_values <- purrr::map(categorical_cols, ~ {
    unique_vals <- unique(data[[.x]])
    unique_vals <- unique_vals[!is.na(unique_vals)]
    if (length(unique_vals) > 10) {
      unique_vals[1:10]
    } else {
      unique_vals
    }
  }) |>
    purrr::set_names(categorical_cols)
  
  # Summary statistics for numeric columns
  numeric_cols <- columns_info |>
    dplyr::filter(is_numeric) |>
    dplyr::pull(column)
  
  summary_stats <- if (length(numeric_cols) > 0) {
    data |>
      dplyr::select(dplyr::all_of(numeric_cols)) |>
      dplyr::summarise(
        dplyr::across(dplyr::everything(), list(
          min = ~ min(.x, na.rm = TRUE),
          max = ~ max(.x, na.rm = TRUE),
          mean = ~ mean(.x, na.rm = TRUE),
          median = ~ median(.x, na.rm = TRUE)
        ))
      ) |>
      tidyr::pivot_longer(
        cols = dplyr::everything(),
        names_to = c("column", "statistic"),
        names_sep = "_(?=min|max|mean|median)$",
        values_to = "value"
      ) |>
      tidyr::pivot_wider(
        names_from = statistic,
        values_from = value
      )
  } else {
    tibble::tibble()
  }
  
  # Enhanced profiling features
  temporal_patterns <- if (detect_temporal_patterns) {
    detect_temporal_patterns_enhanced(data, columns_info)
  } else {
    list()
  }
  
  hierarchical_relationships <- if (detect_hierarchies) {
    detect_hierarchical_relationships_enhanced(data, columns_info)
  } else {
    list()
  }
  
  data_quality <- if (quality_assessment) {
    assess_data_quality_enhanced(data, columns_info)
  } else {
    list()
  }
  
  sdmx_readiness <- assess_sdmx_readiness(columns_info, temporal_patterns, data_quality)
  
  structure(
    list(
      file_path = file_path,
      file_type = file_ext,
      n_rows = nrow(data),
      n_cols = ncol(data),
      columns = columns_info,
      sample_data = dplyr::slice_head(data, n = 5),
      summary_stats = summary_stats,
      unique_values = unique_values,
      temporal_patterns = temporal_patterns,
      hierarchical_relationships = hierarchical_relationships,
      data_quality = data_quality,
      sdmx_readiness = sdmx_readiness
    ),
    class = "llmx_data_analysis"
  )
}

#' Print method for data analysis results
#' @param x An object of class "llmx_data_analysis"
#' @param ... Additional arguments (unused)
#' @export
print.llmx_data_analysis <- function(x, ...) {
  cli::cli_h1("Enhanced Data Structure Analysis")
  cli::cli_text("File: {.path {x$file_path}}")
  cli::cli_text("Type: {.val {x$file_type}}")
  cli::cli_text("Dimensions: {.val {x$n_rows}} rows × {.val {x$n_cols}} columns")
  
  # SDMX Readiness Assessment
  if (!is.null(x$sdmx_readiness) && length(x$sdmx_readiness) > 0) {
    cli::cli_h2("SDMX Readiness Assessment")
    if ("overall_score" %in% names(x$sdmx_readiness)) {
      cli::cli_text("Overall Score: {.val {round(x$sdmx_readiness$overall_score * 100, 1)}}%")
    }
    if ("readiness_level" %in% names(x$sdmx_readiness)) {
      cli::cli_text("Readiness Level: {.val {x$sdmx_readiness$readiness_level}}")
    }
  }
  
  cli::cli_h2("Column Summary")
  print(x$columns)
  
  # Data Quality Summary
  if (!is.null(x$data_quality) && length(x$data_quality) > 0) {
    cli::cli_h2("Data Quality")
    if ("overall_score" %in% names(x$data_quality)) {
      cli::cli_text("Overall Quality: {.val {round(x$data_quality$overall_score * 100, 1)}}%")
    }
    if ("issues" %in% names(x$data_quality) && length(x$data_quality$issues) > 0) {
      cli::cli_text("Quality Issues: {.val {length(x$data_quality$issues)}}")
    }
  }
  
  # Temporal Patterns
  if (!is.null(x$temporal_patterns) && length(x$temporal_patterns) > 0) {
    cli::cli_h2("Temporal Patterns")
    if ("temporal_columns" %in% names(x$temporal_patterns)) {
      cli::cli_text("Temporal Columns: {.val {length(x$temporal_patterns$temporal_columns)}}")
    }
    if ("dominant_frequency" %in% names(x$temporal_patterns)) {
      cli::cli_text("Dominant Frequency: {.val {x$temporal_patterns$dominant_frequency}}")
    }
  }
  
  # Hierarchical Relationships
  if (!is.null(x$hierarchical_relationships) && length(x$hierarchical_relationships) > 0) {
    cli::cli_h2("Hierarchical Relationships")
    if ("potential_hierarchies" %in% names(x$hierarchical_relationships)) {
      n_hierarchies <- length(x$hierarchical_relationships$potential_hierarchies)
      cli::cli_text("Potential Hierarchies: {.val {n_hierarchies}}")
    }
  }
  
  if (nrow(x$summary_stats) > 0) {
    cli::cli_h2("Numeric Summary")
    print(x$summary_stats)
  }
  
  if (length(x$unique_values) > 0) {
    cli::cli_h2("Categorical Values (sample)")
    purrr::iwalk(x$unique_values, ~ {
      cli::cli_text("{.strong {.y}}: {.val {.x}}")
    })
  }
  
  invisible(x)
}

#' Detect Temporal Patterns in Data
#'
#' Enhanced temporal pattern detection that identifies date/time columns,
#' analyzes temporal frequencies, and suggests SDMX time dimensions.
#'
#' @param data Data frame to analyze
#' @param columns_info Column information from analyze_data_structure
#'
#' @return List with temporal pattern analysis
#' @keywords internal
detect_temporal_patterns_enhanced <- function(data, columns_info) {
  
  temporal_patterns <- list(
    temporal_columns = character(),
    date_formats = list(),
    dominant_frequency = "unknown",
    time_range = list(),
    regularity_score = 0.0,
    sdmx_time_suggestions = character()
  )
  
  # Identify potential temporal columns
  potential_temporal <- columns_info$column[columns_info$is_date | 
                                           grepl("date|time|year|period|month|quarter", 
                                                tolower(columns_info$column))]
  
  for (col_name in potential_temporal) {
    col_data <- data[[col_name]]
    
    # Try to parse as dates if not already
    if (!columns_info$is_date[columns_info$column == col_name]) {
      # Try common date formats
      date_formats <- c("%Y", "%Y-%m-%d", "%d/%m/%Y", "%m/%d/%Y", "%Y%m%d", "%Y-Q%q")
      
      for (fmt in date_formats) {
        tryCatch({
          parsed_dates <- as.Date(as.character(col_data), format = fmt)
          if (sum(!is.na(parsed_dates)) > length(col_data) * 0.8) {
            temporal_patterns$temporal_columns <- c(temporal_patterns$temporal_columns, col_name)
            temporal_patterns$date_formats[[col_name]] <- fmt
            
            # Analyze frequency
            if (length(unique(parsed_dates)) > 1) {
              date_diffs <- diff(sort(unique(parsed_dates)))
              if (length(date_diffs) > 0) {
                median_diff <- median(date_diffs, na.rm = TRUE)
                
                # Determine frequency
                if (median_diff <= 32) {
                  temporal_patterns$dominant_frequency <- "monthly"
                  temporal_patterns$sdmx_time_suggestions <- c(temporal_patterns$sdmx_time_suggestions, "M")
                } else if (median_diff <= 100) {
                  temporal_patterns$dominant_frequency <- "quarterly" 
                  temporal_patterns$sdmx_time_suggestions <- c(temporal_patterns$sdmx_time_suggestions, "Q")
                } else {
                  temporal_patterns$dominant_frequency <- "annual"
                  temporal_patterns$sdmx_time_suggestions <- c(temporal_patterns$sdmx_time_suggestions, "A")
                }
                
                temporal_patterns$time_range[[col_name]] <- list(
                  start = min(parsed_dates, na.rm = TRUE),
                  end = max(parsed_dates, na.rm = TRUE)
                )
              }
            }
            break
          }
        }, error = function(e) NULL)
      }
    } else {
      temporal_patterns$temporal_columns <- c(temporal_patterns$temporal_columns, col_name)
    }
  }
  
  # Calculate regularity score
  if (length(temporal_patterns$temporal_columns) > 0) {
    temporal_patterns$regularity_score <- 0.8  # Simplified scoring
  }
  
  return(temporal_patterns)
}

#' Detect Hierarchical Relationships in Data
#'
#' Enhanced hierarchical relationship detection that identifies parent-child
#' relationships between categorical columns.
#'
#' @param data Data frame to analyze
#' @param columns_info Column information from analyze_data_structure
#'
#' @return List with hierarchical relationship analysis
#' @keywords internal
detect_hierarchical_relationships_enhanced <- function(data, columns_info) {
  
  hierarchical_analysis <- list(
    potential_hierarchies = list(),
    parent_child_relationships = list(),
    aggregation_levels = list(),
    hierarchy_score = 0.0
  )
  
  categorical_cols <- columns_info$column[columns_info$is_categorical]
  
  if (length(categorical_cols) < 2) {
    return(hierarchical_analysis)
  }
  
  # Analyze each pair of categorical columns
  for (i in seq_along(categorical_cols)) {
    for (j in seq_along(categorical_cols)) {
      if (i >= j) next
      
      col1 <- categorical_cols[i]
      col2 <- categorical_cols[j]
      
      col1_info <- columns_info[columns_info$column == col1, ]
      col2_info <- columns_info[columns_info$column == col2, ]
      
      # Check if one could be parent of another based on cardinality
      if (col1_info$unique_count < col2_info$unique_count && 
          col1_info$unique_count > 1) {
        
        # Check naming patterns
        name1_lower <- tolower(col1)
        name2_lower <- tolower(col2)
        
        # Geographic hierarchies
        is_geographic <- (grepl("country|region|area|state|province", name1_lower) && 
                         grepl("city|district|locality|detailed", name2_lower)) ||
                        (grepl("broad|general|main", name1_lower) && 
                         grepl("detailed|specific|sub", name2_lower))
        
        # Product/classification hierarchies
        is_classification <- (grepl("category|class|group|sector", name1_lower) && 
                             grepl("product|item|code|detail", name2_lower))
        
        if (is_geographic || is_classification) {
          # Verify relationship by checking if child values are nested under parents
          relationship_strength <- check_hierarchical_nesting(data[[col1]], data[[col2]])
          
          if (relationship_strength > 0.7) {
            hierarchical_analysis$parent_child_relationships[[length(hierarchical_analysis$parent_child_relationships) + 1]] <- 
              list(parent = col1, child = col2, strength = relationship_strength)
            
            if (!col1 %in% names(hierarchical_analysis$potential_hierarchies)) {
              hierarchical_analysis$potential_hierarchies[[col1]] <- character()
            }
            hierarchical_analysis$potential_hierarchies[[col1]] <- c(
              hierarchical_analysis$potential_hierarchies[[col1]], col2
            )
          }
        }
      }
    }
  }
  
  # Calculate hierarchy score
  if (length(hierarchical_analysis$parent_child_relationships) > 0) {
    hierarchical_analysis$hierarchy_score <- min(1.0, length(hierarchical_analysis$parent_child_relationships) * 0.3)
  }
  
  return(hierarchical_analysis)
}

#' Check Hierarchical Nesting
#' @keywords internal
check_hierarchical_nesting <- function(parent_col, child_col) {
  
  # Remove NAs
  valid_rows <- !is.na(parent_col) & !is.na(child_col)
  parent_clean <- parent_col[valid_rows]
  child_clean <- child_col[valid_rows]
  
  if (length(parent_clean) == 0) return(0.0)
  
  # For each parent value, check if child values are consistent
  parent_child_map <- split(child_clean, parent_clean)
  
  # Calculate consistency score
  consistency_scores <- sapply(parent_child_map, function(children) {
    # If a parent has only one type of child, it's perfectly hierarchical
    # If a parent has many different children, it might still be hierarchical
    unique_children <- length(unique(children))
    total_children <- length(children)
    
    # Score based on how "nested" the relationship is
    if (unique_children == 1) {
      return(1.0)  # Perfect nesting
    } else if (unique_children <= total_children * 0.1) {
      return(0.8)  # Good nesting
    } else if (unique_children <= total_children * 0.3) {
      return(0.6)  # Moderate nesting
    } else {
      return(0.2)  # Weak nesting
    }
  })
  
  return(mean(consistency_scores))
}

#' Assess Data Quality
#'
#' Comprehensive data quality assessment including completeness,
#' consistency, validity, and SDMX-specific quality metrics.
#'
#' @param data Data frame to analyze
#' @param columns_info Column information from analyze_data_structure
#'
#' @return List with data quality assessment
#' @keywords internal
assess_data_quality_enhanced <- function(data, columns_info) {
  
  quality_assessment <- list(
    overall_score = 0.0,
    completeness_score = 0.0,
    consistency_score = 0.0,
    validity_score = 0.0,
    issues = list(),
    recommendations = character()
  )
  
  # Completeness Assessment
  completeness_scores <- 1 - columns_info$na_proportion
  quality_assessment$completeness_score <- mean(completeness_scores)
  
  if (quality_assessment$completeness_score < 0.9) {
    high_missing_cols <- columns_info$column[columns_info$na_proportion > 0.1]
    if (length(high_missing_cols) > 0) {
      quality_assessment$issues[["high_missing"]] <- list(
        type = "completeness",
        severity = "medium",
        columns = high_missing_cols,
        description = "Columns with >10% missing values"
      )
      quality_assessment$recommendations <- c(
        quality_assessment$recommendations,
        "Consider imputation or exclusion of high-missing columns"
      )
    }
  }
  
  # Consistency Assessment
  consistency_issues <- list()
  
  # Check for inconsistent categorical values
  categorical_cols <- columns_info$column[columns_info$is_categorical]
  for (col_name in categorical_cols) {
    col_data <- data[[col_name]]
    
    # Check for potential duplicates with different casing/spacing
    unique_vals <- unique(trimws(toupper(as.character(col_data[!is.na(col_data)]))))
    original_vals <- unique(trimws(as.character(col_data[!is.na(col_data)])))
    
    if (length(unique_vals) < length(original_vals)) {
      consistency_issues[[col_name]] <- "inconsistent_categorical_values"
    }
    
    # Check for very long categorical values (might be free text)
    if (any(nchar(as.character(col_data)) > 100, na.rm = TRUE)) {
      consistency_issues[[col_name]] <- c(consistency_issues[[col_name]], "very_long_values")
    }
  }
  
  quality_assessment$consistency_score <- max(0.0, 1.0 - length(consistency_issues) * 0.1)
  
  if (length(consistency_issues) > 0) {
    quality_assessment$issues[["consistency"]] <- list(
      type = "consistency",
      severity = "medium", 
      columns = names(consistency_issues),
      description = "Inconsistent categorical values or very long text"
    )
    quality_assessment$recommendations <- c(
      quality_assessment$recommendations,
      "Standardize categorical values and review long text fields"
    )
  }
  
  # Validity Assessment
  validity_score <- 1.0
  
  # Check numeric ranges
  numeric_cols <- columns_info$column[columns_info$is_numeric]
  for (col_name in numeric_cols) {
    col_data <- data[[col_name]]
    
    # Check for suspicious negative values in typically positive measures
    if (grepl("count|amount|quantity|value|rate|percent", tolower(col_name))) {
      if (any(col_data < 0, na.rm = TRUE)) {
        validity_score <- validity_score - 0.1
        quality_assessment$issues[["negative_values"]] <- list(
          type = "validity",
          severity = "low",
          columns = col_name,
          description = "Negative values in typically positive measures"
        )
      }
    }
  }
  
  quality_assessment$validity_score <- max(0.0, validity_score)
  
  # Overall Score Calculation
  quality_assessment$overall_score <- (
    0.4 * quality_assessment$completeness_score +
    0.3 * quality_assessment$consistency_score +
    0.3 * quality_assessment$validity_score
  )
  
  return(quality_assessment)
}

#' Assess SDMX Mapping Readiness
#'
#' Assesses how ready the data is for SDMX mapping based on structure,
#' quality, and SDMX-specific requirements.
#'
#' @param columns_info Column information from analyze_data_structure
#' @param temporal_patterns Temporal pattern analysis results
#' @param data_quality Data quality assessment results
#'
#' @return List with SDMX readiness assessment
#' @keywords internal
assess_sdmx_readiness <- function(columns_info, temporal_patterns, data_quality) {
  
  readiness_assessment <- list(
    overall_score = 0.0,
    readiness_level = "unknown",
    dimension_potential = 0.0,
    measure_potential = 0.0,
    temporal_readiness = 0.0,
    recommendations = character()
  )
  
  # Dimension Potential (categorical columns)
  categorical_cols <- sum(columns_info$is_categorical)
  total_cols <- nrow(columns_info)
  readiness_assessment$dimension_potential <- min(1.0, categorical_cols / max(3, total_cols * 0.3))
  
  # Measure Potential (numeric columns)
  numeric_cols <- sum(columns_info$is_numeric)
  readiness_assessment$measure_potential <- min(1.0, numeric_cols / max(1, total_cols * 0.2))
  
  # Temporal Readiness
  if (!is.null(temporal_patterns) && length(temporal_patterns$temporal_columns) > 0) {
    readiness_assessment$temporal_readiness <- 0.9
  } else {
    # Check for columns that might be temporal
    potential_temporal <- sum(grepl("date|time|year|period|month|quarter", 
                                   tolower(columns_info$column)))
    readiness_assessment$temporal_readiness <- min(0.7, potential_temporal * 0.5)
  }
  
  # Data Quality Impact
  quality_factor <- if (!is.null(data_quality) && "overall_score" %in% names(data_quality)) {
    data_quality$overall_score
  } else {
    0.8  # Default moderate quality
  }
  
  # Overall SDMX Readiness Score
  readiness_assessment$overall_score <- (
    0.3 * readiness_assessment$dimension_potential +
    0.2 * readiness_assessment$measure_potential +
    0.2 * readiness_assessment$temporal_readiness +
    0.3 * quality_factor
  )
  
  # Readiness Level Classification
  readiness_assessment$readiness_level <- if (readiness_assessment$overall_score >= 0.8) {
    "high"
  } else if (readiness_assessment$overall_score >= 0.6) {
    "medium"
  } else if (readiness_assessment$overall_score >= 0.4) {
    "low"
  } else {
    "very_low"
  }
  
  # Generate Recommendations
  if (readiness_assessment$dimension_potential < 0.5) {
    readiness_assessment$recommendations <- c(
      readiness_assessment$recommendations,
      "Consider creating more categorical dimensions or reviewing data structure"
    )
  }
  
  if (readiness_assessment$measure_potential < 0.3) {
    readiness_assessment$recommendations <- c(
      readiness_assessment$recommendations,
      "Identify numeric measures or create calculated fields"
    )
  }
  
  if (readiness_assessment$temporal_readiness < 0.5) {
    readiness_assessment$recommendations <- c(
      readiness_assessment$recommendations,
      "Add or improve temporal dimension for SDMX compliance"
    )
  }
  
  if (quality_factor < 0.7) {
    readiness_assessment$recommendations <- c(
      readiness_assessment$recommendations,
      "Improve data quality before SDMX transformation"
    )
  }
  
  return(readiness_assessment)
}