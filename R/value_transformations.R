#' Value Transformation Suggestion System
#'
#' Intelligent system for suggesting and generating data value transformations
#' based on mapping analysis, SDMX requirements, and data patterns.
#'
#' @name value_transformations
NULL

#' Generate Value Transformation Suggestions
#'
#' Analyzes mapping results and generates specific transformation suggestions
#' for converting source data values to match SDMX target requirements.
#'
#' @param mapping_result Advanced mapping result from `infer_advanced_mappings()`
#' @param source_data Source data frame
#' @param target_sdmx Target SDMX metadata from `extract_dsd_metadata()`
#' @param llm_config Optional LLM configuration for enhanced suggestions
#'
#' @return List containing transformation suggestions and generated code
#' @export
#' @examples
#' \dontrun{
#' # Generate transformation suggestions
#' transformations <- generate_value_transformations(
#'   mapping_result = mapping_result,
#'   source_data = source_data,
#'   target_sdmx = target_sdmx
#' )
#' 
#' # View suggested transformations
#' print(transformations$suggestions)
#' 
#' # Get generated R code
#' cat(transformations$transformation_code)
#' }
generate_value_transformations <- function(mapping_result,
                                         source_data,
                                         target_sdmx,
                                         llm_config = NULL) {
  
  if (!inherits(mapping_result, "llmx_advanced_mapping_result")) {
    cli::cli_abort("mapping_result must be from {.fn infer_advanced_mappings}")
  }
  
  if (!inherits(target_sdmx, "llmx_sdmx_metadata")) {
    cli::cli_abort("target_sdmx must be from {.fn extract_dsd_metadata}")
  }
  
  cli::cli_inform("Analyzing value transformation requirements...")
  
  transformation_suggestions <- list(
    direct_mappings = list(),
    value_recodings = list(),
    format_conversions = list(),
    data_cleaning = list(),
    aggregations = list(),
    derived_calculations = list(),
    validation_rules = list()
  )
  
  transformation_code <- character()
  
  # Process each mapping for transformation suggestions
  for (mapping in mapping_result$mappings) {
    source_col <- mapping$source_column
    target_col <- mapping$target_column
    source_values <- source_data[[source_col]]
    
    # Get target column information
    target_info <- get_target_column_info(target_col, target_sdmx)
    
    if (is.null(target_info)) next
    
    # Generate transformation suggestions based on mapping type and target requirements
    col_transformations <- analyze_column_transformation(
      source_col, target_col, source_values, target_info, target_sdmx, llm_config
    )
    
    # Categorize transformations
    if (!is.null(col_transformations$direct_mapping)) {
      transformation_suggestions$direct_mappings[[source_col]] <- col_transformations$direct_mapping
    }
    
    if (!is.null(col_transformations$value_recoding)) {
      transformation_suggestions$value_recodings[[source_col]] <- col_transformations$value_recoding
    }
    
    if (!is.null(col_transformations$format_conversion)) {
      transformation_suggestions$format_conversions[[source_col]] <- col_transformations$format_conversion
    }
    
    if (!is.null(col_transformations$data_cleaning)) {
      transformation_suggestions$data_cleaning[[source_col]] <- col_transformations$data_cleaning
    }
    
    if (!is.null(col_transformations$validation_rule)) {
      transformation_suggestions$validation_rules[[source_col]] <- col_transformations$validation_rule
    }
    
    # Accumulate transformation code
    if (!is.null(col_transformations$code)) {
      transformation_code <- c(transformation_code, col_transformations$code)
    }
  }
  
  # Generate derived calculations and aggregations
  derived_transformations <- suggest_derived_transformations(
    mapping_result, source_data, target_sdmx
  )
  
  transformation_suggestions$derived_calculations <- derived_transformations$calculations
  transformation_suggestions$aggregations <- derived_transformations$aggregations
  
  if (length(derived_transformations$code) > 0) {
    transformation_code <- c(transformation_code, derived_transformations$code)
  }
  
  # Generate comprehensive transformation script
  complete_script <- create_transformation_script(
    transformation_suggestions, transformation_code, source_data, target_sdmx
  )
  
  # Use LLM for enhanced suggestions if available
  if (!is.null(llm_config)) {
    enhanced_suggestions <- generate_llm_transformation_suggestions(
      transformation_suggestions, source_data, target_sdmx, llm_config
    )
    transformation_suggestions$llm_enhanced <- enhanced_suggestions
  }
  
  structure(
    list(
      suggestions = transformation_suggestions,
      transformation_code = transformation_code,
      complete_script = complete_script,
      summary = summarize_transformations(transformation_suggestions)
    ),
    class = "llmx_value_transformations"
  )
}

#' Analyze Column Transformation
#' @keywords internal
analyze_column_transformation <- function(source_col, target_col, source_values, target_info, target_sdmx, llm_config) {
  
  transformations <- list()
  
  # Remove NA values for analysis
  clean_values <- source_values[!is.na(source_values)]
  if (length(clean_values) == 0) {
    transformations$data_cleaning <- list(
      type = "missing_data_handling",
      description = "Column contains only missing values",
      suggested_action = "impute_or_exclude"
    )
    return(transformations)
  }
  
  # Direct mapping (no transformation needed)
  if (target_info$type == "measure" && is.numeric(clean_values)) {
    transformations$direct_mapping <- list(
      type = "direct_assignment",
      description = "Direct numeric mapping",
      code = glue::glue("{target_col} = {source_col}")
    )
    transformations$code <- glue::glue("  {target_col} = {source_col}")
    return(transformations)
  }
  
  # Value recoding for categorical/dimension columns
  if (target_info$type %in% c("dimension", "attribute") && !is.na(target_info$codelist_id)) {
    recoding_analysis <- analyze_categorical_recoding(clean_values, target_info, target_sdmx)
    
    if (!is.null(recoding_analysis)) {
      transformations$value_recoding <- recoding_analysis
      transformations$code <- generate_recoding_code(source_col, target_col, recoding_analysis)
    }
  }
  
  # Format conversions
  format_conversion <- detect_format_conversion_needs(clean_values, target_info)
  if (!is.null(format_conversion)) {
    transformations$format_conversion <- format_conversion
    transformations$code <- c(transformations$code, format_conversion$code)
  }
  
  # Data cleaning suggestions
  cleaning_needs <- detect_cleaning_needs(clean_values, target_info)
  if (!is.null(cleaning_needs)) {
    transformations$data_cleaning <- cleaning_needs
    transformations$code <- c(transformations$code, cleaning_needs$code)
  }
  
  # Validation rules
  validation_rule <- generate_validation_rule(clean_values, target_info)
  if (!is.null(validation_rule)) {
    transformations$validation_rule <- validation_rule
  }
  
  return(transformations)
}

#' Analyze Categorical Recoding
#' @keywords internal
analyze_categorical_recoding <- function(source_values, target_info, target_sdmx) {
  
  # Get unique source values
  unique_source <- unique(as.character(source_values))
  
  # For now, create a simplified mock codelist
  # In a full implementation, this would load actual SDMX codelists
  mock_codelist <- create_mock_codelist(target_info$codelist_id)
  
  if (is.null(mock_codelist)) {
    return(list(
      type = "manual_mapping_required",
      description = "Codelist not available - manual mapping required",
      unique_values = unique_source,
      suggested_action = "create_mapping_table"
    ))
  }
  
  # Analyze value patterns against codelist
  value_analysis <- analyze_value_patterns(source_values, mock_codelist)
  
  if (value_analysis$confidence_score > 0.5) {
    return(list(
      type = "automatic_recoding",
      description = "Automatic value recoding based on pattern matching",
      confidence_score = value_analysis$confidence_score,
      match_details = value_analysis$match_details,
      suggested_transformations = value_analysis$suggested_transformations
    ))
  } else {
    return(list(
      type = "assisted_mapping",
      description = "Assisted mapping with manual review required",
      confidence_score = value_analysis$confidence_score,
      unique_values = unique_source,
      suggested_matches = value_analysis$match_details,
      suggested_action = "review_and_confirm_mappings"
    ))
  }
}

#' Create Mock Codelist
#' @keywords internal
create_mock_codelist <- function(codelist_id) {
  
  # Create mock codelists for common SDMX dimensions
  if (grepl("geo|country|area", tolower(codelist_id))) {
    return(data.frame(
      code_id = c("USA", "CAN", "MEX", "GBR", "FRA", "DEU", "JPN", "AUS"),
      name = c("United States", "Canada", "Mexico", "United Kingdom", 
               "France", "Germany", "Japan", "Australia"),
      stringsAsFactors = FALSE
    ))
  } else if (grepl("time|period|freq", tolower(codelist_id))) {
    return(data.frame(
      code_id = c("A", "Q", "M", "D"),
      name = c("Annual", "Quarterly", "Monthly", "Daily"),
      stringsAsFactors = FALSE
    ))
  } else if (grepl("sex|gender", tolower(codelist_id))) {
    return(data.frame(
      code_id = c("M", "F", "T"),
      name = c("Male", "Female", "Total"),
      stringsAsFactors = FALSE
    ))
  }
  
  return(NULL)
}

#' Detect Format Conversion Needs
#' @keywords internal
detect_format_conversion_needs <- function(source_values, target_info) {
  
  # Date/time format conversions
  if (target_info$type == "time_dimension") {
    if (is.character(source_values)) {
      # Check if values look like dates
      date_patterns <- c(
        "\\d{4}-\\d{2}-\\d{2}",  # YYYY-MM-DD
        "\\d{2}/\\d{2}/\\d{4}",  # MM/DD/YYYY
        "\\d{4}",                # YYYY
        "\\d{4}Q[1-4]"          # YYYYQ1
      )
      
      for (i in seq_along(date_patterns)) {
        pattern <- date_patterns[i]
        if (sum(grepl(pattern, source_values)) > length(source_values) * 0.8) {
          return(list(
            type = "date_format_conversion",
            description = "Convert date strings to SDMX time format",
            detected_pattern = pattern,
            code = generate_date_conversion_code(pattern)
          ))
        }
      }
    }
  }
  
  # Numeric format conversions
  if (target_info$type == "measure" && is.character(source_values)) {
    # Check if character values can be converted to numeric
    numeric_candidates <- suppressWarnings(as.numeric(source_values))
    if (sum(!is.na(numeric_candidates)) > length(source_values) * 0.8) {
      return(list(
        type = "numeric_conversion",
        description = "Convert character values to numeric",
        code = "as.numeric(gsub('[^0-9.-]', '', value))"
      ))
    }
  }
  
  return(NULL)
}

#' Detect Cleaning Needs
#' @keywords internal
detect_cleaning_needs <- function(source_values, target_info) {
  
  cleaning_needs <- list()
  code_snippets <- character()
  
  if (is.character(source_values)) {
    # Check for leading/trailing whitespace
    if (any(source_values != trimws(source_values), na.rm = TRUE)) {
      cleaning_needs$whitespace <- "Remove leading/trailing whitespace"
      code_snippets <- c(code_snippets, "trimws(value)")
    }
    
    # Check for inconsistent casing
    if (length(unique(toupper(source_values))) < length(unique(source_values))) {
      cleaning_needs$casing <- "Standardize text casing"
      code_snippets <- c(code_snippets, "toupper(value)")
    }
    
    # Check for special characters
    if (any(grepl("[^a-zA-Z0-9\\s-_]", source_values), na.rm = TRUE)) {
      cleaning_needs$special_chars <- "Remove or standardize special characters"
      code_snippets <- c(code_snippets, "gsub('[^a-zA-Z0-9\\s-_]', '', value)")
    }
  }
  
  if (length(cleaning_needs) > 0) {
    return(list(
      type = "data_cleaning",
      description = "Multiple cleaning operations needed",
      issues = cleaning_needs,
      code = paste(code_snippets, collapse = " |> ")
    ))
  }
  
  return(NULL)
}

#' Generate Validation Rule
#' @keywords internal
generate_validation_rule <- function(source_values, target_info) {
  
  validation_rules <- list()
  
  # Numeric range validation
  if (target_info$type == "measure" && is.numeric(source_values)) {
    min_val <- min(source_values, na.rm = TRUE)
    max_val <- max(source_values, na.rm = TRUE)
    
    validation_rules$range_check <- list(
      description = "Validate numeric range",
      min_value = min_val,
      max_value = max_val,
      rule = glue::glue("value >= {min_val} & value <= {max_val}")
    )
  }
  
  # Categorical value validation
  if (target_info$type %in% c("dimension", "attribute")) {
    unique_vals <- unique(source_values[!is.na(source_values)])
    if (length(unique_vals) < 50) {  # Only for manageable number of categories
      validation_rules$allowed_values <- list(
        description = "Validate allowed categorical values",
        allowed_values = unique_vals,
        rule = glue::glue("value %in% c({paste(shQuote(unique_vals), collapse = ', ')})")
      )
    }
  }
  
  if (length(validation_rules) > 0) {
    return(validation_rules)
  }
  
  return(NULL)
}

#' Generate Recoding Code
#' @keywords internal
generate_recoding_code <- function(source_col, target_col, recoding_analysis) {
  
  if (recoding_analysis$type == "automatic_recoding" && !is.null(recoding_analysis$match_details)) {
    # Generate case_when or recode statement
    match_details <- recoding_analysis$match_details
    
    if (length(match_details) > 0) {
      recode_pairs <- sapply(names(match_details), function(source_val) {
        target_val <- match_details[[source_val]]
        glue::glue('"{source_val}" ~ "{target_val}"')
      })
      
      code <- glue::glue('  {target_col} = case_when(\n    {paste(recode_pairs, collapse = ",\n    ")},\n    TRUE ~ NA_character_\n  )')
      return(code)
    }
  }
  
  # Fallback to identity mapping with cleaning
  return(glue::glue("  {target_col} = trimws(as.character({source_col}))"))
}

#' Generate Date Conversion Code
#' @keywords internal
generate_date_conversion_code <- function(pattern) {
  
  if (pattern == "\\d{4}-\\d{2}-\\d{2}") {
    return("as.Date(value, format = '%Y-%m-%d')")
  } else if (pattern == "\\d{2}/\\d{2}/\\d{4}") {
    return("as.Date(value, format = '%m/%d/%Y')")
  } else if (pattern == "\\d{4}") {
    return("as.Date(paste0(value, '-01-01'))")
  } else if (pattern == "\\d{4}Q[1-4]") {
    return("convert_quarter_to_date(value)")
  }
  
  return("as.Date(value)")
}

#' Suggest Derived Transformations
#' @keywords internal
suggest_derived_transformations <- function(mapping_result, source_data, target_sdmx) {
  
  derived_transformations <- list(
    calculations = list(),
    aggregations = list(),
    code = character()
  )
  
  # Check for missing required SDMX columns that could be derived
  mapped_targets <- sapply(mapping_result$mappings, function(m) m$target_column)
  
  # Look for common derived calculations
  if ("OBS_VALUE" %in% names(target_sdmx$measures) && !"OBS_VALUE" %in% mapped_targets) {
    # Try to identify numeric columns that could be the observation value
    numeric_cols <- names(source_data)[sapply(source_data, is.numeric)]
    unmapped_numeric <- setdiff(numeric_cols, sapply(mapping_result$mappings, function(m) m$source_column))
    
    if (length(unmapped_numeric) > 0) {
      primary_measure <- unmapped_numeric[1]  # Take first available numeric column
      derived_transformations$calculations[["OBS_VALUE"]] <- list(
        type = "direct_assignment",
        description = glue::glue("Use {primary_measure} as observation value"),
        source_column = primary_measure,
        formula = primary_measure
      )
      derived_transformations$code <- c(
        derived_transformations$code,
        glue::glue("  OBS_VALUE = {primary_measure}")
      )
    }
  }
  
  # Standard SDMX attributes that can be derived
  standard_attributes <- c("OBS_STATUS", "UNIT_MEASURE", "DECIMALS")
  
  for (attr in standard_attributes) {
    if (attr %in% names(target_sdmx$attributes) && !attr %in% mapped_targets) {
      if (attr == "OBS_STATUS") {
        derived_transformations$calculations[[attr]] <- list(
          type = "constant_value",
          description = "Set default observation status",
          formula = "'A'",  # Normal value
          explanation = "A = Normal value (SDMX standard)"
        )
        derived_transformations$code <- c(
          derived_transformations$code,
          "  OBS_STATUS = 'A'"
        )
      } else if (attr == "DECIMALS") {
        derived_transformations$calculations[[attr]] <- list(
          type = "calculated_value",
          description = "Calculate decimal places from data",
          formula = "detect_decimals(OBS_VALUE)",
          explanation = "Automatically detect decimal places in values"
        )
        derived_transformations$code <- c(
          derived_transformations$code,
          "  DECIMALS = detect_decimals(OBS_VALUE)"
        )
      }
    }
  }
  
  return(derived_transformations)
}

#' Create Transformation Script
#' @keywords internal
create_transformation_script <- function(transformation_suggestions, transformation_code, source_data, target_sdmx) {
  
  script_parts <- c(
    "# SDMX Value Transformation Script",
    "# Generated by llmx package",
    "",
    "transform_to_sdmx_csv <- function(source_data) {",
    "  ",
    "  # Input validation",
    "  if (!is.data.frame(source_data)) {",
    "    stop('Input must be a data frame')",
    "  }",
    "  ",
    "  # Data transformations",
    "  transformed_data <- source_data |>",
    "    mutate("
  )
  
  # Add transformation code
  if (length(transformation_code) > 0) {
    script_parts <- c(script_parts, paste("   ", transformation_code, collapse = ",\n"))
  } else {
    script_parts <- c(script_parts, "      # No transformations needed")
  }
  
  script_parts <- c(
    script_parts,
    "    )",
    "  ",
    "  # Data validation",
    generate_validation_code(transformation_suggestions$validation_rules),
    "  ",
    "  return(transformed_data)",
    "}"
  )
  
  return(paste(script_parts, collapse = "\n"))
}

#' Generate Validation Code
#' @keywords internal
generate_validation_code <- function(validation_rules) {
  
  if (length(validation_rules) == 0) {
    return("  # No validation rules defined")
  }
  
  validation_code <- c("  # Validation checks")
  
  for (col_name in names(validation_rules)) {
    rules <- validation_rules[[col_name]]
    
    if ("range_check" %in% names(rules)) {
      rule <- rules$range_check$rule
      validation_code <- c(
        validation_code,
        glue::glue("  if (any(!({rule}), na.rm = TRUE)) {{"),
        glue::glue("    warning('Values in {col_name} outside expected range')"),
        "  }"
      )
    }
    
    if ("allowed_values" %in% names(rules)) {
      rule <- rules$allowed_values$rule
      validation_code <- c(
        validation_code,
        glue::glue("  if (any(!({rule}), na.rm = TRUE)) {{"),
        glue::glue("    warning('Invalid values found in {col_name}')"),
        "  }"
      )
    }
  }
  
  return(paste(validation_code, collapse = "\n"))
}

#' Generate LLM Transformation Suggestions
#' @keywords internal
generate_llm_transformation_suggestions <- function(transformation_suggestions, source_data, target_sdmx, llm_config) {
  
  # Create context for LLM
  context <- create_transformation_context(transformation_suggestions, source_data, target_sdmx)
  
  system_prompt <- "You are an expert in SDMX data transformation. Analyze the current transformation suggestions and provide additional insights, optimizations, or alternative approaches."
  
  user_prompt <- glue::glue("
  Given the following transformation analysis:
  
  {context}
  
  Please provide:
  1. Additional transformation suggestions not covered
  2. Potential optimizations or improvements
  3. Alternative approaches for complex mappings
  4. SDMX best practices recommendations
  
  Focus on practical, implementable suggestions.
  ")
  
  tryCatch({
    response <- query_llm(llm_config, system_prompt, user_prompt)
    
    return(list(
      llm_response = response,
      generated_at = Sys.time(),
      model_used = llm_config$model
    ))
    
  }, error = function(e) {
    cli::cli_warn("LLM enhancement failed: {e$message}")
    return(NULL)
  })
}

#' Create Transformation Context
#' @keywords internal
create_transformation_context <- function(transformation_suggestions, source_data, target_sdmx) {
  
  context_parts <- c(
    "TRANSFORMATION ANALYSIS SUMMARY:",
    "",
    glue::glue("Source data: {ncol(source_data)} columns, {nrow(source_data)} rows"),
    glue::glue("Target SDMX: {nrow(target_sdmx$dimensions)} dimensions, {nrow(target_sdmx$measures)} measures"),
    "",
    "Direct mappings:", length(transformation_suggestions$direct_mappings),
    "Value recodings needed:", length(transformation_suggestions$value_recodings),
    "Format conversions needed:", length(transformation_suggestions$format_conversions),
    "Data cleaning requirements:", length(transformation_suggestions$data_cleaning),
    ""
  )
  
  return(paste(context_parts, collapse = "\n"))
}

#' Summarize Transformations
#' @keywords internal
summarize_transformations <- function(transformation_suggestions) {
  
  summary <- list(
    total_transformations = sum(
      length(transformation_suggestions$direct_mappings),
      length(transformation_suggestions$value_recodings),
      length(transformation_suggestions$format_conversions),
      length(transformation_suggestions$data_cleaning),
      length(transformation_suggestions$aggregations),
      length(transformation_suggestions$derived_calculations)
    ),
    complexity_score = calculate_transformation_complexity_score(transformation_suggestions),
    categories = list(
      direct_mappings = length(transformation_suggestions$direct_mappings),
      value_recodings = length(transformation_suggestions$value_recodings),
      format_conversions = length(transformation_suggestions$format_conversions),
      data_cleaning = length(transformation_suggestions$data_cleaning),
      derived_calculations = length(transformation_suggestions$derived_calculations),
      aggregations = length(transformation_suggestions$aggregations)
    )
  )
  
  return(summary)
}

#' Calculate Transformation Complexity Score
#' @keywords internal
calculate_transformation_complexity_score <- function(transformation_suggestions) {
  
  # Weight different types of transformations by complexity
  weights <- list(
    direct_mappings = 0.1,
    value_recodings = 0.4,
    format_conversions = 0.3,
    data_cleaning = 0.2,
    derived_calculations = 0.3,
    aggregations = 0.5
  )
  
  complexity <- 0
  for (category in names(weights)) {
    if (category %in% names(transformation_suggestions)) {
      count <- length(transformation_suggestions[[category]])
      complexity <- complexity + count * weights[[category]]
    }
  }
  
  # Normalize to 0-1 scale
  return(min(1.0, complexity / 10))
}

#' Get Enhanced Target Column Information
#' @keywords internal
get_target_column_info <- function(target_col, target_sdmx) {
  
  # Check dimensions
  if (nrow(target_sdmx$dimensions) > 0) {
    dim_matches <- target_sdmx$dimensions[target_sdmx$dimensions$id == target_col, ]
    if (nrow(dim_matches) > 0) {
      return(list(
        type = "dimension", 
        codelist_id = if ("codelist" %in% names(dim_matches)) dim_matches$codelist[1] else NA,
        data_type = if ("data_type" %in% names(dim_matches)) dim_matches$data_type[1] else "string"
      ))
    }
  }
  
  # Check measures
  if (nrow(target_sdmx$measures) > 0) {
    measure_matches <- target_sdmx$measures[target_sdmx$measures$id == target_col, ]
    if (nrow(measure_matches) > 0) {
      return(list(
        type = "measure", 
        codelist_id = NA,
        data_type = if ("data_type" %in% names(measure_matches)) measure_matches$data_type[1] else "numeric"
      ))
    }
  }
  
  # Check attributes
  if (nrow(target_sdmx$attributes) > 0) {
    attr_matches <- target_sdmx$attributes[target_sdmx$attributes$id == target_col, ]
    if (nrow(attr_matches) > 0) {
      return(list(
        type = "attribute", 
        codelist_id = if ("codelist" %in% names(attr_matches)) attr_matches$codelist[1] else NA,
        data_type = if ("data_type" %in% names(attr_matches)) attr_matches$data_type[1] else "string"
      ))
    }
  }
  
  # Check if it's a time dimension
  if (grepl("time|period|date", tolower(target_col))) {
    return(list(type = "time_dimension", codelist_id = NA, data_type = "date"))
  }
  
  return(NULL)
}

#' Print method for value transformations
#' @param x An object of class "llmx_value_transformations"
#' @param ... Additional arguments (unused)
#' @export
print.llmx_value_transformations <- function(x, ...) {
  cli::cli_h1("Value Transformation Analysis")
  
  summary <- x$summary
  cli::cli_text("Total Transformations: {.val {summary$total_transformations}}")
  cli::cli_text("Complexity Score: {.val {round(summary$complexity_score * 100, 1)}}%")
  
  cli::cli_h2("Transformation Categories")
  for (category in names(summary$categories)) {
    count <- summary$categories[[category]]
    if (count > 0) {
      cli::cli_text("{.strong {gsub('_', ' ', tools::toTitleCase(category))}}: {.val {count}}")
    }
  }
  
  if (length(x$suggestions$value_recodings) > 0) {
    cli::cli_h2("Value Recodings Required")
    for (col in names(x$suggestions$value_recodings)) {
      recoding <- x$suggestions$value_recodings[[col]]
      cli::cli_text("{.strong {col}}: {recoding$description}")
    }
  }
  
  if (length(x$suggestions$data_cleaning) > 0) {
    cli::cli_h2("Data Cleaning Required")
    for (col in names(x$suggestions$data_cleaning)) {
      cleaning <- x$suggestions$data_cleaning[[col]]
      cli::cli_text("{.strong {col}}: {cleaning$description}")
    }
  }
  
  if (!is.null(x$suggestions$llm_enhanced)) {
    cli::cli_h2("LLM Enhanced Suggestions Available")
    cli::cli_text("Model used: {.val {x$suggestions$llm_enhanced$model_used}}")
  }
  
  invisible(x)
}