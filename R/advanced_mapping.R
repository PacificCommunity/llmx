#' Advanced Mapping Inference Engine
#'
#' Intelligent mapping between source data and SDMX schemas using:
#' - Fuzzy string matching with confidence scoring
#' - Value pattern analysis against SDMX codelists
#' - Statistical analysis of data patterns
#' - Hierarchical mapping inference
#' - User feedback incorporation for continuous improvement
#'
#' @name advanced_mapping
NULL

#' Mapping Confidence Levels
#'
#' Confidence levels for mapping suggestions.
#' @export
MAPPING_CONFIDENCE <- list(
  VERY_LOW = 1L,    # 0-20% confidence
  LOW = 2L,         # 20-40% confidence  
  MEDIUM = 3L,      # 40-60% confidence
  HIGH = 4L,        # 60-80% confidence
  VERY_HIGH = 5L    # 80-100% confidence
)

#' Create Advanced Mapping Inference Engine
#'
#' Creates a new inference engine with configurable parameters for
#' intelligent column mapping between source data and SDMX schemas.
#'
#' @param fuzzy_threshold Numeric. Minimum fuzzy matching score (0-1, default: 0.6)
#' @param min_confidence Numeric. Minimum confidence for mapping suggestions (0-1, default: 0.3)
#' @param use_statistical_analysis Logical. Enable statistical compatibility analysis (default: TRUE)
#' @param enable_learning Logical. Enable learning from user feedback (default: TRUE)
#'
#' @return An inference engine object
#' @export
#' @examples
#' \dontrun{
#' engine <- create_inference_engine(fuzzy_threshold = 0.7, min_confidence = 0.4)
#' }
create_inference_engine <- function(fuzzy_threshold = 0.6,
                                   min_confidence = 0.3,
                                   use_statistical_analysis = TRUE,
                                   enable_learning = TRUE) {
  
  structure(
    list(
      # Configuration
      fuzzy_threshold = fuzzy_threshold,
      min_confidence = min_confidence,
      use_statistical_analysis = use_statistical_analysis,
      enable_learning = enable_learning,
      
      # Data
      source_profile = NULL,
      target_schema = NULL,
      codelists_data = list(),
      
      # Learning components
      successful_mappings = list(),
      failed_mappings = list(),
      user_feedback = list()
    ),
    class = "llmx_inference_engine"
  )
}

#' Calculate Fuzzy Matching Score
#'
#' Calculates fuzzy matching score between two strings using multiple algorithms
#' including Jaro-Winkler similarity, substring matching, and semantic patterns.
#'
#' @param str1 Character. First string
#' @param str2 Character. Second string
#'
#' @return Numeric score between 0 and 1
#' @export
#' @examples
#' fuzzy_match_score("country", "geo")
#' fuzzy_match_score("GDP_Annual", "gdp_yearly")
fuzzy_match_score <- function(str1, str2) {
  
  # Normalize strings
  s1 <- tolower(trimws(str1))
  s2 <- tolower(trimws(str2))
  
  if (s1 == s2) {
    return(1.0)
  }
  
  # Jaro-Winkler-like similarity
  jaro_similarity <- function(str1, str2) {
    if (nchar(str1) == 0 && nchar(str2) == 0) {
      return(1.0)
    } else if (nchar(str1) == 0 || nchar(str2) == 0) {
      return(0.0)
    }
    
    len1 <- nchar(str1)
    len2 <- nchar(str2)
    match_window <- max(floor(max(len1, len2) / 2) - 1, 0)
    
    matches <- 0
    transpositions <- 0
    
    str1_matches <- rep(FALSE, len1)
    str2_matches <- rep(FALSE, len2)
    
    # Find matches
    for (i in seq_len(len1)) {
      start <- max(1, i - match_window)
      stop <- min(i + match_window, len2)
      
      for (j in start:stop) {
        if (str2_matches[j] || substr(str1, i, i) != substr(str2, j, j)) {
          next
        }
        str1_matches[i] <- str2_matches[j] <- TRUE
        matches <- matches + 1
        break
      }
    }
    
    if (matches == 0) {
      return(0.0)
    }
    
    # Count transpositions
    k <- 1
    for (i in seq_len(len1)) {
      if (!str1_matches[i]) {
        next
      }
      while (!str2_matches[k]) {
        k <- k + 1
      }
      if (substr(str1, i, i) != substr(str2, k, k)) {
        transpositions <- transpositions + 1
      }
      k <- k + 1
    }
    
    jaro <- (matches/len1 + matches/len2 + (matches - transpositions/2)/matches) / 3.0
    return(jaro)
  }
  
  # Combine multiple similarity measures
  jaro_score <- jaro_similarity(s1, s2)
  
  # Substring similarity
  substring_score <- 0.0
  if (grepl(s1, s2, fixed = TRUE) || grepl(s2, s1, fixed = TRUE)) {
    substring_score <- min(nchar(s1), nchar(s2)) / max(nchar(s1), nchar(s2))
  }
  
  # Token-based similarity
  tokens1 <- strsplit(gsub("[^a-z0-9]", " ", s1), "\\s+")[[1]]
  tokens2 <- strsplit(gsub("[^a-z0-9]", " ", s2), "\\s+")[[1]]
  
  tokens1 <- tokens1[nchar(tokens1) > 0]
  tokens2 <- tokens2[nchar(tokens2) > 0]
  
  token_score <- if (length(tokens1) > 0 && length(tokens2) > 0) {
    length(intersect(tokens1, tokens2)) / length(union(tokens1, tokens2))
  } else {
    0.0
  }
  
  # Semantic similarity boost for known mappings
  semantic_boost <- 0.0
  semantic_pairs <- list(
    c("country", "geo"), c("country", "geographic"), c("country", "pict"),
    c("gender", "sex"), c("year", "time"), c("year", "period"), c("time", "period"),
    c("rate", "value"), c("rate", "obs"), c("count", "value"), c("amount", "value")
  )
  
  for (pair in semantic_pairs) {
    if ((grepl(pair[1], s1) && grepl(pair[2], s2)) || 
        (grepl(pair[2], s1) && grepl(pair[1], s2))) {
      semantic_boost <- 0.3
      break
    }
  }
  
  # Weighted combination
  final_score <- 0.4 * jaro_score + 0.2 * substring_score + 0.2 * token_score + semantic_boost
  return(min(1.0, final_score))
}

#' Analyze Value Patterns Against Codelist
#'
#' Analyzes patterns in source values against target codelist to find
#' matches and suggest transformations.
#'
#' @param source_values Vector. Unique values from source column
#' @param target_codelist Data frame. Target codelist with code_id and name columns
#'
#' @return List with analysis results including match details and transformations
#' @export
analyze_value_patterns <- function(source_values, target_codelist) {
  
  analysis <- list(
    exact_matches = 0,
    fuzzy_matches = 0,
    pattern_matches = 0,
    unmatched = 0,
    match_details = list(),
    suggested_transformations = character(),
    confidence_score = 0.0
  )
  
  # Get unique non-missing source values
  unique_source <- unique(source_values[!is.na(source_values)])
  if (length(unique_source) == 0) {
    return(analysis)
  }
  
  # Get target codes
  if (!("code_id" %in% names(target_codelist))) {
    return(analysis)
  }
  
  target_codes <- unique(target_codelist$code_id)
  
  # Analyze each source value
  for (source_val in unique_source) {
    source_str <- as.character(source_val)
    best_match <- NULL
    best_score <- 0.0
    match_type <- "unmatched"
    
    for (target_code in target_codes) {
      target_str <- as.character(target_code)
      
      # Exact match
      if (tolower(source_str) == tolower(target_str)) {
        best_match <- target_str
        best_score <- 1.0
        match_type <- "exact"
        break
      }
      
      # Fuzzy match
      score <- fuzzy_match_score(source_str, target_str)
      if (score > best_score && score > 0.7) {
        best_match <- target_str
        best_score <- score
        match_type <- "fuzzy"
      }
    }
    
    # Also check against names if available
    if ("name" %in% names(target_codelist)) {
      target_names <- target_codelist$name[!is.na(target_codelist$name)]
      
      for (target_name in target_names) {
        name_str <- as.character(target_name)
        score <- fuzzy_match_score(source_str, name_str)
        if (score > best_score && score > 0.6) {
          # Find corresponding code
          matching_rows <- target_codelist[target_codelist$name == target_name & !is.na(target_codelist$name), ]
          if (nrow(matching_rows) > 0) {
            best_match <- as.character(matching_rows$code_id[1])
            best_score <- score
            match_type <- "name_fuzzy"
          }
        }
      }
    }
    
    # Categorize the match
    if (match_type == "exact") {
      analysis$exact_matches <- analysis$exact_matches + 1
    } else if (match_type %in% c("fuzzy", "name_fuzzy")) {
      analysis$fuzzy_matches <- analysis$fuzzy_matches + 1
    } else {
      analysis$unmatched <- analysis$unmatched + 1
    }
    
    if (!is.null(best_match)) {
      analysis$match_details[[source_str]] <- best_match
    }
  }
  
  # Calculate overall confidence
  total_values <- length(unique_source)
  exact_ratio <- analysis$exact_matches / total_values
  fuzzy_ratio <- analysis$fuzzy_matches / total_values
  
  analysis$confidence_score <- exact_ratio + (fuzzy_ratio * 0.7)
  
  # Suggest transformations
  if (analysis$fuzzy_matches > 0) {
    analysis$suggested_transformations <- c(analysis$suggested_transformations,
                                          "Apply fuzzy matching with manual review")
  }
  
  if (analysis$unmatched > 0) {
    unmatched_ratio <- analysis$unmatched / total_values
    if (unmatched_ratio > 0.3) {
      analysis$suggested_transformations <- c(analysis$suggested_transformations,
                                            "Review unmapped values - may need codelist extension or data cleaning")
    }
  }
  
  return(analysis)
}

#' Perform Advanced Column Mapping Inference
#'
#' Performs comprehensive mapping inference between source data profile
#' and target SDMX schema using all available techniques.
#'
#' @param engine Inference engine object from `create_inference_engine()`
#' @param source_profile Data analysis result from `analyze_data_structure()`
#' @param target_sdmx Target SDMX metadata from `extract_dsd_metadata()`
#' @param source_data Optional. Source data frame for value analysis
#'
#' @return Advanced mapping result with candidates and analysis
#' @export
#' @examples
#' \dontrun{
#' engine <- create_inference_engine()
#' source_analysis <- analyze_data_structure("data.csv")
#' target_schema <- extract_dsd_metadata("https://example.org/dsd")
#' 
#' mapping_result <- infer_advanced_mappings(engine, source_analysis, target_schema)
#' print(mapping_result$mappings)
#' }
infer_advanced_mappings <- function(engine, source_profile, target_sdmx, source_data = NULL) {
  
  if (!inherits(engine, "llmx_inference_engine")) {
    cli::cli_abort("engine must be created with {.fn create_inference_engine}")
  }
  
  if (!inherits(source_profile, "llmx_data_analysis")) {
    cli::cli_abort("source_profile must be from {.fn analyze_data_structure}")
  }
  
  if (!inherits(target_sdmx, "llmx_sdmx_metadata")) {
    cli::cli_abort("target_sdmx must be from {.fn extract_dsd_metadata}")
  }
  
  # Update engine with current data
  engine$source_profile <- source_profile
  engine$target_schema <- target_sdmx
  
  mapping_candidates <- list()
  
  # Get all target columns
  all_target_columns <- c(
    if (nrow(target_sdmx$dimensions) > 0) target_sdmx$dimensions$id else character(),
    if (nrow(target_sdmx$measures) > 0) target_sdmx$measures$id else character(),
    if (nrow(target_sdmx$attributes) > 0) target_sdmx$attributes$id else character()
  )
  
  # Analyze each source column against each target column
  for (i in seq_len(nrow(source_profile$columns))) {
    source_col <- source_profile$columns[i, ]
    source_data_col <- if (!is.null(source_data)) source_data[[source_col$column]] else NULL
    
    for (target_col in all_target_columns) {
      candidate <- analyze_column_mapping(
        engine, source_col, target_col, source_data_col, target_sdmx
      )
      
      if (!is.null(candidate) && candidate$confidence_score >= engine$min_confidence) {
        mapping_candidates <- append(mapping_candidates, list(candidate))
      }
    }
  }
  
  # Sort candidates by confidence
  mapping_candidates <- mapping_candidates[order(sapply(mapping_candidates, function(x) x$confidence_score), decreasing = TRUE)]
  
  # Remove duplicate mappings (same source column mapped to multiple targets)
  final_mappings <- select_best_mappings(mapping_candidates)
  
  # Analyze coverage and quality
  coverage_analysis <- analyze_mapping_coverage(final_mappings, source_profile, target_sdmx)
  
  # Generate recommendations
  recommendations <- generate_mapping_recommendations(final_mappings, coverage_analysis, source_profile, target_sdmx)
  
  # Calculate transformation complexity
  complexity <- calculate_transformation_complexity(final_mappings, coverage_analysis)
  
  # Identify unmapped columns
  mapped_source <- unique(sapply(final_mappings, function(x) x$source_column))
  mapped_target <- unique(sapply(final_mappings, function(x) x$target_column))
  
  unmapped_source <- setdiff(source_profile$columns$column, mapped_source)
  unmapped_target <- setdiff(all_target_columns, mapped_target)
  
  # Overall quality score
  quality_score <- calculate_mapping_quality_score(final_mappings, coverage_analysis)
  
  structure(
    list(
      mappings = final_mappings,
      coverage_analysis = coverage_analysis,
      unmapped_source_columns = unmapped_source,
      unmapped_target_columns = unmapped_target,
      quality_score = quality_score,
      recommendations = recommendations,
      transformation_complexity = complexity
    ),
    class = "llmx_advanced_mapping_result"
  )
}

#' Analyze Individual Column Mapping
#' @keywords internal
analyze_column_mapping <- function(engine, source_col, target_col, source_data_col, target_sdmx) {
  
  evidence <- list()
  confidence_score <- 0.0
  match_type <- "unknown"
  suggested_transformation <- NULL
  validation_notes <- character()
  
  # 1. Name-based matching
  name_score <- fuzzy_match_score(source_col$column, target_col)
  evidence$name_similarity <- name_score
  confidence_score <- confidence_score + name_score * 0.4
  
  if (name_score > 0.8) {
    match_type <- "name_exact"
  } else if (name_score > engine$fuzzy_threshold) {
    match_type <- "name_fuzzy"
  }
  
  # 2. Type compatibility checking
  target_info <- get_target_column_info(target_col, target_sdmx)
  if (!is.null(target_info)) {
    type_compatibility <- assess_type_compatibility(source_col, target_info)
    evidence$type_compatibility <- type_compatibility
    confidence_score <- confidence_score + type_compatibility * 0.2
    
    if (type_compatibility < 0.3) {
      validation_notes <- c(validation_notes, "Type compatibility concerns - may need data transformation")
    }
  }
  
  # 3. Statistical compatibility
  if (engine$use_statistical_analysis) {
    statistical_score <- assess_statistical_compatibility(source_col, target_info)
    evidence$statistical_compatibility <- statistical_score
    confidence_score <- confidence_score + statistical_score * 0.1
  }
  
  # 4. Apply learning from historical data
  if (engine$enable_learning) {
    learning_boost <- apply_learning_boost(engine, source_col$column, target_col)
    evidence$learning_boost <- learning_boost
    confidence_score <- confidence_score + learning_boost
  }
  
  # Normalize confidence score
  confidence_score <- max(0.0, min(1.0, confidence_score))
  
  # Determine confidence level
  confidence_level <- if (confidence_score >= 0.8) {
    MAPPING_CONFIDENCE$VERY_HIGH
  } else if (confidence_score >= 0.6) {
    MAPPING_CONFIDENCE$HIGH
  } else if (confidence_score >= 0.4) {
    MAPPING_CONFIDENCE$MEDIUM
  } else if (confidence_score >= 0.2) {
    MAPPING_CONFIDENCE$LOW
  } else {
    MAPPING_CONFIDENCE$VERY_LOW
  }
  
  # Only return candidate if above minimum threshold
  if (confidence_score >= engine$min_confidence) {
    return(list(
      source_column = source_col$column,
      target_column = target_col,
      confidence_score = confidence_score,
      confidence_level = confidence_level,
      match_type = match_type,
      evidence = evidence,
      suggested_transformation = suggested_transformation,
      validation_notes = validation_notes
    ))
  } else {
    return(NULL)
  }
}

#' Get Target Column Information
#' @keywords internal
get_target_column_info <- function(target_col, target_sdmx) {
  
  # Check dimensions
  if (nrow(target_sdmx$dimensions) > 0) {
    dim_matches <- target_sdmx$dimensions[target_sdmx$dimensions$id == target_col, ]
    if (nrow(dim_matches) > 0) {
      return(list(type = "dimension", codelist_id = dim_matches$codelist[1]))
    }
  }
  
  # Check measures
  if (nrow(target_sdmx$measures) > 0) {
    measure_matches <- target_sdmx$measures[target_sdmx$measures$id == target_col, ]
    if (nrow(measure_matches) > 0) {
      return(list(type = "measure", codelist_id = NA))
    }
  }
  
  # Check attributes
  if (nrow(target_sdmx$attributes) > 0) {
    attr_matches <- target_sdmx$attributes[target_sdmx$attributes$id == target_col, ]
    if (nrow(attr_matches) > 0) {
      return(list(type = "attribute", codelist_id = attr_matches$codelist[1]))
    }
  }
  
  return(NULL)
}

#' Assess Type Compatibility
#' @keywords internal
assess_type_compatibility <- function(source_col, target_info) {
  
  compatibility <- 0.5  # Base score
  
  if (target_info$type == "measure") {
    if (source_col$is_numeric && !source_col$is_categorical) {
      compatibility <- 1.0
    } else if (source_col$type %in% c("numeric", "integer", "double")) {
      compatibility <- 0.8
    } else {
      compatibility <- 0.1
    }
  } else if (target_info$type %in% c("dimension", "attribute")) {
    if (!is.na(target_info$codelist_id)) {
      # Should be categorical or have limited values
      if (source_col$is_categorical) {
        compatibility <- 1.0
      } else if (source_col$unique_count < 100) {
        compatibility <- 0.7
      } else {
        compatibility <- 0.3
      }
    } else {
      # Free text field
      compatibility <- 0.8
    }
  }
  
  return(compatibility)
}

#' Assess Statistical Compatibility
#' @keywords internal
assess_statistical_compatibility <- function(source_col, target_info) {
  
  if (is.null(target_info)) {
    return(0.0)
  }
  
  if (target_info$type == "measure" && source_col$is_numeric) {
    # Check if the numeric range seems reasonable
    if (source_col$type %in% c("numeric", "integer", "double")) {
      return(0.6)  # Numeric columns are generally good for measures
    }
  } else if (target_info$type %in% c("dimension", "attribute") && source_col$is_categorical) {
    # Check cardinality appropriateness
    if (source_col$unique_count >= 2 && source_col$unique_count <= 20) {
      return(0.8)  # Good categorical range
    } else if (source_col$unique_count <= 100) {
      return(0.6)
    } else {
      return(0.3)
    }
  }
  
  return(0.5)  # Default moderate compatibility
}

#' Apply Learning Boost
#' @keywords internal
apply_learning_boost <- function(engine, source_name, target_name) {
  
  boost <- 0.0
  
  # Check successful mappings
  if (source_name %in% names(engine$successful_mappings)) {
    if (target_name %in% engine$successful_mappings[[source_name]]) {
      boost <- boost + 0.1
    }
  }
  
  # Check failed mappings (negative boost)
  if (source_name %in% names(engine$failed_mappings)) {
    if (target_name %in% engine$failed_mappings[[source_name]]) {
      boost <- boost - 0.1
    }
  }
  
  return(boost)
}

#' Select Best Mappings
#' @keywords internal
select_best_mappings <- function(candidates) {
  
  selected <- list()
  used_sources <- character()
  used_targets <- character()
  
  # Sort by confidence and process highest first
  sorted_candidates <- candidates[order(sapply(candidates, function(x) x$confidence_score), decreasing = TRUE)]
  
  for (candidate in sorted_candidates) {
    # Skip if source already used
    if (candidate$source_column %in% used_sources) {
      next
    }
    
    selected <- append(selected, list(candidate))
    used_sources <- c(used_sources, candidate$source_column)
    used_targets <- c(used_targets, candidate$target_column)
  }
  
  return(selected)
}

#' Analyze Mapping Coverage
#' @keywords internal
analyze_mapping_coverage <- function(mappings, source_profile, target_sdmx) {
  
  # Get required vs optional columns (simplified)
  all_target_cols <- c(
    if (nrow(target_sdmx$dimensions) > 0) target_sdmx$dimensions$id else character(),
    if (nrow(target_sdmx$measures) > 0) target_sdmx$measures$id else character()
  )
  
  optional_cols <- if (nrow(target_sdmx$attributes) > 0) target_sdmx$attributes$id else character()
  
  mapped_required <- sum(sapply(mappings, function(m) m$target_column) %in% all_target_cols)
  mapped_optional <- sum(sapply(mappings, function(m) m$target_column) %in% optional_cols)
  
  coverage <- list(
    required_coverage = if (length(all_target_cols) > 0) mapped_required / length(all_target_cols) else 1.0,
    optional_coverage = if (length(optional_cols) > 0) mapped_optional / length(optional_cols) else 1.0,
    total_mappings = length(mappings),
    high_confidence_mappings = sum(sapply(mappings, function(m) m$confidence_level >= MAPPING_CONFIDENCE$HIGH)),
    needs_transformation = sum(sapply(mappings, function(m) !is.null(m$suggested_transformation)))
  )
  
  return(coverage)
}

#' Calculate Transformation Complexity
#' @keywords internal
calculate_transformation_complexity <- function(mappings, coverage_analysis) {
  
  complexity <- 0.0
  
  # Base complexity from number of transformations needed
  transformations_needed <- coverage_analysis$needs_transformation
  complexity <- complexity + transformations_needed * 0.1
  
  # Complexity from low confidence mappings
  low_confidence <- sum(sapply(mappings, function(m) m$confidence_level <= MAPPING_CONFIDENCE$MEDIUM))
  complexity <- complexity + low_confidence * 0.05
  
  # Complexity from poor coverage
  required_coverage <- coverage_analysis$required_coverage
  if (required_coverage < 0.8) {
    complexity <- complexity + (0.8 - required_coverage) * 0.3
  }
  
  return(min(1.0, complexity))
}

#' Calculate Mapping Quality Score
#' @keywords internal
calculate_mapping_quality_score <- function(mappings, coverage_analysis) {
  
  if (length(mappings) == 0) {
    return(0.0)
  }
  
  # Average confidence of mappings
  avg_confidence <- mean(sapply(mappings, function(m) m$confidence_score))
  
  # Weight by coverage
  required_coverage <- coverage_analysis$required_coverage
  
  # Combine scores
  quality <- 0.6 * avg_confidence + 0.4 * required_coverage
  
  return(quality)
}

#' Generate Mapping Recommendations
#' @keywords internal
generate_mapping_recommendations <- function(mappings, coverage_analysis, source_profile, target_sdmx) {
  
  recommendations <- character()
  
  # Coverage recommendations
  if (coverage_analysis$required_coverage < 0.8) {
    recommendations <- c(recommendations, "Required field coverage is below 80% - review unmapped required columns")
  }
  
  # Confidence recommendations
  low_confidence <- sum(sapply(mappings, function(m) m$confidence_level <= MAPPING_CONFIDENCE$MEDIUM))
  if (low_confidence > 0) {
    recommendations <- c(recommendations, paste(low_confidence, "mappings have medium or low confidence - manual review recommended"))
  }
  
  # Transformation recommendations
  if (coverage_analysis$needs_transformation > 0) {
    recommendations <- c(recommendations, paste(coverage_analysis$needs_transformation, "columns need value transformations"))
  }
  
  return(recommendations)
}

#' Generate Codelist Value Mapping Script
#'
#' Uses LLM to generate R code for mapping specific data column values to SDMX codelist codes.
#' This is a focused approach that targets value transformation rather than full structure mapping.
#'
#' @param source_values Vector of unique values from the source data column
#' @param target_codelist Data frame with codelist containing code_id and optional name/description columns
#' @param source_column_name Character. Name of the source column for context
#' @param target_codelist_name Character. Name/ID of the target codelist
#' @param llm_config LLM configuration from `create_llm_config()`
#' @param matching_threshold Numeric. Minimum similarity threshold for automatic matching (0-1, default: 0.7)
#' @param include_fuzzy_matching Logical. Include fuzzy matching logic in generated script (default: TRUE)
#' @param verbose Logical. Print detailed information (default: FALSE)
#'
#' @return Character string containing R code for value mapping
#' @export
#' @examples
#' \dontrun{
#' # Example: Map country names to ISO codes
#' source_values <- c("United States", "Canada", "Mexico")
#' target_codelist <- data.frame(
#'   code_id = c("US", "CA", "MX"),
#'   name = c("United States", "Canada", "Mexico")
#' )
#' 
#' config <- create_llm_config("ollama", "llama2")
#' mapping_script <- generate_codelist_value_mapping(
#'   source_values, target_codelist, "country_name", "ISO_COUNTRY", config
#' )
#' cat(mapping_script)
#' }
generate_codelist_value_mapping <- function(source_values,
                                           target_codelist,
                                           source_column_name,
                                           target_codelist_name,
                                           llm_config,
                                           matching_threshold = 0.7,
                                           include_fuzzy_matching = TRUE,
                                           verbose = FALSE) {
  
  if (!inherits(llm_config, "llmx_llm_config")) {
    cli::cli_abort("llm_config must be created with {.fn create_llm_config}")
  }
  
  # Validate inputs
  if (length(source_values) == 0) {
    cli::cli_abort("source_values cannot be empty")
  }
  
  if (!is.data.frame(target_codelist) || nrow(target_codelist) == 0) {
    cli::cli_abort("target_codelist must be a non-empty data frame")
  }
  
  if (!"code_id" %in% names(target_codelist)) {
    cli::cli_abort("target_codelist must have a 'code_id' column")
  }
  
  # Get unique non-missing source values
  clean_source_values <- unique(source_values[!is.na(source_values) & source_values != ""])
  
  if (length(clean_source_values) == 0) {
    cli::cli_abort("No valid source values found after cleaning")
  }
  
  # Perform initial analysis to provide context to LLM
  value_analysis <- analyze_value_patterns(clean_source_values, target_codelist)
  
  # Create system prompt for value mapping
  system_prompt <- create_value_mapping_system_prompt(include_fuzzy_matching)
  
  # Create detailed user prompt
  user_prompt <- create_value_mapping_prompt(
    clean_source_values, 
    target_codelist, 
    source_column_name, 
    target_codelist_name,
    value_analysis,
    matching_threshold
  )
  
  cli::cli_inform("Generating value mapping script with {.val {llm_config$provider}}...")
  
  if (verbose) {
    cli::cli_h2("VERBOSE MODE: System Prompt")
    cat(system_prompt, "\n\n")
    cli::cli_h2("VERBOSE MODE: User Prompt")
    cat(user_prompt, "\n\n")
    cli::cli_rule("Sending to LLM...")
  }
  
  # Generate mapping script using LLM
  tryCatch({
    response <- query_llm(llm_config, system_prompt, user_prompt)
    
    if (verbose) {
      cli::cli_h2("VERBOSE MODE: Raw LLM Response")
      cat("Response length:", nchar(response), "characters\n")
      cat("First 1000 characters:\n")
      cat(substr(response, 1, 1000), "\n...\n\n")
    }
    
    # Extract R code from response
    mapping_script <- extract_code_from_response(response)
    
    cli::cli_inform("\u2713 Value mapping script generated successfully")
    return(mapping_script)
    
  }, error = function(e) {
    cli::cli_abort(c(
      "Failed to generate value mapping script",
      "x" = "Error: {e$message}",
      "i" = "Check your {llm_config$provider} configuration and connection"
    ))
  })
}

#' Create system prompt for value mapping
#' @keywords internal
create_value_mapping_system_prompt <- function(include_fuzzy_matching) {
  
  base_prompt <- "You are an expert R programmer specializing in data value mapping and harmonization for statistical data processing.

Your task is to generate clean, efficient R code that maps source data values to target codelist codes.

IMPORTANT OUTPUT FORMAT:
- Respond with ONLY the R code and necessary comments
- Do NOT include thinking process, explanations, or reasoning tags
- Do NOT include introductory text
- Start directly with the R function definition

Key requirements:
- Create a function called `map_values_to_codelist` that takes a vector of source values and returns mapped codes
- Use vectorized operations for efficiency
- Handle missing values appropriately (return NA for unmappable values)
- Include clear comments explaining the mapping logic
- Use exact matching first, then fallback methods as needed
- Return the original value with a warning if no mapping is found"

  fuzzy_addition <- if (include_fuzzy_matching) {
    "\n\nInclude fuzzy matching capabilities:
- Use string similarity algorithms for approximate matching
- Apply reasonable similarity thresholds
- Prioritize exact matches over fuzzy matches
- Include confidence scoring for fuzzy matches"
  } else {
    ""
  }
  
  paste0(base_prompt, fuzzy_addition)
}

#' Create value mapping prompt
#' @keywords internal
create_value_mapping_prompt <- function(source_values, target_codelist, source_col_name, 
                                       target_codelist_name, analysis, threshold) {
  
  # Format source values for prompt
  source_vals_str <- if (length(source_values) <= 20) {
    paste0("c(\"", paste(source_values, collapse = "\", \""), "\")")
  } else {
    paste0("c(\"", paste(source_values[1:20], collapse = "\", \""), "\", ...) # ", 
           length(source_values), " total unique values")
  }
  
  # Format target codelist for prompt
  codelist_preview <- if (nrow(target_codelist) <= 10) {
    utils::capture.output(print(target_codelist, n = nrow(target_codelist)))
  } else {
    utils::capture.output(print(head(target_codelist, 10)))
  }
  codelist_str <- paste(codelist_preview, collapse = "\n")
  
  # Include analysis insights
  analysis_str <- glue::glue("
  Initial Analysis:
  - Exact matches found: {analysis$exact_matches}
  - Fuzzy matches found: {analysis$fuzzy_matches}  
  - Unmatched values: {analysis$unmatched}
  - Overall confidence: {round(analysis$confidence_score * 100, 1)}%
  ")
  
  # Create main prompt
  main_prompt <- glue::glue("
  Generate a complete R function that maps values from '{source_col_name}' to codes in '{target_codelist_name}'.
  
  SOURCE VALUES TO MAP:
  {source_vals_str}
  
  TARGET CODELIST:
  {codelist_str}
  
  {analysis_str}
  
  REQUIREMENTS:
  1. Function should be named 'map_values_to_codelist'
  2. Take a vector of source values as input
  3. Return a vector of corresponding codelist codes
  4. Handle exact matches first
  5. Use fuzzy matching with threshold {threshold} for approximate matches
  6. Return NA for values that cannot be mapped
  7. Include informative comments
  8. Add validation to ensure robust operation
  
  The function should be production-ready and handle edge cases gracefully.
  Return only the R code.
  ")
  
  main_prompt
}

#' Learn from User Feedback
#'
#' Incorporates user feedback to improve future mapping suggestions.
#'
#' @param engine Inference engine object
#' @param feedback List with accepted_mappings and rejected_mappings
#'
#' @export
#' @examples
#' \dontrun{
#' feedback <- list(
#'   accepted_mappings = list(list(source = "country", target = "geo")),
#'   rejected_mappings = list(list(source = "year", target = "obs_value"))
#' )
#' learn_from_feedback(engine, feedback)
#' }
learn_from_feedback <- function(engine, feedback) {
  
  if (!engine$enable_learning) {
    return(invisible(engine))
  }
  
  engine$user_feedback <- append(engine$user_feedback, list(feedback))
  
  # Process feedback to update learning patterns
  if ("accepted_mappings" %in% names(feedback)) {
    for (mapping in feedback$accepted_mappings) {
      source_col <- mapping$source
      target_col <- mapping$target
      
      if (!(source_col %in% names(engine$successful_mappings))) {
        engine$successful_mappings[[source_col]] <- character()
      }
      engine$successful_mappings[[source_col]] <- c(engine$successful_mappings[[source_col]], target_col)
    }
  }
  
  if ("rejected_mappings" %in% names(feedback)) {
    for (mapping in feedback$rejected_mappings) {
      source_col <- mapping$source
      target_col <- mapping$target
      
      if (!(source_col %in% names(engine$failed_mappings))) {
        engine$failed_mappings[[source_col]] <- character()
      }
      engine$failed_mappings[[source_col]] <- c(engine$failed_mappings[[source_col]], target_col)
    }
  }
  
  invisible(engine)
}

#' Print method for advanced mapping results
#' @param x An object of class "llmx_advanced_mapping_result"
#' @param ... Additional arguments (unused)
#' @export
print.llmx_advanced_mapping_result <- function(x, ...) {
  cli::cli_h1("Advanced Mapping Analysis Results")
  cli::cli_text("Quality Score: {.val {round(x$quality_score * 100, 1)}}%")
  cli::cli_text("Transformation Complexity: {.val {round(x$transformation_complexity * 100, 1)}}%")
  
  cli::cli_h2("Coverage Analysis")
  cli::cli_text("Required Coverage: {.val {round(x$coverage_analysis$required_coverage * 100, 1)}}%")
  cli::cli_text("Total Mappings: {.val {x$coverage_analysis$total_mappings}}")
  cli::cli_text("High Confidence Mappings: {.val {x$coverage_analysis$high_confidence_mappings}}")
  
  if (length(x$mappings) > 0) {
    cli::cli_h2("Top Mapping Suggestions")
    for (i in seq_len(min(5, length(x$mappings)))) {
      mapping <- x$mappings[[i]]
      confidence_pct <- round(mapping$confidence_score * 100, 1)
      cli::cli_text("{.strong {mapping$source_column}} -> {.strong {mapping$target_column}} ({confidence_pct}% confidence)")
    }
  }
  
  if (length(x$recommendations) > 0) {
    cli::cli_h2("Recommendations")
    for (rec in x$recommendations) {
      cli::cli_text("\u2022 {rec}")
    }
  }
  
  if (length(x$unmapped_source_columns) > 0) {
    cli::cli_h2("Unmapped Source Columns")
    cli::cli_text("{.val {x$unmapped_source_columns}}")
  }
  
  invisible(x)
}