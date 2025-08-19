
#' Generate SDMX Mapping Script Using LLM
#'
#' Uses a Large Language Model to generate R code for mapping data from
#' various formats to SDMX-CSV format based on data analysis and target SDMX structure.
#'
#' @param data_analysis Result from `analyze_data_structure()`
#' @param target_sdmx Target SDMX metadata from `extract_dsd_metadata()`
#' @param llm_config LLM configuration from `create_llm_config()`
#' @param mapping_type Character. Type of mapping: "simple", "complex", or "auto" (default)
#' @param include_validation Logical. Include data validation in generated script (default: TRUE)
#' @param verbose Logical. Print detailed prompts and intermediate steps (default: FALSE)
#'
#' @return Character string containing the full LLM response
#' @export
#' @examples
#' \dontrun{
#' # Generate mapping script
#' config <- create_llm_config("gemini", "gemini-2.5-flash-lite")
#' full_response <- generate_mapping_script(data_analysis, target_sdmx, config)
#' 
#' # Extract just the R code
#' r_code <- trim_script(full_response)
#' }
generate_mapping_script <- function(data_analysis,
                                   target_sdmx,
                                   llm_config,
                                   mapping_type = c("auto", "simple", "complex"),
                                   include_validation = TRUE,
                                   verbose = FALSE) {
  
  if (!inherits(llm_config, "llmx_llm_config")) {
    cli::cli_abort("llm_config must be created with {.fn create_llm_config}")
  }
  
  mapping_type <- match.arg(mapping_type)
  
  # Validate inputs
  if (!inherits(data_analysis, "llmx_data_analysis")) {
    cli::cli_abort("data_analysis must be from {.fn analyze_data_structure}")
  }
  
  if (!inherits(target_sdmx, "llmx_sdmx_metadata")) {
    cli::cli_abort("target_sdmx must be from {.fn extract_dsd_metadata}")
  }
  
  # Auto-determine mapping complexity if needed
  if (mapping_type == "auto") {
    complexity_indicators <- c(
      data_analysis$n_cols > 10,
      nrow(target_sdmx$dimensions) > 5,
      any(data_analysis$columns$is_categorical & data_analysis$columns$unique_count > 50)
    )
    mapping_type <- if (sum(complexity_indicators) >= 2) "complex" else "simple"
    cli::cli_inform("Auto-detected mapping type: {.val {mapping_type}}")
  }
  
  # Create system prompt based on mapping type
  system_prompt <- create_system_prompt(mapping_type, include_validation)
  
  # Create detailed user prompt
  user_prompt <- create_mapping_prompt(data_analysis, target_sdmx, mapping_type)
  
  cli::cli_inform("Generating mapping script with {.val {llm_config$provider}}...")
  
  # Print prompts if verbose mode is enabled
  if (verbose) {
    cli::cli_h2("VERBOSE MODE: System Prompt")
    cat(system_prompt, "\n\n")
    cli::cli_h2("VERBOSE MODE: User Prompt")
    cat(user_prompt, "\n\n")
    cli::cli_rule("Sending to LLM...")
  }
  
  # Generate code using LLM
  tryCatch({
    response <- query_llm(llm_config, system_prompt, user_prompt)
    
    if (verbose) {
      cli::cli_h2("VERBOSE MODE: Raw LLM Response")
      cat("Response length:", nchar(response), "characters\n")
      cat("First 1000 characters:\n")
      cat(substr(response, 1, 1000), "\n...\n\n")
    }
    
    cli::cli_inform("\u2713 LLM response received successfully")
    return(response)
    
  }, error = function(e) {
    cli::cli_abort(c(
      "Failed to generate mapping script",
      "x" = "Error: {e$message}",
      "i" = "Check your {llm_config$provider} configuration and connection"
    ))
  })
}

#' Create system prompt for LLM
#' @keywords internal
create_system_prompt <- function(mapping_type, include_validation) {
  
  base_prompt <- "You are an expert R programmer specializing in statistical data transformation and SDMX standards. 
  
Your task is to generate clean, efficient, and well-documented R code that transforms source data into SDMX-CSV format.

IMPORTANT OUTPUT FORMAT:
- Respond with ONLY the R code and necessary comments
- Do NOT include thinking process, explanations, or reasoning tags like <think>, <thinking>, etc.
- Do NOT include introductory text like 'Here is the code:' or similar
- Start directly with the R function definition

Key requirements:
- Use modern tidyverse functions and the native pipe operator |>
- Include comprehensive error handling and informative messages
- Follow SDMX-CSV specifications exactly
- Generate code that is production-ready and maintainable
- Use type-safe operations and validate data at each step"

  complexity_additions <- switch(mapping_type,
    "simple" = "\n\nFocus on straightforward 1:1 column mappings with minimal transformation.",
    "complex" = "\n\nHandle complex transformations including:\n- Multiple source columns mapping to single SDMX dimensions\n- Data reshaping from wide to long format\n- Advanced data cleaning and standardization\n- Conditional logic for data mapping",
    "auto" = ""
  )
  
  validation_addition <- if (include_validation) {
    "\n\nInclude comprehensive data validation:\n- Check for required SDMX columns\n- Validate data types and formats\n- Identify and handle missing values appropriately\n- Provide clear error messages for validation failures"
  } else {
    ""
  }
  
  paste0(base_prompt, complexity_additions, validation_addition)
}

#' Create detailed mapping prompt
#' @keywords internal  
create_mapping_prompt <- function(data_analysis, target_sdmx, mapping_type) {
  
  # Prepare source data description
  source_desc <- glue::glue("
  SOURCE DATA ANALYSIS:
  File: {data_analysis$file_path}
  Type: {data_analysis$file_type} 
  Dimensions: {data_analysis$n_rows} rows \u00d7 {data_analysis$n_cols} columns
  
  Column Information:
  {format_columns_for_prompt(data_analysis$columns)}
  
  Sample Data:
  {format_sample_data_for_prompt(data_analysis$sample_data)}
  ")
  
  # Prepare target SDMX description
  target_desc <- glue::glue("
  TARGET SDMX STRUCTURE:
  DSD ID: {target_sdmx$dsd_id}
  Agency: {target_sdmx$agency_id}
  
  Required Dimensions:
  {format_sdmx_components_for_prompt(target_sdmx$dimensions, 'dimension')}
  
  Attributes:
  {format_sdmx_components_for_prompt(target_sdmx$attributes, 'attribute')}
  
  Measures:
  {format_sdmx_components_for_prompt(target_sdmx$measures, 'measure')}
  ")
  
  main_prompt <- glue::glue("
  Generate a complete R function called `transform_to_sdmx_csv` that:
  
  1. Takes the source data as input parameter `source_data`
  2. Transforms it to match the target SDMX structure exactly  
  3. Returns a properly formatted SDMX-CSV data frame
  4. Includes error handling and validation
  5. Documents all transformation steps with comments
  
  The function should:
  - Map source columns to SDMX dimensions/attributes appropriately
  - Handle data type conversions
  - Apply any necessary data cleaning
  - Ensure SDMX-CSV compliance
  - Provide informative error messages
  
  {source_desc}
  
  {target_desc}
  
  Return only the R code for the function, properly formatted and commented.
  ")
  
  main_prompt
}

#' Format column information for LLM prompt
#' @keywords internal
format_columns_for_prompt <- function(columns_df) {
  if (nrow(columns_df) == 0) return("No columns found")
  
  columns_df |>
    dplyr::mutate(
      description = glue::glue("{column}: {type} ({unique_count} unique, {scales::percent(na_proportion)} NA)")
    ) |>
    dplyr::pull(description) |>
    paste(collapse = "\n  ")
}

#' Format sample data for LLM prompt  
#' @keywords internal
format_sample_data_for_prompt <- function(sample_data) {
  if (nrow(sample_data) == 0) return("No sample data available")
  
  # Convert to character and truncate long values
  sample_formatted <- sample_data |>
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ {
      char_vals <- as.character(.x)
      ifelse(nchar(char_vals) > 50, paste0(substr(char_vals, 1, 47), "..."), char_vals)
    }))
  
  utils::capture.output(print(sample_formatted, n = 5)) |>
    paste(collapse = "\n  ")
}

#' Format SDMX components for LLM prompt
#' @keywords internal
format_sdmx_components_for_prompt <- function(components_df, type) {
  if (nrow(components_df) == 0) return(paste("No", type, "components found"))
  
  components_df |>
    dplyr::mutate(
      description = dplyr::case_when(
        type == "dimension" ~ glue::glue("{id} (concept: {concept_ref}, codelist: {codelist})"),
        type == "attribute" ~ glue::glue("{id} (concept: {concept_ref}, level: {attachment_level})"),
        type == "measure" ~ glue::glue("{id} (concept: {concept_ref})"),
        TRUE ~ id
      )
    ) |>
    dplyr::pull(description) |>
    paste(collapse = "\n  ")
}

#' Extract R code from LLM response
#' @keywords internal
extract_code_from_response <- function(response) {
  # Clean the response
  cleaned_response <- response
  
  # Remove thinking tags if they exist
  if (grepl("<think>", cleaned_response, ignore.case = TRUE)) {
    cleaned_response <- stringr::str_replace_all(cleaned_response, "(?si)<think>.*?</think>", "")
  }
  
  if (grepl("<thinking>", cleaned_response, ignore.case = TRUE)) {
    cleaned_response <- stringr::str_replace_all(cleaned_response, "(?si)<thinking>.*?</thinking>", "")
  }
  
  # Try to extract code block first
  code_pattern <- "```r\\n(.*?)\\n```"
  code_match <- stringr::str_extract(cleaned_response, code_pattern)
  
  if (!is.na(code_match)) {
    # Remove code block markers
    code <- stringr::str_replace_all(code_match, "```r\\n|\\n```", "")
  } else {
    # Try alternative code block patterns
    alt_patterns <- c(
      "```R\\n(.*?)\\n```",
      "```\\n(.*?)\\n```"
    )
    
    code <- NULL
    for (pattern in alt_patterns) {
      code_match <- stringr::str_extract(cleaned_response, pattern)
      if (!is.na(code_match)) {
        code <- stringr::str_replace_all(code_match, "```[rR]?\\n|\\n```", "")
        break
      }
    }
    
    # If still no code blocks found, try to extract function definitions
    if (is.null(code)) {
      # Look for function definitions
      func_pattern <- "(transform_to_sdmx_csv\\s*<-\\s*function.*?)(?=\\n\\n|$)"
      func_match <- stringr::str_extract(cleaned_response, func_pattern)
      
      if (!is.na(func_match)) {
        code <- func_match
      } else {
        # Last resort: use the cleaned response
        code <- cleaned_response
      }
    }
  }
  
  # Clean up the code
  code <- stringr::str_trim(code)
  
  # Remove any remaining artifacts
  code <- stringr::str_replace_all(code, "^Here's.*?:\\s*", "")
  code <- stringr::str_replace_all(code, "^.*?Here is.*?:\\s*", "")
  
  if (nchar(code) == 0) {
    cli::cli_abort("No R code found in LLM response")
  }
  
  code
}

#' Extract R Code from LLM Response
#'
#' Extracts R code from LLM response, removing markdown code blocks and other formatting.
#'
#' @param response Character. The full LLM response
#' @param add_header Logical. Whether to add header and footer (default: TRUE)
#' @param data_analysis Optional. Data analysis object for header metadata
#' @param target_sdmx Optional. Target SDMX metadata for header
#' @param llm_config Optional. LLM config for header
#' @param output_file Character. Optional file path to save trimmed script
#'
#' @return Character string containing extracted R code
#' @export
#' @examples
#' \dontrun{
#' # Get full response from LLM
#' response <- generate_mapping_script(data_analysis, target_sdmx, config)
#' 
#' # Extract just the R code
#' r_code <- trim_script(response)
#' 
#' # Or with header and save to file
#' script <- trim_script(response, 
#'                      add_header = TRUE,
#'                      data_analysis = data_analysis,
#'                      target_sdmx = target_sdmx,
#'                      llm_config = config,
#'                      output_file = "transform.R")
#' }
trim_script <- function(response, 
                       add_header = TRUE,
                       data_analysis = NULL,
                       target_sdmx = NULL, 
                       llm_config = NULL,
                       output_file = NULL) {
  
  # Extract R code using multiple patterns
  code_patterns <- c(
    "```[rR]\\s*\\n([\\s\\S]*?)\\n```",  # ```r or ```R with content
    "```\\s*\\n([\\s\\S]*?)\\n```"       # ``` with content
  )
  
  extracted_code <- NULL
  for (pattern in code_patterns) {
    matches <- stringr::str_match(response, pattern)
    if (!is.na(matches[1])) {
      # Extract the captured group (the actual code)
      extracted_code <- matches[2]
      break
    }
  }
  
  # If no code blocks found, try to extract function definitions
  if (is.null(extracted_code)) {
    # Look for function definitions directly
    func_pattern <- "(transform_to_sdmx_csv\\s*<-\\s*function[\\s\\S]*)"
    func_match <- stringr::str_match(response, func_pattern)
    
    if (!is.na(func_match[1])) {
      extracted_code <- func_match[2]
    } else {
      # Last resort: clean the entire response
      extracted_code <- response
      # Remove obvious non-code content
      extracted_code <- stringr::str_replace_all(extracted_code, "(?i)here\\s+(is|are)\\s+.*?:", "")
      extracted_code <- stringr::str_replace_all(extracted_code, "(?i)this\\s+(function|code)\\s+.*?:", "")
    }
  }
  
  # Clean up the extracted code
  if (!is.null(extracted_code)) {
    # Remove thinking tags if present
    extracted_code <- stringr::str_replace_all(extracted_code, "(?si)<think>.*?</think>", "")
    extracted_code <- stringr::str_replace_all(extracted_code, "(?si)<thinking>.*?</thinking>", "")
    
    # Trim whitespace
    extracted_code <- stringr::str_trim(extracted_code)
  }
  
  # Add header and footer if requested
  if (add_header && !is.null(data_analysis) && !is.null(target_sdmx) && !is.null(llm_config)) {
    final_script <- create_complete_script(extracted_code, data_analysis, target_sdmx, llm_config)
  } else {
    final_script <- extracted_code
  }
  
  # Save to file if requested
  if (!is.null(output_file)) {
    writeLines(final_script, output_file)
    cli::cli_inform("Trimmed script saved to: {.path {output_file}}")
  }
  
  if (is.null(extracted_code) || nchar(stringr::str_trim(extracted_code)) == 0) {
    cli::cli_warn("No R code found in response. Returning original response.")
    return(response)
  }
  
  return(final_script)
}

#' Create simple script with basic header (legacy)
#' @keywords internal
create_simple_script <- function(script_code, data_analysis, target_sdmx) {
  
  header <- glue::glue("
  # SDMX Data Transformation Script
  # Generated by llmx package on {Sys.time()}
  #
  # Source: {data_analysis$file_path}
  # Target: {target_sdmx$dsd_id} (Agency: {target_sdmx$agency_id})
  #
  # This script transforms source data to SDMX-CSV format
  
  library(dplyr)
  library(tidyr) 
  library(readr)
  library(stringr)
  library(cli)
  
  ")
  
  footer <- glue::glue("
  
  # Example usage:
  # source_data <- readr::read_csv('{data_analysis$file_path}')
  # sdmx_data <- transform_to_sdmx_csv(source_data)
  # readr::write_csv(sdmx_data, 'output_sdmx.csv')
  ")
  
  paste0(header, "\n", script_code, "\n", footer)
}