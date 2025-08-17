#' Generate Data Mapping Script Using LLM
#'
#' Uses a Large Language Model to generate R code for mapping data from
#' various formats (CSV, Excel) to SDMX-CSV format based on data analysis
#' and target SDMX structure.
#'
#' @param data_analysis Result from `analyze_data_structure()`
#' @param target_sdmx Target SDMX metadata from `extract_dsd_metadata()`
#' @param mapping_type Character. Type of mapping: "simple", "complex", or "auto" (default)
#' @param model Character. LLM model to use (default: "gpt-4")
#' @param include_validation Logical. Include data validation in generated script (default: TRUE)
#' @param output_file Character. Optional file path to save generated script
#'
#' @return Character string containing the generated R script
#'
#' @export
#' @examples
#' \dontrun{
#' # Analyze source data
#' data_analysis <- analyze_data_structure("my_data.csv")
#' 
#' # Get target SDMX structure  
#' target_sdmx <- extract_dsd_metadata("https://example.org/dsd")
#' 
#' # Generate mapping script
#' script <- generate_mapping_script(data_analysis, target_sdmx)
#' cat(script)
#' }
generate_mapping_script <- function(data_analysis,
                                   target_sdmx,
                                   mapping_type = c("auto", "simple", "complex"),
                                   model = "gpt-4",
                                   include_validation = TRUE,
                                   output_file = NULL) {
  
  check_api_keys()
  check_packages("ellmer")
  
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
  
  cli::cli_inform("Generating mapping script with {.val {model}}...")
  
  # Generate code using LLM
  tryCatch({
    chat <- ellmer::chat_openai(
      system_prompt = system_prompt,
      model = model,
      temperature = 0.1  # Low temperature for more consistent code generation
    )
    
    response <- chat$chat(user_prompt)
    
    # Extract R code from response
    script_code <- extract_code_from_response(response)
    
    # Add header and metadata
    final_script <- create_complete_script(script_code, data_analysis, target_sdmx)
    
    # Save to file if requested
    if (!is.null(output_file)) {
      readr::write_file(final_script, output_file)
      cli::cli_inform("Mapping script saved to: {.path {output_file}}")
    }
    
    cli::cli_inform("✓ Mapping script generated successfully")
    return(final_script)
    
  }, error = function(e) {
    cli::cli_abort(c(
      "Failed to generate mapping script",
      "x" = "Error: {e$message}",
      "i" = "Check your API key and internet connection"
    ))
  })
}

#' Create system prompt for LLM
#' @keywords internal
create_system_prompt <- function(mapping_type, include_validation) {
  
  base_prompt <- "You are an expert R programmer specializing in statistical data transformation and SDMX standards. 
  
Your task is to generate clean, efficient, and well-documented R code that transforms source data into SDMX-CSV format.

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
  Dimensions: {data_analysis$n_rows} rows × {data_analysis$n_cols} columns
  
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
  # Try to extract code block first
  code_pattern <- "```r\\n(.*?)\\n```"
  code_match <- stringr::str_extract(response, code_pattern)
  
  if (!is.na(code_match)) {
    # Remove code block markers
    code <- stringr::str_replace_all(code_match, "```r\\n|\\n```", "")
  } else {
    # If no code blocks, assume entire response is code
    code <- response
  }
  
  # Clean up the code
  code <- stringr::str_trim(code)
  
  if (nchar(code) == 0) {
    cli::cli_abort("No R code found in LLM response")
  }
  
  code
}

#' Create complete script with header and metadata
#' @keywords internal
create_complete_script <- function(script_code, data_analysis, target_sdmx) {
  
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