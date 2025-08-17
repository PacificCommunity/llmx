#' Multiple LLM Provider Support
#'
#' Provides unified interface for multiple LLM providers including OpenAI,
#' Anthropic, Ollama, and Azure OpenAI for SDMX data transformation tasks.
#'
#' @name llm_providers
NULL

#' LLM Provider Types
#' @export
LLM_PROVIDERS <- list(
  OPENAI = "openai",
  ANTHROPIC = "anthropic", 
  OLLAMA = "ollama",
  AZURE_OPENAI = "azure_openai"
)

#' Create LLM Configuration
#'
#' Creates a configuration object for connecting to various LLM providers
#' with appropriate authentication and model settings.
#'
#' @param provider Character. LLM provider type (see LLM_PROVIDERS)
#' @param model Character. Model name/identifier
#' @param api_key Character. API key (if required by provider)
#' @param base_url Character. Base URL for API (used by Ollama and Azure)
#' @param api_version Character. API version (used by Azure OpenAI)
#' @param temperature Numeric. Sampling temperature (0-1, default: 0.1)
#' @param max_tokens Integer. Maximum tokens in response (default: 2000)
#' @param timeout_seconds Integer. Request timeout in seconds (default: 120)
#'
#' @return LLM configuration object
#' @export
#' @examples
#' \dontrun{
#' # OpenAI configuration
#' openai_config <- create_llm_config(
#'   provider = "openai",
#'   model = "gpt-4",
#'   api_key = Sys.getenv("OPENAI_API_KEY")
#' )
#' 
#' # Ollama configuration (local)
#' ollama_config <- create_llm_config(
#'   provider = "ollama",
#'   model = "llama2",
#'   base_url = "http://localhost:11434"
#' )
#' 
#' # Anthropic configuration
#' anthropic_config <- create_llm_config(
#'   provider = "anthropic",
#'   model = "claude-3-sonnet-20240229",
#'   api_key = Sys.getenv("ANTHROPIC_API_KEY")
#' )
#' }
create_llm_config <- function(provider,
                             model,
                             api_key = NULL,
                             base_url = NULL,
                             api_version = NULL,
                             temperature = 0.1,
                             max_tokens = 2000,
                             timeout_seconds = 120) {
  
  # Validate provider
  if (!provider %in% unlist(LLM_PROVIDERS)) {
    cli::cli_abort("Invalid provider. Must be one of: {.val {unlist(LLM_PROVIDERS)}}")
  }
  
  # Set default URLs and validate requirements
  config <- switch(provider,
    "openai" = {
      if (is.null(api_key)) {
        api_key <- Sys.getenv("OPENAI_API_KEY")
        if (api_key == "") {
          cli::cli_abort("OpenAI API key required. Set OPENAI_API_KEY environment variable or provide api_key parameter.")
        }
      }
      if (is.null(base_url)) base_url <- "https://api.openai.com/v1"
      
      list(
        provider = provider,
        model = model,
        api_key = api_key,
        base_url = base_url,
        temperature = temperature,
        max_tokens = max_tokens,
        timeout_seconds = timeout_seconds
      )
    },
    
    "anthropic" = {
      if (is.null(api_key)) {
        api_key <- Sys.getenv("ANTHROPIC_API_KEY")
        if (api_key == "") {
          cli::cli_abort("Anthropic API key required. Set ANTHROPIC_API_KEY environment variable or provide api_key parameter.")
        }
      }
      if (is.null(base_url)) base_url <- "https://api.anthropic.com/v1"
      
      list(
        provider = provider,
        model = model,
        api_key = api_key,
        base_url = base_url,
        temperature = temperature,
        max_tokens = max_tokens,
        timeout_seconds = timeout_seconds
      )
    },
    
    "ollama" = {
      if (is.null(base_url)) base_url <- "http://localhost:11434"
      
      list(
        provider = provider,
        model = model,
        api_key = NULL,  # Ollama doesn't require API key
        base_url = base_url,
        temperature = temperature,
        max_tokens = max_tokens,
        timeout_seconds = timeout_seconds
      )
    },
    
    "azure_openai" = {
      if (is.null(api_key)) {
        api_key <- Sys.getenv("AZURE_OPENAI_API_KEY")
        if (api_key == "") {
          cli::cli_abort("Azure OpenAI API key required. Set AZURE_OPENAI_API_KEY environment variable or provide api_key parameter.")
        }
      }
      if (is.null(base_url)) {
        cli::cli_abort("Azure OpenAI requires base_url (your endpoint URL)")
      }
      if (is.null(api_version)) api_version <- "2024-02-15-preview"
      
      list(
        provider = provider,
        model = model,
        api_key = api_key,
        base_url = base_url,
        api_version = api_version,
        temperature = temperature,
        max_tokens = max_tokens,
        timeout_seconds = timeout_seconds
      )
    }
  )
  
  structure(config, class = "llmx_llm_config")
}

#' Query LLM with Unified Interface
#'
#' Sends a query to the configured LLM provider with a unified interface
#' that abstracts provider-specific implementations.
#'
#' @param config LLM configuration from `create_llm_config()`
#' @param system_prompt Character. System prompt for the LLM
#' @param user_prompt Character. User prompt/query
#' @param context Character. Optional additional context
#'
#' @return Character string with LLM response
#' @export
#' @examples
#' \dontrun{
#' config <- create_llm_config("openai", "gpt-4")
#' 
#' response <- query_llm(
#'   config,
#'   system_prompt = "You are an expert in data transformation.",
#'   user_prompt = "How would you map country names to ISO codes?"
#' )
#' }
query_llm <- function(config, system_prompt, user_prompt, context = NULL) {
  
  if (!inherits(config, "llmx_llm_config")) {
    cli::cli_abort("config must be created with {.fn create_llm_config}")
  }
  
  check_packages(get_required_packages(config$provider))
  
  # Combine prompts
  full_prompt <- if (!is.null(context)) {
    paste(context, "\n\n", user_prompt, sep = "")
  } else {
    user_prompt
  }
  
  cli::cli_inform("Querying {.val {config$provider}} model: {.val {config$model}}")
  
  response <- switch(config$provider,
    "openai" = query_openai(config, system_prompt, full_prompt),
    "anthropic" = query_anthropic(config, system_prompt, full_prompt),
    "ollama" = query_ollama(config, system_prompt, full_prompt),
    "azure_openai" = query_azure_openai(config, system_prompt, full_prompt),
    stop("Unsupported provider: ", config$provider)
  )
  
  return(response)
}

#' Enhanced Script Generation with Multiple Providers
#'
#' Enhanced version of generate_mapping_script that supports multiple LLM providers.
#'
#' @param data_analysis Result from `analyze_data_structure()`
#' @param target_sdmx Target SDMX metadata from `extract_dsd_metadata()`
#' @param llm_config LLM configuration from `create_llm_config()`
#' @param mapping_type Character. Type of mapping: "simple", "complex", or "auto" (default)
#' @param include_validation Logical. Include data validation in generated script (default: TRUE)
#' @param output_file Character. Optional file path to save generated script
#'
#' @return Character string containing the generated R script
#' @export
#' @examples
#' \dontrun{
#' # With different providers
#' config <- create_llm_config("ollama", "llama2")
#' script <- generate_mapping_script_enhanced(data_analysis, target_sdmx, config)
#' }
generate_mapping_script_enhanced <- function(data_analysis,
                                           target_sdmx,
                                           llm_config,
                                           mapping_type = c("auto", "simple", "complex"),
                                           include_validation = TRUE,
                                           output_file = NULL) {
  
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
  system_prompt <- create_system_prompt_enhanced(mapping_type, include_validation)
  
  # Create detailed user prompt
  user_prompt <- create_mapping_prompt_enhanced(data_analysis, target_sdmx, mapping_type)
  
  cli::cli_inform("Generating mapping script with {.val {llm_config$provider}}...")
  
  # Generate code using LLM
  tryCatch({
    response <- query_llm(llm_config, system_prompt, user_prompt)
    
    # Extract R code from response
    script_code <- extract_code_from_response(response)
    
    # Add header and metadata
    final_script <- create_complete_script_enhanced(script_code, data_analysis, target_sdmx, llm_config)
    
    # Save to file if requested
    if (!is.null(output_file)) {
      writeLines(final_script, output_file)
      cli::cli_inform("Mapping script saved to: {.path {output_file}}")
    }
    
    cli::cli_inform("✓ Mapping script generated successfully")
    return(final_script)
    
  }, error = function(e) {
    cli::cli_abort(c(
      "Failed to generate mapping script",
      "x" = "Error: {e$message}",
      "i" = "Check your {llm_config$provider} configuration and connection"
    ))
  })
}

#' Get Required Packages for Provider
#' @keywords internal
get_required_packages <- function(provider) {
  switch(provider,
    "openai" = "httr2",
    "anthropic" = "httr2",  
    "ollama" = "httr2",
    "azure_openai" = "httr2",
    character()
  )
}

#' Query OpenAI API
#' @keywords internal
query_openai <- function(config, system_prompt, user_prompt) {
  
  # Try ellmer first (if available), then fallback to httr2
  if ("ellmer" %in% rownames(installed.packages())) {
    tryCatch({
      chat <- ellmer::chat_openai(
        system_prompt = system_prompt,
        model = config$model,
        temperature = config$temperature
      )
      return(chat$chat(user_prompt))
    }, error = function(e) {
      cli::cli_warn("ellmer failed, trying direct API call: {e$message}")
    })
  }
  
  # Direct API call using httr2
  req <- httr2::request(paste0(config$base_url, "/chat/completions")) |>
    httr2::req_headers(
      "Authorization" = paste("Bearer", config$api_key),
      "Content-Type" = "application/json"
    ) |>
    httr2::req_body_json(list(
      model = config$model,
      messages = list(
        list(role = "system", content = system_prompt),
        list(role = "user", content = user_prompt)
      ),
      temperature = config$temperature,
      max_tokens = config$max_tokens
    )) |>
    httr2::req_timeout(config$timeout_seconds)
  
  resp <- httr2::req_perform(req)
  result <- httr2::resp_body_json(resp)
  
  if (!is.null(result$error)) {
    stop("OpenAI API error: ", result$error$message)
  }
  
  return(result$choices[[1]]$message$content)
}

#' Query Anthropic API
#' @keywords internal
query_anthropic <- function(config, system_prompt, user_prompt) {
  
  req <- httr2::request(paste0(config$base_url, "/messages")) |>
    httr2::req_headers(
      "x-api-key" = config$api_key,
      "Content-Type" = "application/json",
      "anthropic-version" = "2023-06-01"
    ) |>
    httr2::req_body_json(list(
      model = config$model,
      max_tokens = config$max_tokens,
      temperature = config$temperature,
      system = system_prompt,
      messages = list(
        list(role = "user", content = user_prompt)
      )
    )) |>
    httr2::req_timeout(config$timeout_seconds)
  
  resp <- httr2::req_perform(req)
  result <- httr2::resp_body_json(resp)
  
  if (!is.null(result$error)) {
    stop("Anthropic API error: ", result$error$message)
  }
  
  return(result$content[[1]]$text)
}

#' Query Ollama API
#' @keywords internal
query_ollama <- function(config, system_prompt, user_prompt) {
  
  full_prompt <- paste0(system_prompt, "\n\nUser: ", user_prompt, "\n\nAssistant:")
  
  req <- httr2::request(paste0(config$base_url, "/api/generate")) |>
    httr2::req_headers("Content-Type" = "application/json") |>
    httr2::req_body_json(list(
      model = config$model,
      prompt = full_prompt,
      stream = FALSE,
      options = list(
        temperature = config$temperature,
        num_predict = config$max_tokens
      )
    )) |>
    httr2::req_timeout(config$timeout_seconds)
  
  resp <- httr2::req_perform(req)
  result <- httr2::resp_body_json(resp)
  
  if (!is.null(result$error)) {
    stop("Ollama API error: ", result$error)
  }
  
  return(result$response)
}

#' Query Azure OpenAI API
#' @keywords internal
query_azure_openai <- function(config, system_prompt, user_prompt) {
  
  # Azure OpenAI uses a different URL structure
  url <- paste0(
    config$base_url,
    "/openai/deployments/",
    config$model,
    "/chat/completions?api-version=",
    config$api_version
  )
  
  req <- httr2::request(url) |>
    httr2::req_headers(
      "api-key" = config$api_key,
      "Content-Type" = "application/json"
    ) |>
    httr2::req_body_json(list(
      messages = list(
        list(role = "system", content = system_prompt),
        list(role = "user", content = user_prompt)
      ),
      temperature = config$temperature,
      max_tokens = config$max_tokens
    )) |>
    httr2::req_timeout(config$timeout_seconds)
  
  resp <- httr2::req_perform(req)
  result <- httr2::resp_body_json(resp)
  
  if (!is.null(result$error)) {
    stop("Azure OpenAI API error: ", result$error$message)
  }
  
  return(result$choices[[1]]$message$content)
}

#' Create Enhanced System Prompt
#' @keywords internal
create_system_prompt_enhanced <- function(mapping_type, include_validation) {
  
  base_prompt <- "You are an expert R programmer specializing in statistical data transformation and SDMX standards. 

Your task is to generate clean, efficient, and well-documented R code that transforms source data into SDMX-CSV format.

Key requirements:
- Use modern tidyverse functions and the native pipe operator |>
- Include comprehensive error handling and informative messages
- Follow SDMX-CSV specifications exactly
- Generate code that is production-ready and maintainable
- Use type-safe operations and validate data at each step
- Prefer readr, dplyr, tidyr for data manipulation
- Use appropriate data type conversions"

  complexity_additions <- switch(mapping_type,
    "simple" = "\n\nFocus on straightforward 1:1 column mappings with minimal transformation.",
    "complex" = "\n\nHandle complex transformations including:\n- Multiple source columns mapping to single SDMX dimensions\n- Data reshaping from wide to long format\n- Advanced data cleaning and standardization\n- Conditional logic for data mapping",
    ""
  )
  
  validation_addition <- if (include_validation) {
    "\n\nInclude comprehensive data validation:\n- Check for required SDMX columns\n- Validate data types and formats\n- Identify and handle missing values appropriately\n- Provide clear error messages for validation failures"
  } else {
    ""
  }
  
  paste0(base_prompt, complexity_additions, validation_addition)
}

#' Create Enhanced Mapping Prompt
#' @keywords internal
create_mapping_prompt_enhanced <- function(data_analysis, target_sdmx, mapping_type) {
  
  # Prepare source data description
  source_desc <- glue::glue("
  SOURCE DATA ANALYSIS:
  File: {data_analysis$file_path}
  Type: {data_analysis$file_type} 
  Dimensions: {data_analysis$n_rows} rows × {data_analysis$n_cols} columns
  
  Column Information:
  {format_columns_for_prompt_enhanced(data_analysis$columns)}
  
  Sample Data:
  {format_sample_data_for_prompt_enhanced(data_analysis$sample_data)}
  ")
  
  # Prepare target SDMX description
  target_desc <- glue::glue("
  TARGET SDMX STRUCTURE:
  DSD ID: {target_sdmx$dsd_id}
  Agency: {target_sdmx$agency_id}
  
  Required Dimensions:
  {format_sdmx_components_for_prompt_enhanced(target_sdmx$dimensions, 'dimension')}
  
  Attributes:
  {format_sdmx_components_for_prompt_enhanced(target_sdmx$attributes, 'attribute')}
  
  Measures:
  {format_sdmx_components_for_prompt_enhanced(target_sdmx$measures, 'measure')}
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

#' Format Columns for Enhanced Prompt
#' @keywords internal
format_columns_for_prompt_enhanced <- function(columns_df) {
  if (nrow(columns_df) == 0) return("No columns found")
  
  columns_df |>
    dplyr::mutate(
      description = glue::glue("{column}: {type} ({unique_count} unique, {scales::percent(na_proportion)} NA)")
    ) |>
    dplyr::pull(description) |>
    paste(collapse = "\n  ")
}

#' Format Sample Data for Enhanced Prompt
#' @keywords internal
format_sample_data_for_prompt_enhanced <- function(sample_data) {
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

#' Format SDMX Components for Enhanced Prompt
#' @keywords internal
format_sdmx_components_for_prompt_enhanced <- function(components_df, type) {
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

#' Create Complete Script with Enhanced Header
#' @keywords internal
create_complete_script_enhanced <- function(script_code, data_analysis, target_sdmx, llm_config) {
  
  header <- glue::glue("
  # SDMX Data Transformation Script
  # Generated by llmx package on {Sys.time()}
  # Generated using: {llm_config$provider} ({llm_config$model})
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

#' Test LLM Connection
#'
#' Tests the connection to an LLM provider with a simple query.
#'
#' @param config LLM configuration from `create_llm_config()`
#'
#' @return Logical indicating if connection is successful
#' @export
#' @examples
#' \dontrun{
#' config <- create_llm_config("openai", "gpt-3.5-turbo")
#' if (test_llm_connection(config)) {
#'   message("Connection successful!")
#' }
#' }
test_llm_connection <- function(config) {
  
  if (!inherits(config, "llmx_llm_config")) {
    cli::cli_abort("config must be created with {.fn create_llm_config}")
  }
  
  tryCatch({
    response <- query_llm(
      config,
      system_prompt = "You are a helpful assistant.",
      user_prompt = "Respond with exactly: 'Connection test successful'"
    )
    
    success <- grepl("Connection test successful", response, ignore.case = TRUE)
    
    if (success) {
      cli::cli_inform("✓ {config$provider} connection successful")
    } else {
      cli::cli_warn("⚠ {config$provider} connection may have issues - unexpected response")
    }
    
    return(success)
    
  }, error = function(e) {
    cli::cli_abort("✗ {config$provider} connection failed: {e$message}")
    return(FALSE)
  })
}

#' Print method for LLM config
#' @param x An object of class "llmx_llm_config"
#' @param ... Additional arguments (unused)
#' @export
print.llmx_llm_config <- function(x, ...) {
  cli::cli_h1("LLM Configuration")
  cli::cli_text("Provider: {.val {x$provider}}")
  cli::cli_text("Model: {.val {x$model}}")
  cli::cli_text("Base URL: {.val {x$base_url}}")
  cli::cli_text("Temperature: {.val {x$temperature}}")
  cli::cli_text("Max Tokens: {.val {x$max_tokens}}")
  
  if (!is.null(x$api_key)) {
    masked_key <- paste0(substr(x$api_key, 1, 8), "...")
    cli::cli_text("API Key: {.val {masked_key}}")
  } else {
    cli::cli_text("API Key: {.val {None (not required)}}")
  }
  
  invisible(x)
}