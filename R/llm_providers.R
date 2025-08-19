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
  AZURE_OPENAI = "azure_openai",
  GEMINI = "gemini",
  DEEPSEEK = "deepseek",
  GROQ = "groq",
  GITHUB = "github"
)

#' Create LLM Configuration
#'
#' Creates a configuration object for connecting to various LLM providers
#' with appropriate authentication and model settings.
#'
#' @param provider Character. LLM provider type (see LLM_PROVIDERS)
#' @param model Character. Model name/identifier
#' @param api_key Character. API key (if required by provider). If NULL, will try to load from environment or .env file
#' @param base_url Character. Base URL for API (used by Ollama, Azure, and Gemini)
#' @param api_version Character. API version (used by Azure OpenAI)
#' @param temperature Numeric. Sampling temperature (0-1, default: 0.1)
#' @param max_tokens Integer. Maximum tokens in response (default: 2000)
#' @param timeout_seconds Integer. Request timeout in seconds (default: 120)
#' @param env_file Character. Path to .env file to load API keys from (optional)
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
#' 
#' # Gemini configuration with automatic .env loading
#' gemini_config <- create_llm_config(
#'   provider = "gemini",
#'   model = "gemini-1.5-flash"
#' )
#' 
#' # Gemini configuration with specific .env file
#' gemini_config <- create_llm_config(
#'   provider = "gemini",
#'   model = "gemini-1.5-flash",
#'   env_file = "path/to/.env"
#' )
#' }
create_llm_config <- function(provider,
                             model,
                             api_key = NULL,
                             base_url = NULL,
                             api_version = NULL,
                             temperature = 0.1,
                             max_tokens = 2000,
                             timeout_seconds = 120,
                             env_file = NULL) {
  
  # Load .env file if provided
  if (!is.null(env_file)) {
    load_env(env_file, verbose = FALSE)
  } else {
    # Try to load from common .env locations if no api_key provided
    if (is.null(api_key)) {
      # Try current directory first, then parent directory
      for (env_path in c(".env", "../.env", "../../.env")) {
        if (file.exists(env_path)) {
          load_env(env_path, verbose = FALSE)
          break
        }
      }
    }
  }
  
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
    },
    
    "gemini" = {
      if (is.null(api_key)) {
        api_key <- Sys.getenv("GEMINI_API_KEY")
        if (api_key == "") {
          cli::cli_abort("Gemini API key required. Set GEMINI_API_KEY environment variable or provide api_key parameter.")
        }
      }
      if (is.null(base_url)) base_url <- "https://generativelanguage.googleapis.com/v1beta"
      
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
    
    "deepseek" = {
      if (is.null(api_key)) {
        api_key <- Sys.getenv("DEEPSEEK_API_KEY")
        if (api_key == "") {
          cli::cli_abort("DeepSeek API key required. Set DEEPSEEK_API_KEY environment variable or provide api_key parameter.")
        }
      }
      
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
    
    "groq" = {
      if (is.null(api_key)) {
        api_key <- Sys.getenv("GROQ_API_KEY")
        if (api_key == "") {
          cli::cli_abort("Groq API key required. Set GROQ_API_KEY environment variable or provide api_key parameter.")
        }
      }
      
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
    
    "github" = {
      if (is.null(api_key)) {
        api_key <- Sys.getenv("GITHUB_TOKEN")
        if (api_key == "") {
          cli::cli_abort("GitHub token required. Set GITHUB_TOKEN environment variable or provide api_key parameter.")
        }
      }
      
      list(
        provider = provider,
        model = model,
        api_key = api_key,
        base_url = base_url,
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
  
  # Try ellmer first for supported providers (excluding Gemini for now)
  if (config$provider %in% c("openai", "anthropic", "ollama", "deepseek", "groq", "github") && 
      "ellmer" %in% rownames(installed.packages())) {
    
    tryCatch({
      response <- query_with_ellmer(config, system_prompt, full_prompt)
      return(response)
    }, error = function(e) {
      cli::cli_warn("ellmer failed ({e$message}), falling back to direct API calls")
    })
  }
  
  # Fallback to direct API calls
  response <- switch(config$provider,
    "openai" = query_openai(config, system_prompt, full_prompt),
    "anthropic" = query_anthropic(config, system_prompt, full_prompt),
    "ollama" = query_ollama(config, system_prompt, full_prompt),
    "azure_openai" = query_azure_openai(config, system_prompt, full_prompt),
    "gemini" = query_gemini(config, system_prompt, full_prompt),
    stop("Unsupported provider: ", config$provider)
  )
  
  return(response)
}


#' Get Required Packages for Provider
#' @keywords internal
get_required_packages <- function(provider) {
  # Try ellmer first for most providers, use httr2 for Gemini and others
  if (provider %in% c("openai", "anthropic", "ollama")) {
    return("ellmer")
  } else {
    return("httr2")
  }
}

#' Query LLM using ellmer package
#' @keywords internal
query_with_ellmer <- function(config, system_prompt, user_prompt) {
  
  # ellmer handles API keys directly via parameters, but set env vars as backup
  # Based on ellmer/R/provider-google.R lines 591-594
  if (config$provider == "gemini" && !is.null(config$api_key)) {
    # ellmer checks GOOGLE_API_KEY then GEMINI_API_KEY
    if (Sys.getenv("GOOGLE_API_KEY") == "") {
      Sys.setenv(GOOGLE_API_KEY = config$api_key)
    }
  }
  
  # Create ellmer chat object based on provider using standardized pattern
  chat <- switch(config$provider,
    "openai" = ellmer::chat_openai(
      system_prompt = system_prompt,
      model = config$model,
      api_key = config$api_key,
      params = list(
        temperature = config$temperature,
        max_tokens = config$max_tokens
      )
    ),
    "anthropic" = ellmer::chat_anthropic(
      system_prompt = system_prompt,
      model = config$model,
      api_key = config$api_key,
      params = list(
        temperature = config$temperature,
        max_tokens = config$max_tokens
      )
    ),
    "gemini" = ellmer::chat_google_gemini(
      system_prompt = system_prompt,
      model = config$model,
      api_key = config$api_key,
      base_url = if (!is.null(config$base_url)) config$base_url else NULL,
      params = ellmer::params(
        temperature = config$temperature,
        max_tokens = config$max_tokens
      )
    ),
    "ollama" = ellmer::chat_ollama(
      system_prompt = system_prompt,
      model = config$model,
      base_url = if (!is.null(config$base_url)) config$base_url else NULL,
      params = list(
        temperature = config$temperature
      )
    ),
    "deepseek" = ellmer::chat_deepseek(
      system_prompt = system_prompt,
      model = config$model,
      api_key = config$api_key,
      params = list(
        temperature = config$temperature,
        max_tokens = config$max_tokens
      )
    ),
    "groq" = ellmer::chat_groq(
      system_prompt = system_prompt,
      model = config$model,
      api_key = config$api_key,
      params = list(
        temperature = config$temperature,
        max_tokens = config$max_tokens
      )
    ),
    "github" = ellmer::chat_github(
      system_prompt = system_prompt,
      model = config$model,
      api_key = config$api_key,
      params = list(
        temperature = config$temperature,
        max_tokens = config$max_tokens
      )
    ),
    stop("Provider not supported by ellmer: ", config$provider)
  )
  
  # Send the user prompt and get response
  response <- chat$chat(user_prompt)
  return(response)
}

#' Query OpenAI API
#' @importFrom utils installed.packages
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

#' Query Gemini API
#' @keywords internal
query_gemini <- function(config, system_prompt, user_prompt) {
  
  # Gemini API endpoint format
  url <- paste0(
    config$base_url,
    "/models/",
    config$model,
    ":generateContent?key=",
    config$api_key
  )
  
  # Gemini API uses separate systemInstruction field (like ellmer does)
  req <- httr2::request(url) |>
    httr2::req_headers("Content-Type" = "application/json") |>
    httr2::req_body_json(list(
      contents = list(
        list(
          parts = list(
            list(text = user_prompt)
          )
        )
      ),
      systemInstruction = list(
        parts = list(
          list(text = system_prompt)
        )
      ),
      generationConfig = list(
        temperature = config$temperature,
        maxOutputTokens = config$max_tokens
      )
    )) |>
    httr2::req_timeout(config$timeout_seconds)
  
  resp <- httr2::req_perform(req)
  
  if (httr2::resp_status(resp) != 200) {
    resp_text <- httr2::resp_body_string(resp)
    stop("HTTP ", httr2::resp_status(resp), " ", httr2::resp_status_desc(resp), ": ", resp_text)
  }
  
  result <- httr2::resp_body_json(resp)
  
  if (!is.null(result$error)) {
    stop("Gemini API error: ", result$error$message)
  }
  
  # Extract content from Gemini response based on ellmer source code
  # Reference: ellmer/R/provider-google.R line 265
  if ("candidates" %in% names(result) && length(result$candidates) > 0) {
    candidate <- result$candidates[[1]]
    
    if ("content" %in% names(candidate) && 
        "parts" %in% names(candidate$content) &&
        length(candidate$content$parts) > 0 &&
        "text" %in% names(candidate$content$parts[[1]])) {
      return(candidate$content$parts[[1]]$text)
    }
  }
  
  # If extraction failed, return error
  stop("Failed to extract text from Gemini response")
}

#' Create System Prompt for SDMX Mapping
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

#' Create Detailed SDMX Mapping Prompt
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

#' Format Columns for LLM Prompt
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

#' Format Sample Data for LLM Prompt
#' @keywords internal
format_sample_data_for_prompt <- function(sample_data) {
  if (nrow(sample_data) == 0) return("No sample data available")
  
  # Convert to character and truncate long values
  sample_formatted <- sample_data |>
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ {
      char_vals <- as.character(.x)
      ifelse(nchar(char_vals) > 50, paste0(substr(char_vals, 1, 47), "..."), char_vals)
    }))
  
  utils::capture.output(print(head(sample_formatted, 5))) |>
    paste(collapse = "\n  ")
}

#' Format SDMX Components for LLM Prompt
#' @keywords internal
format_sdmx_components_for_prompt <- function(components_df, type) {
  if (nrow(components_df) == 0) return(paste("No", type, "components found"))
  
  format_component <- function(row_idx) {
    row <- components_df[row_idx, ]
    name_info <- if (!is.na(row$name)) row$name else row$id
    
    # Format codes information with actual examples
    codes_info <- if ("codes" %in% names(components_df) && !is.null(row$codes[[1]])) {
      codes_df <- row$codes[[1]]
      total_codes <- nrow(codes_df)
      
      if (total_codes > 0) {
        # Show first few codes as examples
        examples_count <- min(5, total_codes)
        examples <- codes_df$id[1:examples_count]
        examples_names <- codes_df$name[1:examples_count]
        
        # Create examples string
        examples_str <- paste(
          sapply(1:examples_count, function(i) {
            name_part <- if (!is.na(examples_names[i])) paste0(" (", examples_names[i], ")") else ""
            paste0(examples[i], name_part)
          }), 
          collapse = ", "
        )
        
        if (total_codes > examples_count) {
          paste0(examples_str, ", ... (", total_codes, " total codes)")
        } else {
          examples_str
        }
      } else {
        "no codes available"
      }
    } else {
      "codes: unknown"
    }
    
    # Build description based on type
    if (type == "dimension") {
      glue::glue("{row$id}: {name_info}\n      Codes: {codes_info}")
    } else if (type == "attribute") {
      format_info <- if ("text_format" %in% names(components_df) && !is.na(row$text_format)) {
        paste("format:", row$text_format)
      } else "format: unknown"
      glue::glue("{row$id}: {name_info} ({format_info})\n      Codes: {codes_info}")
    } else { # measure
      format_info <- if ("text_format" %in% names(components_df) && !is.na(row$text_format)) {
        paste("format:", row$text_format)
      } else "format: unknown"
      glue::glue("{row$id}: {name_info} ({format_info})")
    }
  }
  
  # Apply formatting to each row
  descriptions <- sapply(1:nrow(components_df), format_component)
  paste(descriptions, collapse = "\n  ")
}

#' Create Complete Script with Header and Metadata
#' @keywords internal
create_complete_script <- function(script_code, data_analysis, target_sdmx, llm_config) {
  
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
      cli::cli_inform("\u2713 {config$provider} connection successful")
    } else {
      cli::cli_warn("\u26a0 {config$provider} connection may have issues - unexpected response")
    }
    
    return(success)
    
  }, error = function(e) {
    cli::cli_abort("\u2717 {config$provider} connection failed: {e$message}")
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
    cli::cli_text("API Key: {.val {'None (not required)'}}")
  }
  
  invisible(x)
}