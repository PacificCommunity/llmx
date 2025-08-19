#' Load Environment Variables from .env File
#'
#' Loads environment variables from a .env file. This is useful for storing
#' API keys and other configuration without exposing them in code.
#'
#' @param env_file Character. Path to the .env file (default: ".env")
#' @param override Logical. Whether to override existing environment variables (default: FALSE)
#' @param verbose Logical. Whether to print loaded variables (default: FALSE)
#'
#' @return Logical. TRUE if file was found and loaded, FALSE otherwise
#' @export
#' @examples
#' \dontrun{
#' # Load from default .env file in current directory
#' load_env()
#' 
#' # Load from specific path
#' load_env("path/to/my/.env")
#' 
#' # Override existing variables and show what was loaded
#' load_env(".env", override = TRUE, verbose = TRUE)
#' }
load_env <- function(env_file = ".env", override = FALSE, verbose = FALSE) {
  
  if (!file.exists(env_file)) {
    if (verbose) {
      cli::cli_inform("No .env file found at: {.path {env_file}}")
    }
    return(FALSE)
  }
  
  # Read .env file
  env_lines <- readLines(env_file, warn = FALSE)
  
  # Filter out comments and empty lines
  env_lines <- env_lines[!grepl("^\\s*#", env_lines) & nzchar(trimws(env_lines))]
  
  # Track loaded variables
  loaded_vars <- character(0)
  
  # Parse and set environment variables
  for (line in env_lines) {
    if (grepl("=", line)) {
      parts <- strsplit(line, "=", fixed = TRUE)[[1]]
      if (length(parts) >= 2) {
        key <- trimws(parts[1])
        value <- trimws(paste(parts[-1], collapse = "="))
        
        # Remove quotes if present
        value <- gsub('^"(.*)"$', '\\1', value)
        value <- gsub("^'(.*)'$", '\\1', value)
        
        # Check if variable already exists
        existing_value <- Sys.getenv(key, unset = NA)
        
        # Set environment variable if it doesn't exist or override is TRUE
        if (is.na(existing_value) || override) {
          do.call(Sys.setenv, setNames(list(value), key))
          loaded_vars <- c(loaded_vars, key)
          
          if (verbose) {
            # Mask sensitive keys for display
            display_value <- if (grepl("(KEY|TOKEN|SECRET|PASSWORD)", key, ignore.case = TRUE)) {
              paste0(substr(value, 1, min(8, nchar(value))), "...")
            } else {
              value
            }
            cli::cli_inform("Loaded: {.strong {key}} = {.val {display_value}}")
          }
        } else if (verbose) {
          cli::cli_inform("Skipped: {.strong {key}} (already exists, use override = TRUE to replace)")
        }
      }
    }
  }
  
  if (verbose) {
    cli::cli_inform("Environment variables loaded from {.path {env_file}}")
  }
  
  return(TRUE)
}

#' Check for Required API Keys
#'
#' Checks if required API keys are available in environment variables.
#' Optionally loads them from a .env file first.
#'
#' @param providers Character vector. LLM providers to check (default: all supported)
#' @param env_file Character. Path to .env file to load first (optional)
#' @param required_only Logical. Only check for keys that are actually required (default: TRUE)
#'
#' @return Named logical vector indicating which API keys are available
#' @export
#' @examples
#' \dontrun{
#' # Check all providers
#' check_api_keys()
#' 
#' # Check specific providers after loading .env
#' check_api_keys(c("gemini", "openai"), env_file = ".env")
#' 
#' # Show status of all possible keys
#' check_api_keys(required_only = FALSE)
#' }
check_api_keys <- function(providers = c("openai", "anthropic", "gemini", "ollama", "azure_openai", "deepseek", "groq", "github"),
                          env_file = NULL,
                          required_only = TRUE) {
  
  # Load .env file if provided
  if (!is.null(env_file)) {
    load_env(env_file, verbose = FALSE)
  }
  
  # Define API key environment variable names for each provider
  key_mappings <- list(
    openai = "OPENAI_API_KEY",
    anthropic = "ANTHROPIC_API_KEY",
    gemini = c("GEMINI_API_KEY", "GOOGLE_API_KEY"),  # Gemini checks both
    ollama = NULL,  # Ollama doesn't require API key
    azure_openai = "AZURE_OPENAI_API_KEY",
    deepseek = "DEEPSEEK_API_KEY",
    groq = "GROQ_API_KEY",
    github = "GITHUB_TOKEN"
  )
  
  # Check each provider
  results <- setNames(logical(length(providers)), providers)
  
  for (provider in providers) {
    keys_to_check <- key_mappings[[provider]]
    
    if (is.null(keys_to_check)) {
      # Provider doesn't require API key (like Ollama)
      results[provider] <- TRUE
    } else {
      # Check if any of the possible keys are set
      key_available <- any(sapply(keys_to_check, function(key) {
        value <- Sys.getenv(key, unset = "")
        nzchar(value)
      }))
      results[provider] <- key_available
    }
  }
  
  # Filter to only required providers if requested
  if (required_only) {
    # Only show providers that actually need API keys
    results <- results[!sapply(providers, function(p) is.null(key_mappings[[p]]))]
  }
  
  return(results)
}

#' Set API Key for Provider
#'
#' Sets an API key for a specific LLM provider. Useful for programmatic setup.
#'
#' @param provider Character. LLM provider name
#' @param api_key Character. The API key to set
#' @param overwrite Logical. Whether to overwrite existing key (default: FALSE)
#'
#' @return Logical. TRUE if key was set, FALSE if it already existed and overwrite=FALSE
#' @export
#' @examples
#' \dontrun{
#' # Set Gemini API key
#' set_api_key("gemini", "your_api_key_here")
#' 
#' # Force overwrite existing key
#' set_api_key("openai", "new_key", overwrite = TRUE)
#' }
set_api_key <- function(provider, api_key, overwrite = FALSE) {
  
  # Define primary environment variable for each provider
  key_names <- list(
    openai = "OPENAI_API_KEY",
    anthropic = "ANTHROPIC_API_KEY",
    gemini = "GEMINI_API_KEY",
    azure_openai = "AZURE_OPENAI_API_KEY",
    deepseek = "DEEPSEEK_API_KEY",
    groq = "GROQ_API_KEY",
    github = "GITHUB_TOKEN"
  )
  
  if (!provider %in% names(key_names)) {
    cli::cli_abort("Unsupported provider: {.val {provider}}")
  }
  
  env_var <- key_names[[provider]]
  existing_value <- Sys.getenv(env_var, unset = "")
  
  if (nzchar(existing_value) && !overwrite) {
    cli::cli_inform("API key for {.strong {provider}} already exists. Use overwrite = TRUE to replace.")
    return(FALSE)
  }
  
  do.call(Sys.setenv, setNames(list(api_key), env_var))
  cli::cli_inform("API key set for {.strong {provider}}")
  return(TRUE)
}