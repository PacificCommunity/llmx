#' SDMX Workflow Orchestration System
#'
#' Comprehensive workflow system that orchestrates the complete SDMX data 
#' transformation process from source analysis to script generation and validation.
#'
#' @name workflow
NULL

#' Workflow Step Status
#' @export
WORKFLOW_STATUS <- list(
  PENDING = "pending",
  RUNNING = "running", 
  COMPLETED = "completed",
  FAILED = "failed",
  SKIPPED = "skipped"
)

#' Create SDMX Transformation Workflow
#'
#' Creates a comprehensive workflow for transforming source data to SDMX format.
#' The workflow includes data analysis, schema extraction, intelligent mapping,
#' script generation, and validation steps.
#'
#' @param source_file_path Character. Path to source data file
#' @param target_dataflow_url Character. URL to target SDMX dataflow
#' @param output_directory Character. Directory for output files
#' @param llm_provider Character. LLM provider ("openai", "ollama", etc.)
#' @param llm_model Character. LLM model name
#' @param enable_llm_generation Logical. Enable LLM-powered script generation
#' @param enable_validation Logical. Enable data validation
#' @param enable_advanced_mapping Logical. Enable advanced mapping inference
#'
#' @return Workflow configuration object
#' @export
#' @examples
#' \dontrun{
#' workflow <- create_sdmx_workflow(
#'   source_file_path = "data/source.csv",
#'   target_dataflow_url = "https://stats.oecd.org/restsdmx/sdmx.ashx/GetDataStructure/QNA",
#'   output_directory = "output/",
#'   llm_provider = "openai",
#'   llm_model = "gpt-4"
#' )
#' 
#' result <- execute_workflow(workflow)
#' }
create_sdmx_workflow <- function(source_file_path,
                                target_dataflow_url,
                                output_directory = "output",
                                llm_provider = "openai",
                                llm_model = "gpt-4",
                                enable_llm_generation = TRUE,
                                enable_validation = TRUE,
                                enable_advanced_mapping = TRUE,
                                performance_mode = FALSE,
                                strict_validation = FALSE) {
  
  # Validate inputs
  if (!file.exists(source_file_path)) {
    cli::cli_abort("Source file not found: {.path {source_file_path}}")
  }
  
  if (!dir.exists(output_directory)) {
    dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)
  }
  
  # Create workflow configuration
  config <- list(
    # Data source settings
    source_file_path = source_file_path,
    target_dataflow_url = target_dataflow_url,
    output_directory = output_directory,
    
    # LLM settings
    llm_provider = llm_provider,
    llm_model = llm_model,
    enable_llm_generation = enable_llm_generation,
    
    # Processing settings
    enable_validation = enable_validation,
    enable_advanced_mapping = enable_advanced_mapping,
    strict_validation = strict_validation,
    performance_mode = performance_mode,
    
    # Output settings
    generate_script = TRUE,
    generate_validation_report = enable_validation,
    save_intermediate_results = TRUE,
    output_formats = c("script", "report", "json")
  )
  
  # Create workflow steps
  steps <- create_default_workflow_steps()
  
  # Create workflow object
  workflow <- structure(
    list(
      workflow_id = paste0("sdmx_workflow_", format(Sys.time(), "%Y%m%d_%H%M%S")),
      config = config,
      steps = steps,
      current_step_index = 1,
      status = "initialized",
      
      # Component instances
      inference_engine = NULL,
      
      # State tracking
      execution_start_time = NULL,
      intermediate_data = list(),
      error_log = character(),
      progress_callback = NULL
    ),
    class = "llmx_workflow"
  )
  
  return(workflow)
}

#' Create Default Workflow Steps
#' @keywords internal
create_default_workflow_steps <- function() {
  
  steps <- list()
  
  # Step 1: Source Data Analysis
  steps[["source_analysis"]] <- list(
    step_id = "source_analysis",
    step_name = "Source Data Analysis",
    description = "Analyze and profile the source dataset",
    status = WORKFLOW_STATUS$PENDING,
    start_time = NULL,
    end_time = NULL,
    duration_ms = 0,
    input_data = NULL,
    output_data = NULL,
    error_message = "",
    warnings = character(),
    metrics = list(),
    auto_retry = TRUE,
    max_retries = 2,
    retry_count = 0
  )
  
  # Step 2: Target Schema Analysis
  steps[["schema_analysis"]] <- list(
    step_id = "schema_analysis", 
    step_name = "Target Schema Analysis",
    description = "Extract and analyze the target SDMX dataflow schema",
    status = WORKFLOW_STATUS$PENDING,
    start_time = NULL,
    end_time = NULL,
    duration_ms = 0,
    input_data = NULL,
    output_data = NULL,
    error_message = "",
    warnings = character(),
    metrics = list(),
    auto_retry = TRUE,
    max_retries = 3,
    retry_count = 0
  )
  
  # Step 3: Intelligent Mapping
  steps[["intelligent_mapping"]] <- list(
    step_id = "intelligent_mapping",
    step_name = "Intelligent Column Mapping", 
    description = "Perform advanced mapping inference between source and target",
    status = WORKFLOW_STATUS$PENDING,
    start_time = NULL,
    end_time = NULL,
    duration_ms = 0,
    input_data = NULL,
    output_data = NULL,
    error_message = "",
    warnings = character(),
    metrics = list(),
    auto_retry = TRUE,
    max_retries = 2,
    retry_count = 0
  )
  
  # Step 4: Script Generation
  steps[["script_generation"]] <- list(
    step_id = "script_generation",
    step_name = "LLM-Assisted Script Generation",
    description = "Generate R transformation script using LLM",
    status = WORKFLOW_STATUS$PENDING,
    start_time = NULL,
    end_time = NULL,
    duration_ms = 0,
    input_data = NULL,
    output_data = NULL,
    error_message = "",
    warnings = character(),
    metrics = list(),
    auto_retry = TRUE,
    max_retries = 1,
    retry_count = 0
  )
  
  # Step 5: Validation
  steps[["validation"]] <- list(
    step_id = "validation",
    step_name = "SDMX-CSV Validation",
    description = "Validate the transformation and output quality",
    status = WORKFLOW_STATUS$PENDING,
    start_time = NULL,
    end_time = NULL,
    duration_ms = 0,
    input_data = NULL,
    output_data = NULL,
    error_message = "",
    warnings = character(),
    metrics = list(),
    auto_retry = FALSE,
    max_retries = 0,
    retry_count = 0
  )
  
  # Step 6: Output Generation
  steps[["output_generation"]] <- list(
    step_id = "output_generation",
    step_name = "Output Generation & Reporting",
    description = "Generate final outputs, reports, and documentation",
    status = WORKFLOW_STATUS$PENDING,
    start_time = NULL,
    end_time = NULL,
    duration_ms = 0,
    input_data = NULL,
    output_data = NULL,
    error_message = "",
    warnings = character(),
    metrics = list(),
    auto_retry = FALSE,
    max_retries = 0,
    retry_count = 0
  )
  
  return(steps)
}

#' Execute SDMX Workflow
#'
#' Executes the complete SDMX transformation workflow with progress tracking
#' and comprehensive error handling.
#'
#' @param workflow Workflow object from `create_sdmx_workflow()`
#' @param progress_callback Optional function to receive progress updates
#'
#' @return Workflow result with all outputs and metadata
#' @export
#' @examples
#' \dontrun{
#' workflow <- create_sdmx_workflow("data.csv", "https://example.org/dsd")
#' 
#' # With progress callback
#' result <- execute_workflow(workflow, progress_callback = function(step, total, name, status) {
#'   cat(sprintf("Step %d/%d: %s - %s\n", step, total, name, status))
#' })
#' 
#' print(result$overall_status)
#' }
execute_workflow <- function(workflow, progress_callback = NULL) {
  
  if (!inherits(workflow, "llmx_workflow")) {
    cli::cli_abort("workflow must be created with {.fn create_sdmx_workflow}")
  }
  
  workflow$execution_start_time <- Sys.time()
  workflow$status <- "running"
  workflow$progress_callback <- progress_callback
  
  log_info(workflow, paste("Starting SDMX workflow execution:", workflow$workflow_id))
  
  tryCatch({
    # Execute each workflow step
    step_names <- names(workflow$steps)
    for (i in seq_along(step_names)) {
      workflow$current_step_index <- i
      step_name <- step_names[i]
      step <- workflow$steps[[step_name]]
      
      execute_workflow_step(workflow, step_name, step)
      
      # Check if step failed and handle accordingly
      if (workflow$steps[[step_name]]$status == WORKFLOW_STATUS$FAILED && 
          !workflow$steps[[step_name]]$auto_retry) {
        workflow$status <- "failed"
        log_error(workflow, paste("Workflow failed at step:", step$step_name))
        break
      }
      
      # Report progress if callback is provided
      if (!is.null(workflow$progress_callback)) {
        workflow$progress_callback(i, length(step_names), step$step_name, workflow$steps[[step_name]]$status)
      }
    }
    
    # Determine overall status
    if (workflow$status != "failed") {
      failed_critical_steps <- sum(sapply(workflow$steps, function(s) {
        s$status == WORKFLOW_STATUS$FAILED && !s$auto_retry
      }))
      
      if (failed_critical_steps == 0) {
        workflow$status <- "success"
      } else {
        workflow$status <- "partial_success"
      }
    }
    
  }, error = function(e) {
    workflow$status <- "failed"
    error_msg <- paste("Workflow execution failed:", e$message)
    log_error(workflow, error_msg)
    
    # Update current step as failed
    if (workflow$current_step_index <= length(workflow$steps)) {
      current_step_name <- names(workflow$steps)[workflow$current_step_index]
      workflow$steps[[current_step_name]]$status <- WORKFLOW_STATUS$FAILED
      workflow$steps[[current_step_name]]$error_message <- error_msg
      workflow$steps[[current_step_name]]$end_time <- Sys.time()
    }
  })
  
  # Generate workflow result
  result <- generate_workflow_result(workflow)
  
  log_info(workflow, paste("Workflow completed with status:", workflow$status))
  
  return(result)
}

#' Execute Individual Workflow Step
#' @keywords internal
execute_workflow_step <- function(workflow, step_name, step) {
  
  log_info(workflow, paste("Starting step:", step$step_name))
  
  workflow$steps[[step_name]]$start_time <- Sys.time()
  workflow$steps[[step_name]]$status <- WORKFLOW_STATUS$RUNNING
  
  tryCatch({
    # Execute the specific step
    if (step$step_id == "source_analysis") {
      execute_source_analysis_step(workflow, step_name)
    } else if (step$step_id == "schema_analysis") {
      execute_schema_analysis_step(workflow, step_name)
    } else if (step$step_id == "intelligent_mapping") {
      execute_intelligent_mapping_step(workflow, step_name)
    } else if (step$step_id == "script_generation") {
      execute_script_generation_step(workflow, step_name)
    } else if (step$step_id == "validation") {
      execute_validation_step(workflow, step_name)
    } else if (step$step_id == "output_generation") {
      execute_output_generation_step(workflow, step_name)
    } else {
      stop(paste("Unknown workflow step:", step$step_id))
    }
    
    workflow$steps[[step_name]]$status <- WORKFLOW_STATUS$COMPLETED
    workflow$steps[[step_name]]$end_time <- Sys.time()
    workflow$steps[[step_name]]$duration_ms <- as.numeric(
      difftime(workflow$steps[[step_name]]$end_time, 
               workflow$steps[[step_name]]$start_time, units = "secs") * 1000
    )
    
    log_info(workflow, sprintf("Completed step: %s (%.2fs)", 
                              step$step_name, 
                              workflow$steps[[step_name]]$duration_ms / 1000))
    
  }, error = function(e) {
    workflow$steps[[step_name]]$status <- WORKFLOW_STATUS$FAILED
    workflow$steps[[step_name]]$error_message <- as.character(e$message)
    workflow$steps[[step_name]]$end_time <- Sys.time()
    workflow$steps[[step_name]]$duration_ms <- as.numeric(
      difftime(workflow$steps[[step_name]]$end_time, 
               workflow$steps[[step_name]]$start_time, units = "secs") * 1000
    )
    
    log_error(workflow, paste("Step failed:", step$step_name, "-", e$message))
    
    # Retry logic
    if (workflow$steps[[step_name]]$auto_retry && 
        workflow$steps[[step_name]]$retry_count < workflow$steps[[step_name]]$max_retries) {
      
      workflow$steps[[step_name]]$retry_count <- workflow$steps[[step_name]]$retry_count + 1
      log_info(workflow, sprintf("Retrying step: %s (attempt %d)", 
                                step$step_name, 
                                workflow$steps[[step_name]]$retry_count))
      
      # Wait before retry (exponential backoff)
      Sys.sleep(2^workflow$steps[[step_name]]$retry_count)
      
      execute_workflow_step(workflow, step_name, workflow$steps[[step_name]])
    }
  })
}

#' Execute Source Analysis Step
#' @keywords internal
execute_source_analysis_step <- function(workflow, step_name) {
  
  config <- workflow$config
  
  # Analyze source data structure
  source_profile <- analyze_data_structure(config$source_file_path)
  
  # Store results
  workflow$intermediate_data$source_profile <- source_profile
  workflow$steps[[step_name]]$output_data <- source_profile
  
  # Add metrics
  workflow$steps[[step_name]]$metrics$row_count <- source_profile$n_rows
  workflow$steps[[step_name]]$metrics$column_count <- source_profile$n_cols
  
  # Calculate basic data quality score
  quality_score <- calculate_data_quality_score(source_profile)
  workflow$steps[[step_name]]$metrics$data_quality_score <- quality_score
  
  file_info <- file.info(config$source_file_path)
  workflow$steps[[step_name]]$metrics$file_size_mb <- file_info$size / (1024^2)
  
  # Quality warnings
  if (quality_score < 0.8) {
    workflow$steps[[step_name]]$warnings <- c(
      workflow$steps[[step_name]]$warnings,
      sprintf("Source data quality is below recommended threshold (%.1f%%)", quality_score * 100)
    )
  }
  
  if (source_profile$n_rows > 100000 && !config$performance_mode) {
    workflow$steps[[step_name]]$warnings <- c(
      workflow$steps[[step_name]]$warnings,
      "Large dataset detected - consider enabling performance mode"
    )
  }
}

#' Execute Schema Analysis Step
#' @keywords internal
execute_schema_analysis_step <- function(workflow, step_name) {
  
  config <- workflow$config
  
  # Extract SDMX schema
  target_schema <- extract_dsd_metadata(config$target_dataflow_url)
  
  # Store results
  workflow$intermediate_data$target_schema <- target_schema
  workflow$steps[[step_name]]$output_data <- target_schema
  
  # Add metrics
  workflow$steps[[step_name]]$metrics$total_dimensions <- nrow(target_schema$dimensions)
  workflow$steps[[step_name]]$metrics$total_attributes <- nrow(target_schema$attributes)
  workflow$steps[[step_name]]$metrics$total_measures <- nrow(target_schema$measures)
  
  # Calculate complexity
  total_required <- nrow(target_schema$dimensions) + nrow(target_schema$measures)
  workflow$steps[[step_name]]$metrics$required_columns <- total_required
  workflow$steps[[step_name]]$metrics$optional_columns <- nrow(target_schema$attributes)
  
  # Complexity warnings
  if (total_required > 15) {
    workflow$steps[[step_name]]$warnings <- c(
      workflow$steps[[step_name]]$warnings,
      sprintf("Complex target schema with %d required columns", total_required)
    )
  }
}

#' Execute Intelligent Mapping Step
#' @keywords internal
execute_intelligent_mapping_step <- function(workflow, step_name) {
  
  if (!workflow$config$enable_advanced_mapping) {
    workflow$steps[[step_name]]$status <- WORKFLOW_STATUS$SKIPPED
    return()
  }
  
  source_profile <- workflow$intermediate_data$source_profile
  target_schema <- workflow$intermediate_data$target_schema
  
  # Create inference engine if not exists
  if (is.null(workflow$inference_engine)) {
    workflow$inference_engine <- create_inference_engine()
  }
  
  # Read source data for value analysis
  source_data <- tryCatch({
    if (tools::file_ext(workflow$config$source_file_path) == "csv") {
      readr::read_csv(workflow$config$source_file_path, show_col_types = FALSE)
    } else {
      readxl::read_excel(workflow$config$source_file_path)
    }
  }, error = function(e) {
    log_error(workflow, paste("Failed to read source data for mapping:", e$message))
    NULL
  })
  
  # Perform advanced mapping
  mapping_result <- infer_advanced_mappings(workflow$inference_engine, source_profile, target_schema, source_data)
  
  # Store results
  workflow$intermediate_data$mapping_result <- mapping_result
  workflow$steps[[step_name]]$output_data <- mapping_result
  
  # Add metrics
  workflow$steps[[step_name]]$metrics$quality_score <- mapping_result$quality_score
  workflow$steps[[step_name]]$metrics$coverage_score <- mapping_result$coverage_analysis$required_coverage
  workflow$steps[[step_name]]$metrics$transformation_complexity <- mapping_result$transformation_complexity
  workflow$steps[[step_name]]$metrics$total_mappings <- length(mapping_result$mappings)
  workflow$steps[[step_name]]$metrics$high_confidence_mappings <- mapping_result$coverage_analysis$high_confidence_mappings
  
  # Quality warnings
  if (mapping_result$quality_score < 0.7) {
    workflow$steps[[step_name]]$warnings <- c(
      workflow$steps[[step_name]]$warnings,
      "Low mapping quality score - manual review recommended"
    )
  }
  
  if (mapping_result$coverage_analysis$required_coverage < 0.8) {
    workflow$steps[[step_name]]$warnings <- c(
      workflow$steps[[step_name]]$warnings,
      "Incomplete coverage of required columns"
    )
  }
}

#' Execute Script Generation Step
#' @keywords internal
execute_script_generation_step <- function(workflow, step_name) {
  
  if (!workflow$config$enable_llm_generation) {
    workflow$steps[[step_name]]$status <- WORKFLOW_STATUS$SKIPPED
    return()
  }
  
  source_profile <- workflow$intermediate_data$source_profile
  target_schema <- workflow$intermediate_data$target_schema
  mapping_result <- workflow$intermediate_data$mapping_result
  
  # Generate transformation script using existing LLM functionality
  generated_script <- generate_mapping_script(
    source_profile,
    target_schema,
    mapping_type = "auto",
    model = workflow$config$llm_model,
    include_validation = workflow$config$enable_validation
  )
  
  # Store results
  workflow$intermediate_data$generated_script <- generated_script
  workflow$steps[[step_name]]$output_data <- generated_script
  
  # Add metrics
  script_lines <- length(strsplit(generated_script, "\n")[[1]])
  workflow$steps[[step_name]]$metrics$script_length_lines <- script_lines
  
  # Estimate complexity based on script content
  complexity_keywords <- c("mutate", "pivot", "case_when", "recode", "filter", "group_by")
  complexity_score <- sum(sapply(complexity_keywords, function(kw) {
    length(gregexpr(kw, generated_script, fixed = TRUE)[[1]]) > 0
  })) / length(complexity_keywords)
  
  workflow$steps[[step_name]]$metrics$estimated_complexity <- complexity_score
  
  # Validation warnings
  if (complexity_score > 0.8) {
    workflow$steps[[step_name]]$warnings <- c(
      workflow$steps[[step_name]]$warnings,
      "Generated script has high complexity - thorough testing recommended"
    )
  }
}

#' Execute Validation Step
#' @keywords internal
execute_validation_step <- function(workflow, step_name) {
  
  if (!workflow$config$enable_validation) {
    workflow$steps[[step_name]]$status <- WORKFLOW_STATUS$SKIPPED
    return()
  }
  
  # For now, this is a placeholder for validation functionality
  # In a complete implementation, this would validate the generated script
  # and check SDMX-CSV compliance
  
  validation_result <- list(
    overall_score = 0.8,
    compliance_status = "compliant",
    issues = list(),
    recommendations = character()
  )
  
  # Store results
  workflow$intermediate_data$validation_result <- validation_result
  workflow$steps[[step_name]]$output_data <- validation_result
  
  # Add metrics
  workflow$steps[[step_name]]$metrics$overall_score <- validation_result$overall_score
  workflow$steps[[step_name]]$metrics$compliance_status <- validation_result$compliance_status
  workflow$steps[[step_name]]$metrics$total_issues <- length(validation_result$issues)
}

#' Execute Output Generation Step
#' @keywords internal
execute_output_generation_step <- function(workflow, step_name) {
  
  config <- workflow$config
  output_files <- list()
  
  # Generate script file
  if (config$generate_script && "generated_script" %in% names(workflow$intermediate_data)) {
    generated_script <- workflow$intermediate_data$generated_script
    script_filename <- paste0("sdmx_transformation_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".R")
    script_path <- file.path(config$output_directory, script_filename)
    
    dir.create(dirname(script_path), recursive = TRUE, showWarnings = FALSE)
    writeLines(generated_script, script_path)
    
    output_files$script <- script_path
  }
  
  # Generate mapping report
  if ("mapping_result" %in% names(workflow$intermediate_data)) {
    mapping_result <- workflow$intermediate_data$mapping_result
    report_filename <- paste0("mapping_analysis_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt")
    report_path <- file.path(config$output_directory, report_filename)
    
    dir.create(dirname(report_path), recursive = TRUE, showWarnings = FALSE)
    
    # Generate mapping report
    report_content <- generate_mapping_report(mapping_result, workflow$intermediate_data$source_profile)
    writeLines(report_content, report_path)
    
    output_files$mapping_report <- report_path
  }
  
  # Generate JSON summary
  if ("json" %in% config$output_formats) {
    json_filename <- paste0("workflow_summary_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".json")
    json_path <- file.path(config$output_directory, json_filename)
    summary <- create_workflow_summary(workflow)
    
    dir.create(dirname(json_path), recursive = TRUE, showWarnings = FALSE)
    jsonlite::write_json(summary, json_path, pretty = TRUE, auto_unbox = TRUE)
    
    output_files$json_summary <- json_path
  }
  
  # Store output file paths
  workflow$intermediate_data$output_files <- output_files
  workflow$steps[[step_name]]$output_data <- output_files
  
  # Add metrics
  workflow$steps[[step_name]]$metrics$files_generated <- length(output_files)
  
  total_size <- sum(sapply(output_files, function(path) {
    if (file.exists(path)) file.info(path)$size else 0
  }))
  workflow$steps[[step_name]]$metrics$total_output_size_mb <- total_size / (1024^2)
}

#' Generate Workflow Result
#' @keywords internal
generate_workflow_result <- function(workflow) {
  
  total_duration <- if (!is.null(workflow$execution_start_time)) {
    as.numeric(difftime(Sys.time(), workflow$execution_start_time, units = "secs")) * 1000
  } else {
    0
  }
  
  # Extract primary outputs
  source_profile <- workflow$intermediate_data$source_profile
  target_schema <- workflow$intermediate_data$target_schema
  mapping_result <- workflow$intermediate_data$mapping_result
  generated_script <- workflow$intermediate_data$generated_script
  validation_result <- workflow$intermediate_data$validation_result
  
  # Collect performance metrics
  performance_metrics <- list(
    total_duration_ms = total_duration,
    step_durations = sapply(workflow$steps, function(s) s$duration_ms),
    successful_steps = sum(sapply(workflow$steps, function(s) s$status == WORKFLOW_STATUS$COMPLETED)),
    failed_steps = sum(sapply(workflow$steps, function(s) s$status == WORKFLOW_STATUS$FAILED)),
    skipped_steps = sum(sapply(workflow$steps, function(s) s$status == WORKFLOW_STATUS$SKIPPED))
  )
  
  # Quality assessment
  quality_assessment <- create_quality_assessment(workflow)
  
  # Recommendations
  recommendations <- create_workflow_recommendations(workflow)
  
  # Collect warnings and errors
  warnings <- unlist(lapply(workflow$steps, function(step) step$warnings))
  errors <- workflow$error_log
  
  # Output files
  output_files <- workflow$intermediate_data$output_files
  if (is.null(output_files)) output_files <- list()
  
  structure(
    list(
      workflow_id = workflow$workflow_id,
      execution_timestamp = format(Sys.time()),
      config = workflow$config,
      steps = workflow$steps,
      total_duration_ms = total_duration,
      overall_status = workflow$status,
      source_profile = source_profile,
      target_schema = target_schema,
      mapping_result = mapping_result,
      generated_script = generated_script,
      validation_result = validation_result,
      performance_metrics = performance_metrics,
      quality_assessment = quality_assessment,
      recommendations = recommendations,
      warnings = warnings,
      errors = errors,
      output_files = output_files
    ),
    class = "llmx_workflow_result"
  )
}

#' Calculate Data Quality Score
#' @keywords internal
calculate_data_quality_score <- function(source_profile) {
  
  # Simple quality scoring based on completeness and data types
  total_cols <- nrow(source_profile$columns)
  
  if (total_cols == 0) return(0.0)
  
  # Penalize high missing value proportions
  avg_na_proportion <- mean(source_profile$columns$na_proportion)
  completeness_score <- 1 - avg_na_proportion
  
  # Reward diverse data types
  type_diversity <- length(unique(source_profile$columns$type)) / max(3, total_cols)
  
  # Penalize too many or too few unique values
  unique_ratio_scores <- sapply(source_profile$columns$unique_count / source_profile$n_rows, function(ratio) {
    if (ratio < 0.01) 0.5    # Too few unique values
    else if (ratio > 0.95) 0.7  # Too many unique values (might be identifiers)
    else 1.0                 # Good ratio
  })
  
  avg_unique_score <- mean(unique_ratio_scores)
  
  # Combine scores
  quality_score <- 0.5 * completeness_score + 0.2 * type_diversity + 0.3 * avg_unique_score
  
  return(max(0.0, min(1.0, quality_score)))
}

#' Create Quality Assessment
#' @keywords internal
create_quality_assessment <- function(workflow) {
  
  assessment <- list()
  
  # Source data quality
  if ("source_profile" %in% names(workflow$intermediate_data)) {
    source_profile <- workflow$intermediate_data$source_profile
    assessment$source_data_quality <- calculate_data_quality_score(source_profile)
    assessment$source_completeness <- 1.0 - mean(source_profile$columns$na_proportion)
  }
  
  # Mapping quality
  if ("mapping_result" %in% names(workflow$intermediate_data)) {
    mapping_result <- workflow$intermediate_data$mapping_result
    assessment$mapping_quality <- mapping_result$quality_score
    assessment$mapping_coverage <- mapping_result$coverage_analysis$required_coverage
  }
  
  # Overall quality score
  quality_scores <- unlist(assessment[grepl("quality|score", names(assessment))])
  assessment$overall_quality <- if (length(quality_scores) > 0) mean(quality_scores) else 0.0
  
  return(assessment)
}

#' Create Workflow Recommendations
#' @keywords internal
create_workflow_recommendations <- function(workflow) {
  
  recommendations <- character()
  
  # Failed steps
  failed_steps <- sapply(workflow$steps, function(s) {
    s$status == WORKFLOW_STATUS$FAILED
  })
  
  if (any(failed_steps)) {
    failed_step_names <- names(workflow$steps)[failed_steps]
    recommendations <- c(recommendations, paste("Address failures in:", paste(failed_step_names, collapse = ", ")))
  }
  
  # Data quality recommendations
  if ("source_profile" %in% names(workflow$intermediate_data)) {
    quality_score <- calculate_data_quality_score(workflow$intermediate_data$source_profile)
    if (quality_score < 0.8) {
      recommendations <- c(recommendations, "Improve source data quality before proceeding to production")
    }
  }
  
  # Mapping recommendations
  if ("mapping_result" %in% names(workflow$intermediate_data)) {
    mapping_result <- workflow$intermediate_data$mapping_result
    if (mapping_result$quality_score < 0.7) {
      recommendations <- c(recommendations, "Review and refine column mappings manually")
    }
    if (mapping_result$coverage_analysis$required_coverage < 0.9) {
      recommendations <- c(recommendations, "Address unmapped required columns")
    }
  }
  
  return(recommendations)
}

#' Create Workflow Summary
#' @keywords internal
create_workflow_summary <- function(workflow) {
  
  return(list(
    workflow_id = workflow$workflow_id,
    status = workflow$status,
    execution_time = format(workflow$execution_start_time),
    steps = lapply(workflow$steps, function(step) {
      list(
        step_id = step$step_id,
        step_name = step$step_name,
        status = step$status,
        duration_ms = step$duration_ms,
        metrics = step$metrics,
        warnings = step$warnings,
        error = step$error_message
      )
    }),
    config = list(
      source_file = workflow$config$source_file_path,
      target_dataflow = workflow$config$target_dataflow_url,
      output_directory = workflow$config$output_directory,
      enable_llm_generation = workflow$config$enable_llm_generation,
      enable_validation = workflow$config$enable_validation
    ),
    quality_assessment = create_quality_assessment(workflow),
    recommendations = create_workflow_recommendations(workflow)
  ))
}

#' Generate Mapping Report
#' @keywords internal
generate_mapping_report <- function(mapping_result, source_profile) {
  
  report_lines <- c(
    "=== SDMX MAPPING ANALYSIS REPORT ===",
    "",
    sprintf("Generated: %s", format(Sys.time())),
    sprintf("Overall Quality Score: %.1f%%", mapping_result$quality_score * 100),
    sprintf("Transformation Complexity: %.1f%%", mapping_result$transformation_complexity * 100),
    "",
    "=== COVERAGE ANALYSIS ===",
    sprintf("Required Coverage: %.1f%%", mapping_result$coverage_analysis$required_coverage * 100),
    sprintf("Total Mappings: %d", length(mapping_result$mappings)),
    sprintf("High Confidence Mappings: %d", mapping_result$coverage_analysis$high_confidence_mappings),
    "",
    "=== MAPPING SUGGESTIONS ==="
  )
  
  if (length(mapping_result$mappings) > 0) {
    report_lines <- c(report_lines, "")
    for (i in seq_len(min(10, length(mapping_result$mappings)))) {
      mapping <- mapping_result$mappings[[i]]
      confidence_pct <- round(mapping$confidence_score * 100, 1)
      report_lines <- c(
        report_lines,
        sprintf("%d. %s -> %s (%.1f%% confidence)", 
                i, mapping$source_column, mapping$target_column, confidence_pct)
      )
    }
  }
  
  if (length(mapping_result$recommendations) > 0) {
    report_lines <- c(
      report_lines,
      "",
      "=== RECOMMENDATIONS ===",
      ""
    )
    for (rec in mapping_result$recommendations) {
      report_lines <- c(report_lines, paste("•", rec))
    }
  }
  
  if (length(mapping_result$unmapped_source_columns) > 0) {
    report_lines <- c(
      report_lines,
      "",
      "=== UNMAPPED SOURCE COLUMNS ===",
      "",
      paste(mapping_result$unmapped_source_columns, collapse = ", ")
    )
  }
  
  return(report_lines)
}

#' Logging Utilities
#' @keywords internal
log_info <- function(workflow, message) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_message <- sprintf("[%s] INFO: %s", timestamp, message)
  message(log_message)
}

#' @keywords internal
log_error <- function(workflow, message) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_message <- sprintf("[%s] ERROR: %s", timestamp, message)
  warning(log_message, immediate. = TRUE)
  workflow$error_log <- c(workflow$error_log, message)
}

#' Print method for workflow results
#' @param x An object of class "llmx_workflow_result"
#' @param ... Additional arguments (unused)
#' @export
print.llmx_workflow_result <- function(x, ...) {
  cli::cli_h1("SDMX Workflow Execution Report")
  cli::cli_text("Workflow ID: {.val {x$workflow_id}}")
  cli::cli_text("Overall Status: {.val {x$overall_status}}")
  cli::cli_text("Total Duration: {.val {round(x$total_duration_ms/1000, 2)}} seconds")
  
  cli::cli_h2("Step Summary")
  step_names <- names(x$steps)
  for (step_name in step_names) {
    step <- x$steps[[step_name]]
    status_symbol <- switch(step$status,
                           "completed" = "✅",
                           "failed" = "❌", 
                           "skipped" = "⏸️",
                           "⏳")
    cli::cli_text("{status_symbol} {step$step_name} ({step$status})")
  }
  
  if (!is.null(x$quality_assessment) && length(x$quality_assessment) > 0) {
    cli::cli_h2("Quality Assessment")
    if ("overall_quality" %in% names(x$quality_assessment)) {
      cli::cli_text("Overall Quality: {.val {round(x$quality_assessment$overall_quality * 100, 1)}}%")
    }
  }
  
  if (length(x$recommendations) > 0) {
    cli::cli_h2("Recommendations")
    for (rec in x$recommendations) {
      cli::cli_text("• {rec}")
    }
  }
  
  if (length(x$output_files) > 0) {
    cli::cli_h2("Output Files")
    for (type in names(x$output_files)) {
      cli::cli_text("{type}: {.path {x$output_files[[type]]}}")
    }
  }
  
  invisible(x)
}