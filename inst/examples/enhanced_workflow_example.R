# Enhanced SDMX Workflow Example
# Demonstrating the full capabilities of the enhanced llmx package

library(llmx)

# =============================================================================
# EXAMPLE 1: Complete Workflow with Multiple LLM Providers
# =============================================================================

# Create sample data
sample_data <- data.frame(
  country = c("USA", "Canada", "Mexico", "Germany", "France"),
  year = c(2020, 2020, 2020, 2020, 2020),
  gdp_value = c(21400, 1700, 1300, 3800, 2600),
  population = c(331, 38, 128, 83, 67),
  category = c("Developed", "Developed", "Developing", "Developed", "Developed"),
  stringsAsFactors = FALSE
)

# Save sample data
write.csv(sample_data, "sample_economic_data.csv", row.names = FALSE)

# 1. Enhanced Data Analysis with Profiling
cat("=== Enhanced Data Analysis ===\n")
data_analysis <- analyze_data_structure(
  "sample_economic_data.csv",
  detect_hierarchies = TRUE,
  detect_temporal_patterns = TRUE,
  quality_assessment = TRUE
)

print(data_analysis)

# 2. SDMX Schema Analysis
cat("\n=== SDMX Schema Analysis ===\n")
# Note: Using a mock OECD QNA URL - replace with actual DSD URL
target_dsd_url <- "https://stats.oecd.org/restsdmx/sdmx.ashx/GetDataStructure/QNA"

tryCatch({
  target_schema <- extract_dsd_metadata(target_dsd_url)
  print(target_schema)
}, error = function(e) {
  cat("Note: Using mock schema for example (replace with actual DSD URL)\n")
  
  # Create mock target schema for demonstration
  target_schema <- structure(list(
    dsd_url = target_dsd_url,
    dsd_id = "QNA", 
    agency_id = "OECD",
    version = "1.0",
    dimensions = data.frame(
      id = c("LOCATION", "SUBJECT", "MEASURE", "FREQUENCY"),
      concept_ref = c("REF_AREA", "INDICATOR", "UNIT_MEASURE", "FREQ"),
      codelist = c("CL_AREA", "CL_INDICATOR", "CL_UNIT", "CL_FREQ"),
      position = 1:4,
      stringsAsFactors = FALSE
    ),
    attributes = data.frame(
      id = c("OBS_STATUS", "UNIT_MULT"),
      concept_ref = c("OBS_STATUS", "UNIT_MULT"), 
      attachment_level = c("O", "S"),
      codelist = c("CL_OBS_STATUS", "CL_UNIT_MULT"),
      stringsAsFactors = FALSE
    ),
    measures = data.frame(
      id = "OBS_VALUE",
      concept_ref = "OBS_VALUE",
      stringsAsFactors = FALSE
    ),
    extracted_at = Sys.time()
  ), class = "llmx_sdmx_metadata")
})

# 3. Advanced Mapping Inference
cat("\n=== Advanced Mapping Inference ===\n")
inference_engine <- create_inference_engine(
  fuzzy_threshold = 0.6,
  min_confidence = 0.3,
  use_statistical_analysis = TRUE,
  enable_learning = TRUE
)

# Read the sample data for mapping analysis
source_data <- read.csv("sample_economic_data.csv", stringsAsFactors = FALSE)

mapping_result <- infer_advanced_mappings(
  inference_engine,
  data_analysis,
  target_schema,
  source_data
)

print(mapping_result)

# 4. Value Transformation Suggestions
cat("\n=== Value Transformation Analysis ===\n")
transformations <- generate_value_transformations(
  mapping_result,
  source_data,
  target_schema
)

print(transformations)

# =============================================================================
# EXAMPLE 2: Multiple LLM Provider Support
# =============================================================================

cat("\n=== Multiple LLM Provider Examples ===\n")

# OpenAI Configuration (requires API key)
if (Sys.getenv("OPENAI_API_KEY") != "") {
  cat("Testing OpenAI configuration...\n")
  openai_config <- create_llm_config(
    provider = "openai",
    model = "gpt-3.5-turbo",
    temperature = 0.1
  )
  
  # Test connection
  tryCatch({
    test_result <- test_llm_connection(openai_config)
    if (test_result) {
      # Generate enhanced script
      enhanced_script <- generate_mapping_script_enhanced(
        data_analysis,
        target_schema,
        openai_config,
        mapping_type = "auto"
      )
      
      cat("Enhanced script generated successfully!\n")
      cat("First 500 characters:\n")
      cat(substr(enhanced_script, 1, 500), "...\n")
    }
  }, error = function(e) {
    cat("OpenAI test failed:", e$message, "\n")
  })
} else {
  cat("OpenAI API key not found - set OPENAI_API_KEY environment variable\n")
}

# Ollama Configuration (for local LLM)
cat("\nTesting Ollama configuration...\n")
ollama_config <- create_llm_config(
  provider = "ollama",
  model = "llama2",
  base_url = "http://localhost:11434"
)

tryCatch({
  test_result <- test_llm_connection(ollama_config)
  if (test_result) {
    cat("Ollama connection successful!\n")
  }
}, error = function(e) {
  cat("Ollama test failed (likely not running locally):", e$message, "\n")
})

# =============================================================================
# EXAMPLE 3: Complete Workflow Orchestration
# =============================================================================

cat("\n=== Complete Workflow Orchestration ===\n")

# Create comprehensive workflow
workflow <- create_sdmx_workflow(
  source_file_path = "sample_economic_data.csv",
  target_dataflow_url = target_dsd_url,
  output_directory = "output",
  llm_provider = "openai",
  llm_model = "gpt-3.5-turbo",
  enable_llm_generation = FALSE,  # Disable for demo unless API key available
  enable_validation = TRUE,
  enable_advanced_mapping = TRUE
)

# Execute workflow with progress tracking
result <- execute_workflow(workflow, progress_callback = function(step, total, name, status) {
  cat(sprintf("Step %d/%d: %s - %s\n", step, total, name, status))
})

print(result)

# =============================================================================
# EXAMPLE 4: Learning from User Feedback
# =============================================================================

cat("\n=== Learning from User Feedback ===\n")

# Simulate user feedback
user_feedback <- list(
  accepted_mappings = list(
    list(source = "country", target = "LOCATION"),
    list(source = "year", target = "TIME_PERIOD"),
    list(source = "gdp_value", target = "OBS_VALUE")
  ),
  rejected_mappings = list(
    list(source = "population", target = "OBS_VALUE")
  )
)

# Update inference engine with feedback
learn_from_feedback(inference_engine, user_feedback)

# Re-run mapping with learned patterns
updated_mapping <- infer_advanced_mappings(
  inference_engine,
  data_analysis,
  target_schema,
  source_data
)

cat("Mapping results after learning:\n")
print(updated_mapping)

# =============================================================================
# EXAMPLE 5: Fuzzy String Matching Demonstration
# =============================================================================

cat("\n=== Fuzzy String Matching Examples ===\n")

# Test various string similarities
test_pairs <- list(
  c("country", "geo"),
  c("GDP_Annual", "gdp_yearly"),
  c("United States", "USA"),
  c("year", "time"),
  c("population_count", "pop_value")
)

for (pair in test_pairs) {
  score <- fuzzy_match_score(pair[1], pair[2])
  cat(sprintf("'%s' vs '%s': %.3f\n", pair[1], pair[2], score))
}

# =============================================================================
# EXAMPLE 6: Value Pattern Analysis
# =============================================================================

cat("\n=== Value Pattern Analysis Example ===\n")

# Create mock codelist for demonstration
mock_codelist <- data.frame(
  code_id = c("USA", "CAN", "MEX", "DEU", "FRA"),
  name = c("United States", "Canada", "Mexico", "Germany", "France"),
  stringsAsFactors = FALSE
)

# Analyze patterns in country column
country_values <- sample_data$country
pattern_analysis <- analyze_value_patterns(country_values, mock_codelist)

cat("Pattern analysis results:\n")
str(pattern_analysis)

# Clean up
file.remove("sample_economic_data.csv")

cat("\n=== Enhanced llmx Package Demonstration Complete! ===\n")
cat("The package now includes:\n")
cat("✓ Advanced mapping inference with fuzzy matching\n")
cat("✓ Comprehensive workflow orchestration\n") 
cat("✓ Multiple LLM provider support\n")
cat("✓ Enhanced data profiling and quality assessment\n")
cat("✓ Intelligent value transformation suggestions\n")
cat("✓ Learning capabilities from user feedback\n")
cat("✓ Temporal pattern detection and hierarchical relationship analysis\n")