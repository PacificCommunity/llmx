# Basic SDMX Data Mapping Template
# This template provides a starting point for transforming your data to SDMX-CSV format

# Load required libraries
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(cli)

#' Transform data to SDMX-CSV format
#'
#' This function transforms source data to match SDMX-CSV specifications.
#' Customize the mapping logic below based on your specific data structure.
#'
#' @param source_data Data frame containing the source data
#' @return Data frame in SDMX-CSV format
transform_to_sdmx_csv <- function(source_data) {
  
  # Step 1: Validate input data
  cli_inform("Starting SDMX transformation...")
  
  if (!is.data.frame(source_data)) {
    cli_abort("source_data must be a data frame")
  }
  
  if (nrow(source_data) == 0) {
    cli_abort("source_data is empty")
  }
  
  # Step 2: Data cleaning and standardization
  cli_inform("Cleaning and standardizing data...")
  
  cleaned_data <- source_data |>
    # Remove empty rows
    filter(if_any(everything(), ~ !is.na(.x) & .x != "")) |>
    # Standardize text columns
    mutate(across(where(is.character), ~ str_trim(str_to_upper(.x))))
  
  # Step 3: Column mapping
  # *** CUSTOMIZE THIS SECTION FOR YOUR DATA ***
  cli_inform("Mapping columns to SDMX structure...")
  
  mapped_data <- cleaned_data |>
    # Example mappings - replace with your actual column mappings
    select(
      # Dimensions (required)
      REF_AREA = country,           # Geographic area/country
      TIME_PERIOD = year,           # Time period
      INDICATOR = indicator_code,   # Statistical indicator
      
      # Measures (required) 
      OBS_VALUE = value,            # Observation value
      
      # Attributes (optional)
      UNIT_MEASURE = unit,          # Unit of measurement
      OBS_STATUS = status           # Observation status
    )
  
  # Step 4: Data type conversions
  cli_inform("Converting data types...")
  
  typed_data <- mapped_data |>
    mutate(
      # Ensure dimensions are character
      REF_AREA = as.character(REF_AREA),
      TIME_PERIOD = as.character(TIME_PERIOD), 
      INDICATOR = as.character(INDICATOR),
      
      # Ensure measures are numeric
      OBS_VALUE = as.numeric(OBS_VALUE),
      
      # Handle attributes
      UNIT_MEASURE = as.character(UNIT_MEASURE),
      OBS_STATUS = as.character(OBS_STATUS)
    )
  
  # Step 5: Data validation
  cli_inform("Validating transformed data...")
  
  # Check for required columns
  required_cols <- c("REF_AREA", "TIME_PERIOD", "INDICATOR", "OBS_VALUE")
  missing_cols <- setdiff(required_cols, names(typed_data))
  
  if (length(missing_cols) > 0) {
    cli_abort("Missing required SDMX columns: {paste(missing_cols, collapse = ', ')}")
  }
  
  # Check for missing values in required dimensions
  dimension_cols <- c("REF_AREA", "TIME_PERIOD", "INDICATOR")
  for (col in dimension_cols) {
    na_count <- sum(is.na(typed_data[[col]]))
    if (na_count > 0) {
      cli_warn("Column {col} has {na_count} missing values")
    }
  }
  
  # Check numeric measures
  if (!is.numeric(typed_data$OBS_VALUE)) {
    cli_abort("OBS_VALUE must be numeric")
  }
  
  # Step 6: Final formatting
  cli_inform("Applying final formatting...")
  
  final_data <- typed_data |>
    # Remove rows with missing observation values (optional)
    filter(!is.na(OBS_VALUE)) |>
    # Sort by dimensions for consistency
    arrange(REF_AREA, TIME_PERIOD, INDICATOR) |>
    # Select only necessary columns in SDMX order
    select(all_of(required_cols), everything())
  
  cli_inform("✓ Transformation completed successfully")
  cli_inform("Final data: {nrow(final_data)} rows × {ncol(final_data)} columns")
  
  return(final_data)
}

# Example usage:
# source_data <- read_csv("your_data.csv")
# sdmx_data <- transform_to_sdmx_csv(source_data)
# write_csv(sdmx_data, "output_sdmx.csv")

# Common SDMX dimension examples:
# - REF_AREA: Country/geographic area codes (e.g., "USA", "CAN", "MEX")
# - TIME_PERIOD: Time periods (e.g., "2021", "2021-Q1", "2021-M01")  
# - INDICATOR: Statistical indicators (e.g., "GDP", "POP", "CPI")
# - FREQ: Frequency (e.g., "A" for annual, "Q" for quarterly, "M" for monthly)

# Common SDMX attributes examples:
# - UNIT_MEASURE: Units (e.g., "USD", "PERSONS", "PERCENT")
# - OBS_STATUS: Status codes (e.g., "A" for normal, "E" for estimated)
# - DECIMALS: Number of decimal places
# - SOURCE_AGENCY: Data source agency