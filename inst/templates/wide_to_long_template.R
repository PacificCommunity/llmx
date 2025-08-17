# Wide-to-Long SDMX Data Transformation Template
# This template handles data that needs to be reshaped from wide to long format

library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(cli)

#' Transform wide-format data to SDMX-CSV long format
#'
#' This function handles the common case where source data is in wide format
#' (multiple indicator columns) and needs to be transformed to SDMX long format.
#'
#' @param source_data Data frame in wide format
#' @param id_cols Character vector of column names that identify observations
#' @param value_cols Character vector of column names containing indicator values  
#' @param indicator_names Named character vector mapping value_cols to indicator codes
#' @return Data frame in SDMX-CSV long format
transform_wide_to_sdmx_csv <- function(source_data, 
                                      id_cols = c("country", "year"),
                                      value_cols = NULL,
                                      indicator_names = NULL) {
  
  # Step 1: Input validation
  cli_inform("Starting wide-to-long SDMX transformation...")
  
  if (!is.data.frame(source_data)) {
    cli_abort("source_data must be a data frame")
  }
  
  if (nrow(source_data) == 0) {
    cli_abort("source_data is empty")
  }
  
  # Auto-detect value columns if not specified
  if (is.null(value_cols)) {
    value_cols <- names(source_data)[sapply(source_data, is.numeric)]
    value_cols <- setdiff(value_cols, id_cols)
    cli_inform("Auto-detected value columns: {paste(value_cols, collapse = ', ')}")
  }
  
  # Auto-generate indicator names if not specified
  if (is.null(indicator_names)) {
    indicator_names <- setNames(str_to_upper(value_cols), value_cols)
    cli_inform("Using auto-generated indicator codes")
  }
  
  # Step 2: Reshape from wide to long
  cli_inform("Reshaping data from wide to long format...")
  
  long_data <- source_data |>
    # Ensure ID columns are present
    {
      missing_ids <- setdiff(id_cols, names(.))
      if (length(missing_ids) > 0) {
        cli_abort("Missing ID columns: {paste(missing_ids, collapse = ', ')}")
      }
      .
    } |>
    # Pivot to long format
    pivot_longer(
      cols = all_of(value_cols),
      names_to = "source_indicator", 
      values_to = "value",
      values_drop_na = TRUE  # Remove missing values
    ) |>
    # Map to indicator codes
    mutate(
      INDICATOR = recode(source_indicator, !!!indicator_names)
    )
  
  # Step 3: Map to SDMX structure
  cli_inform("Mapping to SDMX column structure...")
  
  # *** CUSTOMIZE THESE MAPPINGS FOR YOUR DATA ***
  sdmx_data <- long_data |>
    mutate(
      # Map ID columns to SDMX dimensions
      REF_AREA = case_when(
        # Example country code mappings - customize as needed
        str_detect(str_to_upper(!!sym(id_cols[1])), "USA|UNITED.STATES") ~ "USA",
        str_detect(str_to_upper(!!sym(id_cols[1])), "CAN|CANADA") ~ "CAN", 
        str_detect(str_to_upper(!!sym(id_cols[1])), "MEX|MEXICO") ~ "MEX",
        TRUE ~ str_to_upper(!!sym(id_cols[1]))  # Default: use as-is
      ),
      
      # Map time column
      TIME_PERIOD = if (length(id_cols) >= 2) {
        as.character(!!sym(id_cols[2]))
      } else {
        "2021"  # Default value if no time column
      },
      
      # Use mapped indicator codes
      INDICATOR = INDICATOR,
      
      # Map values
      OBS_VALUE = as.numeric(value),
      
      # Add default attributes
      UNIT_MEASURE = case_when(
        str_detect(INDICATOR, "GDP|VALUE") ~ "USD_MILLIONS",
        str_detect(INDICATOR, "POP|POPULATION") ~ "PERSONS",
        str_detect(INDICATOR, "RATE|PERCENT") ~ "PERCENT",
        TRUE ~ "NUMBER"  # Default unit
      ),
      
      OBS_STATUS = if_else(is.na(OBS_VALUE), "M", "A")  # M=Missing, A=Available
    )
  
  # Step 4: Data cleaning and validation
  cli_inform("Cleaning and validating data...")
  
  final_data <- sdmx_data |>
    # Remove rows with missing observation values
    filter(!is.na(OBS_VALUE), is.finite(OBS_VALUE)) |>
    # Standardize text fields
    mutate(
      REF_AREA = str_trim(str_to_upper(REF_AREA)),
      TIME_PERIOD = str_trim(as.character(TIME_PERIOD)),
      INDICATOR = str_trim(str_to_upper(INDICATOR)),
      UNIT_MEASURE = str_trim(str_to_upper(UNIT_MEASURE))
    ) |>
    # Select final columns in SDMX order
    select(REF_AREA, TIME_PERIOD, INDICATOR, OBS_VALUE, UNIT_MEASURE, OBS_STATUS) |>
    # Sort for consistency
    arrange(REF_AREA, TIME_PERIOD, INDICATOR)
  
  # Step 5: Final validation
  required_cols <- c("REF_AREA", "TIME_PERIOD", "INDICATOR", "OBS_VALUE")
  missing_cols <- setdiff(required_cols, names(final_data))
  
  if (length(missing_cols) > 0) {
    cli_abort("Missing required SDMX columns: {paste(missing_cols, collapse = ', ')}")
  }
  
  # Check data quality
  n_missing_area <- sum(is.na(final_data$REF_AREA) | final_data$REF_AREA == "")
  n_missing_time <- sum(is.na(final_data$TIME_PERIOD) | final_data$TIME_PERIOD == "")
  n_missing_indicator <- sum(is.na(final_data$INDICATOR) | final_data$INDICATOR == "")
  
  if (n_missing_area > 0) cli_warn("Missing REF_AREA values: {n_missing_area}")
  if (n_missing_time > 0) cli_warn("Missing TIME_PERIOD values: {n_missing_time}")  
  if (n_missing_indicator > 0) cli_warn("Missing INDICATOR values: {n_missing_indicator}")
  
  cli_inform("✓ Wide-to-long transformation completed successfully")
  cli_inform("Original: {nrow(source_data)} rows × {ncol(source_data)} columns")
  cli_inform("Final: {nrow(final_data)} rows × {ncol(final_data)} columns")
  cli_inform("Indicators transformed: {length(unique(final_data$INDICATOR))}")
  
  return(final_data)
}

# Example usage with sample data structure:
#
# Wide format input:
# | country | year | gdp | population | unemployment_rate |
# |---------|------|-----|------------|------------------|
# | USA     | 2021 | 23315.08 | 331900000 | 5.3 |
# | Canada  | 2021 | 1988.34  | 38000000  | 7.5 |
#
# SDMX long format output:
# | REF_AREA | TIME_PERIOD | INDICATOR | OBS_VALUE | UNIT_MEASURE | OBS_STATUS |
# |----------|-------------|-----------|-----------|--------------|------------|
# | USA      | 2021        | GDP       | 23315.08  | USD_MILLIONS | A          |
# | USA      | 2021        | POP       | 331900000 | PERSONS      | A          |
# | USA      | 2021        | UNEMP     | 5.3       | PERCENT      | A          |

# Usage example:
# source_data <- read_csv("wide_format_data.csv")
# 
# # Define mappings
# id_columns <- c("country", "year")
# value_columns <- c("gdp", "population", "unemployment_rate")
# indicator_mapping <- c(
#   "gdp" = "GDP", 
#   "population" = "POP",
#   "unemployment_rate" = "UNEMP_RATE"
# )
# 
# # Transform
# sdmx_data <- transform_wide_to_sdmx_csv(
#   source_data,
#   id_cols = id_columns,
#   value_cols = value_columns, 
#   indicator_names = indicator_mapping
# )
# 
# # Save result
# write_csv(sdmx_data, "output_sdmx_long.csv")