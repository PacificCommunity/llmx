test_that("validate_sdmx_csv identifies missing columns", {
  # Create mock SDMX metadata
  mock_sdmx <- structure(
    list(
      dsd_id = "TEST_DSD",
      agency_id = "TEST",
      dimensions = data.frame(
        id = c("REF_AREA", "TIME_PERIOD", "INDICATOR"),
        concept_ref = c("REF_AREA", "TIME_PERIOD", "INDICATOR"),
        codelist = c("CL_AREA", NA, "CL_INDICATOR"),
        position = 1:3
      ),
      attributes = data.frame(
        id = "UNIT_MEASURE",
        concept_ref = "UNIT_MEASURE", 
        attachment_level = "DataSet",
        codelist = "CL_UNIT"
      ),
      measures = data.frame(
        id = "OBS_VALUE",
        concept_ref = "OBS_VALUE"
      )
    ),
    class = "llmx_sdmx_metadata"
  )
  
  # Test data missing required columns
  incomplete_data <- data.frame(
    REF_AREA = c("USA", "CAN"),
    TIME_PERIOD = c("2021", "2021")
    # Missing INDICATOR and OBS_VALUE
  )
  
  expect_error(
    validate_sdmx_csv(incomplete_data, mock_sdmx),
    "Missing required columns"
  )
  
  # Test with detailed validation
  result <- validate_sdmx_csv(incomplete_data, mock_sdmx, return_details = TRUE)
  expect_s3_class(result, "llmx_validation_result")
  expect_false(result$is_valid)
  expect_true(length(result$issues) > 0)
})

test_that("validate_sdmx_csv passes with complete data", {
  # Create mock SDMX metadata
  mock_sdmx <- structure(
    list(
      dsd_id = "TEST_DSD",
      agency_id = "TEST", 
      dimensions = data.frame(
        id = c("REF_AREA", "TIME_PERIOD"),
        concept_ref = c("REF_AREA", "TIME_PERIOD"),
        codelist = c("CL_AREA", NA),
        position = 1:2
      ),
      attributes = data.frame(
        id = character(),
        concept_ref = character(),
        attachment_level = character(),
        codelist = character()
      ),
      measures = data.frame(
        id = "OBS_VALUE",
        concept_ref = "OBS_VALUE"
      )
    ),
    class = "llmx_sdmx_metadata"
  )
  
  # Complete valid data
  valid_data <- data.frame(
    REF_AREA = c("USA", "CAN", "MEX"),
    TIME_PERIOD = c("2021", "2021", "2021"),
    OBS_VALUE = c(100.5, 200.3, 150.7)
  )
  
  expect_true(validate_sdmx_csv(valid_data, mock_sdmx))
  
  # Test detailed validation
  result <- validate_sdmx_csv(valid_data, mock_sdmx, return_details = TRUE)
  expect_true(result$is_valid)
  expect_equal(length(result$issues), 0)
})

test_that("suggest_sdmx_mapping generates reasonable suggestions", {
  # Mock data analysis
  mock_analysis <- structure(
    list(
      columns = data.frame(
        column = c("country", "year", "gdp_value", "currency"),
        type = c("character", "integer", "numeric", "character"),
        unique_count = c(3, 3, 3, 3),
        is_categorical = c(TRUE, FALSE, FALSE, TRUE),
        is_numeric = c(FALSE, TRUE, TRUE, FALSE)
      )
    ),
    class = "llmx_data_analysis"
  )
  
  # Mock SDMX metadata
  mock_sdmx <- structure(
    list(
      dimensions = data.frame(
        id = c("REF_AREA", "TIME_PERIOD", "INDICATOR"),
        concept_ref = c("REF_AREA", "TIME_PERIOD", "INDICATOR"),
        component_type = rep("dimension", 3)
      ),
      attributes = data.frame(
        id = "UNIT_MEASURE",
        concept_ref = "UNIT_MEASURE",
        component_type = "attribute"
      ),
      measures = data.frame(
        id = "OBS_VALUE", 
        concept_ref = "OBS_VALUE",
        component_type = "measure"
      )
    ),
    class = "llmx_sdmx_metadata"
  )
  
  result <- suggest_sdmx_mapping(mock_analysis, mock_sdmx, similarity_threshold = 0.1)
  
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true("source_column" %in% names(result))
  expect_true("target_id" %in% names(result))
  expect_true("overall_score" %in% names(result))
  
  # Should suggest country -> REF_AREA mapping
  country_suggestions <- result[result$source_column == "country", ]
  expect_true(nrow(country_suggestions) > 0)
})

test_that("print.llmx_validation_result works", {
  mock_result <- structure(
    list(
      is_valid = FALSE,
      has_warnings = TRUE,
      issues = list(
        list(type = "missing_columns", message = "Missing column X", severity = "error")
      ),
      warnings = list(
        list(type = "extra_columns", message = "Extra column Y", severity = "warning")
      ),
      validated_at = Sys.time(),
      n_rows = 100,
      n_cols = 5
    ),
    class = "llmx_validation_result"
  )
  
  # Test that print doesn't error and returns invisibly
  expect_no_error(print(mock_result))
  expect_invisible(print(mock_result))
})