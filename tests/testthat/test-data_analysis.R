test_that("analyze_data_structure works with CSV files", {
  # Create a temporary CSV file for testing
  temp_csv <- tempfile(fileext = ".csv")
  test_data <- data.frame(
    country = c("USA", "CAN", "MEX"),
    year = c(2021, 2021, 2021),
    gdp = c(23315.08, 1988.34, 1293.84),
    population = c(331900000, 38000000, 128900000),
    currency = c("USD", "CAD", "MXN")
  )
  readr::write_csv(test_data, temp_csv)
  
  # Test the function
  result <- analyze_data_structure(temp_csv)
  
  # Check structure
  expect_s3_class(result, "llmx_data_analysis")
  expect_equal(result$n_rows, 3)
  expect_equal(result$n_cols, 5)
  expect_equal(result$file_type, "csv")
  
  # Check columns analysis
  expect_s3_class(result$columns, "data.frame")
  expect_true("country" %in% result$columns$column)
  expect_true("year" %in% result$columns$column)
  
  # Check that categorical columns are identified
  country_row <- result$columns[result$columns$column == "country", ]
  expect_true(country_row$is_categorical)
  
  # Check that numeric columns are identified  
  gdp_row <- result$columns[result$columns$column == "gdp", ]
  expect_true(gdp_row$is_numeric)
  
  # Clean up
  unlink(temp_csv)
})

test_that("analyze_data_structure handles missing files gracefully", {
  expect_error(
    analyze_data_structure("nonexistent_file.csv"),
    "File not found"
  )
})

test_that("analyze_data_structure works with Excel files", {
  skip_if_not_installed("readxl")
  
  # Create temporary Excel file
  temp_xlsx <- tempfile(fileext = ".xlsx")
  test_data <- data.frame(
    indicator = c("GDP", "POP", "CPI"),
    value = c(1000, 50000, 102.5),
    unit = c("USD Million", "Persons", "Index")
  )
  
  # Write Excel file (requires openxlsx or similar)
  skip_if_not_installed("openxlsx")
  openxlsx::write.xlsx(test_data, temp_xlsx)
  
  result <- analyze_data_structure(temp_xlsx)
  
  expect_s3_class(result, "llmx_data_analysis")
  expect_equal(result$file_type, "xlsx")
  expect_equal(result$n_cols, 3)
  
  # Clean up
  unlink(temp_xlsx)
})

test_that("print.llmx_data_analysis works", {
  # Create mock analysis result
  mock_analysis <- structure(
    list(
      file_path = "test.csv",
      file_type = "csv",
      n_rows = 10,
      n_cols = 3,
      columns = data.frame(
        column = c("A", "B", "C"),
        type = c("character", "numeric", "character"),
        na_count = c(0, 1, 0),
        unique_count = c(3, 8, 2),
        na_proportion = c(0, 0.1, 0),
        is_categorical = c(TRUE, FALSE, TRUE),
        is_numeric = c(FALSE, TRUE, FALSE),
        is_date = c(FALSE, FALSE, FALSE)
      ),
      sample_data = data.frame(A = c("x", "y"), B = c(1, 2), C = c("a", "b")),
      summary_stats = data.frame(),
      unique_values = list(A = c("x", "y", "z"), C = c("a", "b"))
    ),
    class = "llmx_data_analysis"
  )
  
  # Test that print doesn't error and returns invisibly
  expect_no_error(print(mock_analysis))
  expect_invisible(print(mock_analysis))
})