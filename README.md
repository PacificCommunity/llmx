# llmx: LLM-Powered SDMX Data Transformation

[![R-CMD-check](https://github.com/PacificCommunity/llmx/workflows/R-CMD-check/badge.svg)](https://github.com/pacificcommunit/llmx/actions)
[![Codecov test coverage](https://codecov.io/gh/PacificCommunity/llmx/branch/main/graph/badge.svg)](https://codecov.io/gh/pacificcommunit/llmx?branch=main)

A modern R package for transforming diverse data sources (CSV, Excel, etc.) into SDMX-CSV format using Large Language Models (LLMs). Provides intelligent mapping suggestions, automated script generation, and comprehensive validation for statistical data exchange workflows.

## Features

- 🤖 **LLM-powered script generation** - Automatically generate R code for data mapping using GPT models
- 📊 **Smart data analysis** - Analyze source data structure and suggest SDMX mappings
- ✅ **Comprehensive validation** - Validate data against SDMX-CSV specifications
- 📚 **Template library** - Ready-to-use templates for common transformation patterns
- 🔧 **Modern R practices** - Built with tidyverse, native pipe operator, and 2025 best practices

## Installation

```r
# Install from GitHub (development version)
# install.packages("devtools")
devtools::install_github("PacificCommunity/llmx")

# Or install from CRAN (when available)
install.packages("llmx")
```

## Prerequisites

1. **LLM Provider**: The package supports multiple LLM providers:
   - **Ollama** (recommended fallback): Install locally for free usage
   - **OpenAI**: Set API key for cloud-based models
   - **Anthropic Claude**: Alternative cloud provider
   - **Other providers**: See configuration section
   
   ```r
   # For OpenAI (optional)
   Sys.setenv(OPENAI_API_KEY = "your-api-key-here")
   
   # For Ollama (free, local fallback)
   # Install Ollama from https://ollama.ai and run: ollama serve
   ```

2. **Required packages**: The package will prompt you to install missing dependencies.

## Quick Start

### 1. Analyze Your Data

```r
library(llmx)

# Analyze the structure of your source data
analysis <- analyze_data_structure("my_data.csv")
print(analysis)
```

### 2. Get SDMX Structure Information

```r
# Extract metadata from an SDMX endpoint
sdmx_meta <- extract_dsd_metadata(
  "https://stats.oecd.org/restsdmx/sdmx.ashx/GetDataStructure/QNA"
)
print(sdmx_meta$dimensions)
```

### 3. Generate Mapping Suggestions

```r
# Get intelligent mapping suggestions
suggestions <- suggest_sdmx_mapping(analysis, sdmx_meta)
print(suggestions)
```

### 4. Generate Transformation Script

```r
# Let the LLM generate a complete transformation script
script <- generate_mapping_script(
  data_analysis = analysis,
  target_sdmx = sdmx_meta,
  output_file = "transform_script.R"
)

# Review and customize the generated script
cat(script)
```

### 5. Validate Results

```r
# Load and transform your data using the generated script
source("transform_script.R")
source_data <- readr::read_csv("my_data.csv")
sdmx_data <- transform_to_sdmx_csv(source_data)

# Validate the transformed data
validate_sdmx_csv(sdmx_data, sdmx_meta)
```

## Core Functions

| Function | Purpose |
|----------|---------|
| `analyze_data_structure()` | Analyze source data format and structure |
| `extract_dsd_metadata()` | Extract SDMX Data Structure Definition metadata |
| `suggest_sdmx_mapping()` | Generate intelligent mapping suggestions |
| `generate_mapping_script()` | Create LLM-generated transformation code |
| `generate_codelist_value_mapping()` | Generate code to map data values to codelist codes |
| `validate_sdmx_csv()` | Validate data against SDMX specifications |

## Templates

The package includes ready-to-use templates for common scenarios:

- **Basic mapping**: Simple 1:1 column transformations
- **Wide-to-long**: Reshape data from wide to long format
- **Complex transformations**: Multi-step data cleaning and mapping

Access templates:
```r
# List available templates
fs::dir_ls(system.file("templates", package = "llmx"))

# Copy a template to your working directory
file.copy(
  system.file("templates/basic_mapping_template.R", package = "llmx"),
  "my_mapping.R"
)
```

## Examples

### Transform GDP data to SDMX format

```r
# Analyze source GDP data
gdp_analysis <- analyze_data_structure("gdp_data.csv")

# Get OECD QNA structure
oecd_qna <- extract_dsd_metadata(
  "https://stats.oecd.org/restsdmx/sdmx.ashx/GetDataStructure/QNA"
)

# Generate transformation script
transform_script <- generate_mapping_script(
  gdp_analysis, 
  oecd_qna,
  mapping_type = "complex"
)

# Execute transformation
source_data <- readr::read_csv("gdp_data.csv")
eval(parse(text = transform_script))
sdmx_data <- transform_to_sdmx_csv(source_data)

# Validate result
validation <- validate_sdmx_csv(sdmx_data, oecd_qna, return_details = TRUE)
print(validation)
```

### Handle wide-format data

```r
# Use the wide-to-long template for data with multiple indicator columns
source(system.file("templates/wide_to_long_template.R", package = "llmx"))

wide_data <- data.frame(
  country = c("USA", "CAN", "MEX"),
  year = c(2021, 2021, 2021),
  gdp = c(23315.08, 1988.34, 1293.84),
  population = c(331900000, 38000000, 128900000)
)

sdmx_long <- transform_wide_to_sdmx_csv(
  wide_data,
  id_cols = c("country", "year"),
  value_cols = c("gdp", "population"),
  indicator_names = c("gdp" = "GDP", "population" = "POP")
)
```

### Map data values to codelist codes

```r
# Generate mapping script for specific value transformations
source_values <- c("United States", "Canada", "Mexico", "United Kingdom")
country_codelist <- data.frame(
  code_id = c("US", "CA", "MX", "GB"),
  name = c("United States", "Canada", "Mexico", "United Kingdom")
)

config <- create_llm_config("ollama", "llama2")  # Using Ollama fallback
mapping_script <- generate_codelist_value_mapping(
  source_values = source_values,
  target_codelist = country_codelist,
  source_column_name = "country_name",
  target_codelist_name = "ISO_COUNTRY_CODES",
  llm_config = config
)

# Review and execute the generated mapping function
cat(mapping_script)
eval(parse(text = mapping_script))

# Use the generated function
mapped_codes <- map_values_to_codelist(c("United States", "Canada"))
print(mapped_codes)  # Should return c("US", "CA")
```

## Configuration

### LLM Settings

```r
# Use different providers and models
script <- generate_mapping_script(
  analysis, 
  sdmx_meta,
  provider = "ollama",       # Use local Ollama (fallback)
  model = "llama2",          # Free local model
  mapping_type = "simple",   # For straightforward mappings
  include_validation = TRUE  # Include data validation code
)

# Or use cloud providers
script <- generate_mapping_script(
  analysis, 
  sdmx_meta,
  provider = "openai",       # Cloud option
  model = "gpt-3.5-turbo",   # Faster, cheaper cloud option
  mapping_type = "complex"   # For advanced transformations
)
```

### Validation Options

```r
# Strict validation (default)
validate_sdmx_csv(data, sdmx_meta, strict = TRUE)

# Permissive validation
validate_sdmx_csv(data, sdmx_meta, strict = FALSE)

# Detailed validation results
result <- validate_sdmx_csv(data, sdmx_meta, return_details = TRUE)
print(result$issues)
```

## Contributing

We welcome contributions! Please see [CONTRIBUTING.md](CONTRIBUTING.md) for details.

1. Fork the repository
2. Create a feature branch
3. Add tests for new functionality  
4. Ensure all tests pass: `devtools::test()`
5. Submit a pull request

## License

MIT License. See [LICENSE](LICENSE) for details.

## Citation

```r
citation("llmx")
```

## Related Packages

- [rsdmx](https://github.com/opensdmx/rsdmx) - SDMX data reading
- [ellmer](https://github.com/conchaedler/ellmer) - LLM integration for R
- [tidyverse](https://tidyverse.org/) - Modern data science toolkit
