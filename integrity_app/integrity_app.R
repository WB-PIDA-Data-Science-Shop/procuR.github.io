# ========================================================================
# PROCUREMENT INTEGRITY ANALYSIS - SHINY APPLICATION
# Simplified Enhanced Version (Avoids Stack Overflow)
# ========================================================================

# Increase expression limit to prevent node stack overflow
options(expressions = 10000)

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(data.table)
library(tidyverse)
library(patchwork)
library(corrr)
library(giscoR)
library(eurostat)
library(sf)
library(tidytext)
library(fixest)
library(ggeffects)
library(igraph)
library(ggraph)
library(purrr)
library(kableExtra)
library(ggrepel)
library(DT)
library(rmarkdown)
library(officer)
library(flextable)

# Source the utility functions
source("C:/Users/wb589991/OneDrive - WBG/Documents/procuR.github.io/src/utils/integrity_utils.R")

# ========================================================================
# INCREASE FILE UPLOAD LIMIT
# ========================================================================
# Default Shiny limit is 5MB - way too small for procurement data
# Set to 100MB (adjust as needed - use 500 for very large datasets)
options(shiny.maxRequestSize = 1000*1024^2)  # 100MB in bytes

# ========================================================================
# HELPER FUNCTIONS
# ========================================================================

# Filter data based on user selections
# Then update the filter_data function to use the divisor:

filter_data <- function(df, year_range = NULL, market = NULL, value_range = NULL, buyer_type = NULL, procedure_type = NULL, value_divisor = 1) {
  filtered_df <- df
  
  # Filter by year
  if (!is.null(year_range)) {
    year_col <- NULL
    if ("tender_year" %in% names(df)) {
      year_col <- "tender_year"
    } else if ("year" %in% names(df)) {
      year_col <- "year"
    } else if ("cal_year" %in% names(df)) {
      year_col <- "cal_year"
    }
    
    if (!is.null(year_col)) {
      filtered_df <- filtered_df %>%
        filter(.data[[year_col]] >= year_range[1] & .data[[year_col]] <= year_range[2])
    }
  }
  
  # Filter by market (CPV 2-digit)
  if (!is.null(market) && length(market) > 0 && "All" %ni% market && "lot_productcode" %in% names(df)) {
    filtered_df <- filtered_df %>%
      mutate(cpv_2dig = substr(lot_productcode, 1, 2)) %>%
      filter(cpv_2dig %in% market) %>%
      select(-cpv_2dig)
  }
  
  # Filter by contract value - SCALE BACK using divisor
  if (!is.null(value_range)) {
    price_col <- NULL
    if ("bid_priceusd" %in% names(df)) {
      price_col <- "bid_priceusd"
    } else if ("bid_price" %in% names(df)) {
      price_col <- "bid_price"
    }
    
    if (!is.null(price_col)) {
      # Scale the slider values back to actual prices
      actual_min <- value_range[1] * value_divisor
      actual_max <- value_range[2] * value_divisor
      
      filtered_df <- filtered_df %>%
        filter(!is.na(.data[[price_col]])) %>%
        filter(.data[[price_col]] >= actual_min & .data[[price_col]] <= actual_max)
    }
  }
  
  # Filter by buyer type
  if (!is.null(buyer_type) && length(buyer_type) > 0 && "All" %ni% buyer_type && "buyer_buyertype" %in% names(df)) {
    filtered_df <- filtered_df %>%
      mutate(buyer_group = add_buyer_group(buyer_buyertype))
    
    filtered_df <- filtered_df %>%
      filter(as.character(buyer_group) %in% buyer_type) %>%
      select(-buyer_group)
  }
  
  # Filter by procedure type - FIXED (no add_procedure_labels)
  if (!is.null(procedure_type) && length(procedure_type) > 0 && "All" %ni% procedure_type && "tender_proceduretype" %in% names(df)) {
    # Filter directly - no label conversion needed
    filtered_df <- filtered_df %>%
      filter(tender_proceduretype %in% procedure_type)
  }
  
  return(filtered_df)
}

# Helper operator
`%ni%` <- Negate(`%in%`)

# ========================================================================
# REPORT GENERATION FUNCTIONS
# ========================================================================

generate_pdf_report <- function(filtered_data, filtered_analysis, country_code, output_file, filters_text = "") {
  
  saveRDS(list(
    data = filtered_data,
    analysis = filtered_analysis,
    country_code = country_code
  ), file = file.path(tempdir(), "integrity_report_data.rds"))
  
  temp_rmd <- tempfile(fileext = ".Rmd")
  
  rmd_content <- '---
title: "Procurement Integrity Analysis Report"
output: pdf_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE, 
                      fig.width = 10, fig.height = 7)
library(ggplot2)
library(dplyr)

report_data <- readRDS(file.path(tempdir(), "integrity_report_data.rds"))
filtered_data <- report_data$data
filtered_analysis <- report_data$analysis
```

# Procurement Integrity Analysis Report

**Country:** `r report_data$country_code`

## Executive Summary

This report presents a comprehensive analysis of transparency and accountability in public procurement. Data quality represents a cross-cutting challenge in procurement transparency. Missing values can undermine the ability to detect corruption risks and assess market competition.

## Data Overview

- **Total Contracts:** `r format(nrow(filtered_data), big.mark = ",")`
- **Unique Buyers:** `r if("buyer_masterid" %in% names(filtered_data)) format(length(unique(filtered_data$buyer_masterid)), big.mark = ",") else "N/A"`
- **Unique Suppliers:** `r if("bidder_masterid" %in% names(filtered_data)) format(length(unique(filtered_data$bidder_masterid)), big.mark = ",") else "N/A"`
- **Years Covered:** `r if("tender_year" %in% names(filtered_data)) length(unique(filtered_data$tender_year[!is.na(filtered_data$tender_year)])) else "N/A"`

\\newpage

# Missing Values Analysis

## Are there observable patterns of underreporting information in the data?

The first step is to assess data completeness by examining missing values across all variables or a defined subset. Given the limited number of variables in the ProAct dataset, a full review can be efficiently conducted to identify any variables with significant underreporting.

### Overall Missing Values by Variable

This figure summarises the share of missing values for each key variable in the dataset. For every non-indicator column, the pipeline computes the proportion of records where that field is missing. Use this plot to identify which core fields are most affected by missingness and may compromise analysis—especially high-missingness identifiers, dates, and price variables.

```{r fig.height=8}
if (!is.null(filtered_analysis$missing$overall_plot)) {
  print(filtered_analysis$missing$overall_plot)
}
```

### Missingness by Buyer Type

This heatmap shows how missingness varies across buyer types and variables. Different procurement authorities may have varying data quality standards based on organizational capacity, compliance culture, or access to technical resources.

```{r fig.height=6}
if (!is.null(filtered_analysis$missing$by_buyer_plot)) {
  print(filtered_analysis$missing$by_buyer_plot)
}
```

### Missingness by Procedure Type

This plot shows how data quality varies across different procurement procedure types. Certain procedures may have different reporting requirements or practices.

```{r fig.height=6}
if (!is.null(filtered_analysis$missing$by_procedure_plot)) {
  print(filtered_analysis$missing$by_procedure_plot)
}
```

### Missingness Over Time

Temporal patterns in missing data can reveal changes in reporting practices over time, potentially following reforms or new transparency requirements.

```{r fig.height=6}
if (!is.null(filtered_analysis$missing$by_year_plot)) {
  print(filtered_analysis$missing$by_year_plot)
}
```

\\newpage

# Interoperability Analysis

## Organization ID Availability

Unique and persistent organization identifiers are critical for tracking procurement entities across contracts and detecting potential integrity risks. High-quality identifiers enable linking procurement data with other administrative registers to conduct comprehensive due diligence.

```{r}
if (!is.null(filtered_analysis$interoperability$org_missing)) {
  org_table <- filtered_analysis$interoperability$org_missing
  knitr::kable(org_table, caption = "Organization ID Availability")
}
```

\\newpage

# Market Analysis

## Network of Unusual Supplier Market Entries

This network visualization identifies suppliers that have entered markets where they are significantly underrepresented nationally. Each node represents either a supplier or a market, with connections showing unusual market entry patterns.

```{r fig.height=10}
if (!is.null(filtered_analysis$markets$network_plot)) {
  print(filtered_analysis$markets$network_plot)
}
```

### Suppliers with Unusual Market Entries

This analysis identifies suppliers that dominate specific markets disproportionately. Extreme concentration may indicate integrity risks.

```{r fig.height=6}
if (!is.null(filtered_analysis$markets$supplier_unusual_plot)) {
  print(filtered_analysis$markets$supplier_unusual_plot)
}
```

### Markets with Unusual Supplier Entries

This analysis examines which markets show unusual patterns of supplier entry.

```{r fig.height=6}
if (!is.null(filtered_analysis$markets$market_unusual_plot)) {
  print(filtered_analysis$markets$market_unusual_plot)
}
```

\\newpage

# Competition Analysis

## Buyer-Supplier Concentration

High concentration of contracts between specific buyer-supplier pairs may indicate reduced competition or favoritism.

```{r fig.height=8}
if (!is.null(filtered_analysis$competition$concentration_yearly_plot)) {
  print(filtered_analysis$competition$concentration_yearly_plot)
}
```

\\newpage

# Effect on Prices and Competition

## Is there an observable effect of transparency and integrity issues on prices and competition?

While this analysis does not establish any causal relationships, it can be used to explore correlations between transparency-related measures, prices, and competitiveness.

### Predicted Single-Bidding by Missing Share

This figure summarises the estimated relationship between data missingness and the prevalence of single-bid tenders. At the buyer–year level, the pipeline computes cumulative missingness and the average share of single-bid tenders. A fractional logit model with year fixed effects regresses buyer-year single-bidding share on cumulative missingness, controlling for contract numbers and value, with standard errors clustered by buyer type.

If the slope is positive and statistically significant, this suggests that data quality issues may be associated with reduced competition.

```{r fig.height=6}
if (!is.null(filtered_analysis$prices$singleb_plot)) {
  print(filtered_analysis$prices$singleb_plot)
}
```

### Single-Bidding Sensitivity Analysis

This table shows the robustness of the single-bidding model across different specifications.

```{r}
if (!is.null(filtered_analysis$prices$singleb_sensitivity_table)) {
  knitr::kable(filtered_analysis$prices$singleb_sensitivity_table, 
               caption = "Single-Bidding Model Sensitivity Analysis")
}
```

### Predicted Relative Price by Missing Share

This figure shows how relative prices correlate with overall missingness after controlling for key factors. The pipeline computes relative_price as bid_price / lot_estimatedprice and estimates a linear fixed-effects model controlling for procedure type, buyer type, and year.

An upward slope suggests that contracts with more missing data tend to have higher relative prices, potentially indicating transparency or quality issues.

```{r fig.height=6}
if (!is.null(filtered_analysis$prices$rel_price_plot)) {
  print(filtered_analysis$prices$rel_price_plot)
}
```

### Relative Price Sensitivity Analysis

This table shows the robustness of the relative price model across different specifications.

```{r}
if (!is.null(filtered_analysis$prices$relprice_sensitivity_table)) {
  knitr::kable(filtered_analysis$prices$relprice_sensitivity_table,
               caption = "Relative Price Model Sensitivity Analysis")
}
```

\\newpage

# Conclusions

This comprehensive analysis of procurement integrity highlights several key findings:

1. **Data Quality:** Systematic patterns in missing data may undermine transparency
2. **Market Competition:** Network analysis reveals unusual supplier concentrations
3. **Buyer-Supplier Concentration:** Certain buyers show elevated concentration with top suppliers
4. **Transparency and Outcomes:** Correlations between data quality issues and procurement outcomes
5. **Interoperability:** Limited identifier availability constrains analytical capabilities

These findings can inform targeted interventions to strengthen procurement integrity and transparency.

---

*Report generated using the Procurement Integrity Analysis Tool*
'

writeLines(rmd_content, temp_rmd)

tryCatch({
  rmarkdown::render(temp_rmd, output_file = output_file, quiet = FALSE, envir = new.env())
  return(TRUE)
}, error = function(e) {
  message("PDF generation error: ", e$message)
  print(e)
  return(FALSE)
})
}

generate_word_report <- function(filtered_data, filtered_analysis, country_code, output_file, filters_text = "") {
  tryCatch({
    doc <- read_docx()
    
    # Title and metadata
    doc <- doc %>%
      body_add_par("Procurement Integrity Analysis Report", style = "heading 1") %>%
      body_add_par(paste("Country:", country_code), style = "Normal") %>%
      body_add_par(paste("Date:", format(Sys.Date(), "%B %d, %Y")), style = "Normal")
    
    if (filters_text != "") {
      doc <- doc %>% 
        body_add_par("", style = "Normal") %>%
        body_add_par("Applied Filters", style = "heading 2") %>%
        body_add_par(filters_text, style = "Normal")
    }
    
    # Executive Summary
    doc <- doc %>%
      body_add_par("", style = "Normal") %>%
      body_add_par("Executive Summary", style = "heading 1") %>%
      body_add_par("This report presents a comprehensive analysis of transparency and accountability in public procurement. Data quality represents a cross-cutting challenge in procurement transparency. Missing values can undermine the ability to detect corruption risks and assess market competition.", style = "Normal")
    
    # Data Overview
    doc <- doc %>%
      body_add_par("", style = "Normal") %>%
      body_add_par("Data Overview", style = "heading 1")
    
    year_count <- if ("tender_year" %in% names(filtered_data)) {
      length(unique(filtered_data$tender_year[!is.na(filtered_data$tender_year)]))
    } else {
      "N/A"
    }
    
    overview_data <- data.frame(
      Metric = c("Total Contracts", "Unique Buyers", "Unique Suppliers", "Years Covered"),
      Value = c(
        format(nrow(filtered_data), big.mark = ","),
        format(length(unique(filtered_data$buyer_masterid)), big.mark = ","),
        format(length(unique(filtered_data$bidder_masterid)), big.mark = ","),
        as.character(year_count)
      )
    )
    
    ft <- flextable(overview_data) %>%
      theme_booktabs() %>%
      autofit()
    
    doc <- doc %>%
      body_add_flextable(ft) %>%
      body_add_par("", style = "Normal")
    
    # Missing Values Section
    doc <- doc %>%
      body_add_par("Missing Values Analysis", style = "heading 1") %>%
      body_add_par("Are there observable patterns of underreporting information in the data?", style = "heading 2") %>%
      body_add_par("The first step is to assess data completeness by examining missing values across all variables or a defined subset. Given the limited number of variables in the ProAct dataset, a full review can be efficiently conducted to identify any variables with significant underreporting.", style = "Normal") %>%
      body_add_par("", style = "Normal")
    
    if (!is.null(filtered_analysis$missing$overall_plot)) {
      temp_img <- tempfile(fileext = ".png")
      ggsave(temp_img, filtered_analysis$missing$overall_plot, width = 10, height = 8, dpi = 300)
      doc <- doc %>%
        body_add_par("Overall Missing Values by Variable", style = "heading 2") %>%
        body_add_par("This figure summarises the share of missing values for each key variable in the dataset. For every non-indicator column, the pipeline computes the proportion of records where that field is missing. Use this plot to identify which core fields are most affected by missingness and may compromise analysis—especially high-missingness identifiers, dates, and price variables.", style = "Normal") %>%
        body_add_img(temp_img, width = 6, height = 4.8) %>%
        body_add_par("", style = "Normal")
      unlink(temp_img)
    }
    
    if (!is.null(filtered_analysis$missing$by_buyer_plot)) {
      temp_img <- tempfile(fileext = ".png")
      ggsave(temp_img, filtered_analysis$missing$by_buyer_plot, width = 12, height = 10, dpi = 300)
      doc <- doc %>%
        body_add_par("Missingness by Buyer Type", style = "heading 2") %>%
        body_add_par("This heatmap shows how missingness varies across buyer types and variables. Different procurement authorities may have varying data quality standards based on organizational capacity, compliance culture, or access to technical resources for data management.", style = "Normal") %>%
        body_add_img(temp_img, width = 6, height = 4.8) %>%
        body_add_par("", style = "Normal")
      unlink(temp_img)
    }
    
    if (!is.null(filtered_analysis$missing$by_procedure_plot)) {
      temp_img <- tempfile(fileext = ".png")
      ggsave(temp_img, filtered_analysis$missing$by_procedure_plot, width = 10, height = 6, dpi = 300)
      doc <- doc %>%
        body_add_par("Missingness by Procedure Type", style = "heading 2") %>%
        body_add_par("This plot shows how data quality varies across different procurement procedure types. Certain procedures may have different reporting requirements or practices.", style = "Normal") %>%
        body_add_img(temp_img, width = 6, height = 3.6) %>%
        body_add_par("", style = "Normal")
      unlink(temp_img)
    }
    
    if (!is.null(filtered_analysis$missing$by_year_plot)) {
      temp_img <- tempfile(fileext = ".png")
      ggsave(temp_img, filtered_analysis$missing$by_year_plot, width = 10, height = 6, dpi = 300)
      doc <- doc %>%
        body_add_par("Missingness Over Time", style = "heading 2") %>%
        body_add_par("Temporal patterns in missing data can reveal changes in reporting practices over time, potentially following reforms or new transparency requirements.", style = "Normal") %>%
        body_add_img(temp_img, width = 6, height = 3.6) %>%
        body_add_par("", style = "Normal")
      unlink(temp_img)
    }
    
    # Interoperability Section
    if (!is.null(filtered_analysis$interoperability$org_missing)) {
      doc <- doc %>%
        body_add_par("Interoperability Analysis", style = "heading 1") %>%
        body_add_par("Organization ID Availability", style = "heading 2") %>%
        body_add_par("Unique and persistent organization identifiers are critical for tracking procurement entities across contracts and detecting potential integrity risks. High-quality identifiers enable linking procurement data with other administrative registers to conduct comprehensive due diligence.", style = "Normal") %>%
        body_add_par("", style = "Normal")
      
      org_table <- filtered_analysis$interoperability$org_missing %>%
        mutate(`Missing share` = ifelse(is.na(missing_share), "Not available",
                                        scales::percent(missing_share, accuracy = 1))) %>%
        select(`Organization type` = organization_type, `ID type` = id_type, `Missing share`)
      
      ft_org <- flextable(org_table) %>% theme_booktabs() %>% autofit()
      
      doc <- doc %>%
        body_add_flextable(ft_org) %>%
        body_add_par("", style = "Normal")
    }
    
    # Market Analysis Section
    doc <- doc %>%
      body_add_par("Market Analysis", style = "heading 1") %>%
      body_add_par("", style = "Normal")
    
    if (!is.null(filtered_analysis$markets$network_plot)) {
      temp_img <- tempfile(fileext = ".png")
      ggsave(temp_img, filtered_analysis$markets$network_plot, width = 12, height = 12, dpi = 300)
      doc <- doc %>%
        body_add_par("Network of Unusual Supplier Market Entries", style = "heading 2") %>%
        body_add_par("This network visualization identifies suppliers that have entered markets where they are significantly underrepresented nationally. Each node represents either a supplier or a market, with connections showing unusual market entry patterns. Such patterns may warrant closer examination.", style = "Normal") %>%
        body_add_img(temp_img, width = 6, height = 6) %>%
        body_add_par("", style = "Normal")
      unlink(temp_img)
    }
    
    if (!is.null(filtered_analysis$markets$supplier_unusual_plot)) {
      temp_img <- tempfile(fileext = ".png")
      ggsave(temp_img, filtered_analysis$markets$supplier_unusual_plot, width = 10, height = 8, dpi = 300)
      doc <- doc %>%
        body_add_par("Suppliers with Unusual Market Entries", style = "heading 2") %>%
        body_add_par("This analysis identifies suppliers that dominate specific markets disproportionately. Extreme concentration may indicate integrity risks.", style = "Normal") %>%
        body_add_img(temp_img, width = 6, height = 4.8) %>%
        body_add_par("", style = "Normal")
      unlink(temp_img)
    }
    
    if (!is.null(filtered_analysis$markets$market_unusual_plot)) {
      temp_img <- tempfile(fileext = ".png")
      ggsave(temp_img, filtered_analysis$markets$market_unusual_plot, width = 10, height = 8, dpi = 300)
      doc <- doc %>%
        body_add_par("Markets with Unusual Supplier Entries", style = "heading 2") %>%
        body_add_par("This analysis examines which markets show unusual patterns of supplier entry.", style = "Normal") %>%
        body_add_img(temp_img, width = 6, height = 4.8) %>%
        body_add_par("", style = "Normal")
      unlink(temp_img)
    }
    
    # Competition Analysis
    doc <- doc %>%
      body_add_par("Competition Analysis", style = "heading 1") %>%
      body_add_par("Buyer-Supplier Concentration", style = "heading 2") %>%
      body_add_par("High concentration of contracts between specific buyer-supplier pairs may indicate reduced competition or favoritism. This analysis identifies buyers with the highest concentration of contracts awarded to their top suppliers.", style = "Normal") %>%
      body_add_par("", style = "Normal")
    
    if (!is.null(filtered_analysis$competition$concentration_yearly_plot)) {
      temp_img <- tempfile(fileext = ".png")
      ggsave(temp_img, filtered_analysis$competition$concentration_yearly_plot, width = 12, height = 10, dpi = 300)
      doc <- doc %>%
        body_add_img(temp_img, width = 6, height = 5) %>%
        body_add_par("", style = "Normal")
      unlink(temp_img)
    }
    
    # Effect on Prices and Competition
    doc <- doc %>%
      body_add_par("Effect on Prices and Competition", style = "heading 1") %>%
      body_add_par("Is there an observable effect of transparency and integrity issues on prices and competition?", style = "heading 2") %>%
      body_add_par("While this analysis does not establish any causal relationships, it can be used to explore correlations between transparency-related measures, prices, and competitiveness.", style = "Normal") %>%
      body_add_par("", style = "Normal")
    
    if (!is.null(filtered_analysis$prices$singleb_plot)) {
      temp_img <- tempfile(fileext = ".png")
      ggsave(temp_img, filtered_analysis$prices$singleb_plot, width = 10, height = 8, dpi = 300)
      doc <- doc %>%
        body_add_par("Predicted Single-Bidding by Missing Share", style = "heading 2") %>%
        body_add_par("This figure summarises the estimated relationship between data missingness and the prevalence of single-bid tenders. At the buyer–year level, the pipeline computes cumulative missingness and the average share of single-bid tenders. A fractional logit model with year fixed effects regresses buyer-year single-bidding share on cumulative missingness, controlling for contract numbers and value. If the slope is positive and statistically significant, this suggests that data quality issues may be associated with reduced competition.", style = "Normal") %>%
        body_add_img(temp_img, width = 6, height = 4.8) %>%
        body_add_par("", style = "Normal")
      unlink(temp_img)
    }
    
    # Single-bidding sensitivity table
    if (!is.null(filtered_analysis$prices$singleb_sensitivity_table)) {
      doc <- doc %>%
        body_add_par("Single-Bidding Sensitivity Analysis", style = "heading 2") %>%
        body_add_par("This table shows the robustness of the single-bidding model across different specifications.", style = "Normal")
      
      ft_singleb <- flextable(filtered_analysis$prices$singleb_sensitivity_table) %>%
        theme_booktabs() %>%
        autofit()
      
      doc <- doc %>%
        body_add_flextable(ft_singleb) %>%
        body_add_par("", style = "Normal")
    }
    
    if (!is.null(filtered_analysis$prices$rel_price_plot)) {
      temp_img <- tempfile(fileext = ".png")
      ggsave(temp_img, filtered_analysis$prices$rel_price_plot, width = 10, height = 8, dpi = 300)
      doc <- doc %>%
        body_add_par("Predicted Relative Price by Missing Share", style = "heading 2") %>%
        body_add_par("This figure shows how relative prices correlate with overall missingness after controlling for key factors. The pipeline computes relative_price as bid_price / lot_estimatedprice and estimates a linear fixed-effects model controlling for procedure type, buyer type, and year. An upward slope suggests that contracts with more missing data tend to have higher relative prices, potentially indicating transparency or quality issues.", style = "Normal") %>%
        body_add_img(temp_img, width = 6, height = 4.8) %>%
        body_add_par("", style = "Normal")
      unlink(temp_img)
    }
    
    # Relative price sensitivity table
    if (!is.null(filtered_analysis$prices$relprice_sensitivity_table)) {
      doc <- doc %>%
        body_add_par("Relative Price Sensitivity Analysis", style = "heading 2") %>%
        body_add_par("This table shows the robustness of the relative price model across different specifications.", style = "Normal")
      
      ft_relprice <- flextable(filtered_analysis$prices$relprice_sensitivity_table) %>%
        theme_booktabs() %>%
        autofit()
      
      doc <- doc %>%
        body_add_flextable(ft_relprice) %>%
        body_add_par("", style = "Normal")
    }
    
    # Conclusions
    doc <- doc %>%
      body_add_par("Conclusions", style = "heading 1") %>%
      body_add_par("This analysis highlights several key findings:", style = "Normal") %>%
      body_add_par("• Data Quality: Systematic patterns in missing data may undermine transparency", style = "Normal") %>%
      body_add_par("• Market Competition: Network analysis reveals unusual supplier concentrations", style = "Normal") %>%
      body_add_par("• Buyer-Supplier Concentration: Certain buyers show elevated concentration with top suppliers", style = "Normal") %>%
      body_add_par("• Transparency and Outcomes: Correlations between data quality issues and procurement outcomes", style = "Normal") %>%
      body_add_par("• Interoperability: Limited identifier availability constrains analytical capabilities", style = "Normal") %>%
      body_add_par("", style = "Normal") %>%
      body_add_par("These findings can inform targeted interventions to strengthen procurement integrity and transparency.", style = "Normal")
    
    print(doc, target = output_file)
    return(TRUE)
    
  }, error = function(e) {
    message("Word generation error: ", e$message)
    print(e)
    return(FALSE)
  })
}

# ========================================================================
# USER INTERFACE
# ========================================================================

ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(title = "Procurement Integrity Analysis", titleWidth = 350),
  
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      id = "sidebar",
      menuItem("Setup", tabName = "setup", icon = icon("cog")),
      menuItem("Overview", tabName = "overview", icon = icon("home")),
      menuItem("Data Overview", tabName = "data_overview", icon = icon("table")),
      menuItem("Missing Values", tabName = "missing", icon = icon("exclamation-triangle")),
      menuItem("Interoperability", tabName = "interoperability", icon = icon("link")),
      menuItem("Risky Profiles", tabName = "risky", icon = icon("exclamation-circle")),
      menuItem("Prices & Competition", tabName = "prices_competition", icon = icon("dollar-sign")),
      menuItem("Export", tabName = "export", icon = icon("download"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper { background-color: #f4f4f4; }
        .box { box-shadow: 0 1px 3px rgba(0,0,0,0.12); margin-bottom: 20px; }
        .description-box { 
          background-color: #f9f9f9; 
          padding: 15px; 
          border-left: 4px solid #3c8dbc; 
          margin-bottom: 15px;
          border-radius: 3px;
          font-size: 14px;
          line-height: 1.6;
        }
        .question-header {
          color: #dd4b39;
          font-weight: bold;
          font-size: 18px;
          margin-top: 20px;
          margin-bottom: 10px;
        }
        .download-btn {
          margin: 5px;
          min-width: 150px;
        }
        .filter-box {
          background-color: #ecf0f5;
          padding: 15px;
          border-radius: 5px;
          margin-bottom: 20px;
        }
        .filter-title {
          font-size: 16px;
          font-weight: bold;
          margin-bottom: 10px;
          color: #3c8dbc;
        }
      "))
    ),
    
    tabItems(
      # Setup Tab
      tabItem(
        tabName = "setup",
        fluidRow(
          box(
            title = "Data Upload and Configuration",
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            
            fluidRow(
              column(6,
                     fileInput("datafile", "Upload Procurement Data (CSV)",
                               accept = c(".csv", "text/csv", "text/comma-separated-values,text/plain")),
                     helpText(tags$b("Maximum file size: 1 GB")),
                     textInput("country_code", "Country Code (2 letters)", value = "BG", placeholder = "e.g., BG, UY, UG"),
                     actionButton("run_analysis", "Run Analysis", icon = icon("play"), class = "btn-success btn-lg")
              ),
              column(6,
                     h4("Instructions:"),
                     tags$ol(
                       tags$li("Upload your procurement data CSV file"),
                       tags$li("Enter the two-letter country code"),
                       tags$li("Click 'Run Analysis' to process the data"),
                       tags$li("Use filters on each tab to subset the data"),
                       tags$li("Navigate to Export tab to download reports")
                     ),
                     hr(),
                     h4("New: Filtering Options"),
                     tags$ul(
                       tags$li("Filter by year range"),
                       tags$li("Filter by market (CPV codes)"),
                       tags$li("Filter by contract value"),
                       tags$li("Filter by buyer type")
                     )
              )
            )
          )
        ),
        
        fluidRow(
          box(
            title = "Analysis Status",
            width = 12,
            solidHeader = TRUE,
            status = "info",
            verbatimTextOutput("analysis_status")
          )
        )
      ),
      
      # Overview Tab
      tabItem(
        tabName = "overview",
        h2("Integrity, Transparency, and Accountability Analysis"),
        
        fluidRow(
          box(
            width = 12,
            status = "primary",
            p(style = "font-size: 16px;",
              "This analysis examines transparency and accountability challenges in public procurement systems using procurement data. 
              The tool provides a framework for assessing the extent to which regulatory requirements are reflected in actual implementation, 
              focusing on transparency and accountability as key determinants of procurement integrity and corruption risk. 
              Beyond evaluating data availability and accessibility, the analysis proposes tests to identify potential collusive relationships 
              between buyers and suppliers and assesses how transparency affects competition and price overruns."
            )
          )
        ),
        
        fluidRow(
          box(
            title = "Analysis Framework",
            width = 12,
            status = "info",
            solidHeader = TRUE,
            
            h4("Key Questions Addressed:"),
            tags$ol(
              tags$li(tags$b("Data Quality:"), " Are there observable patterns of underreporting information in the data?"),
              tags$li(tags$b("Interoperability:"), " Can this data be matched to other registers in the country to ensure higher quality monitoring?"),
              tags$li(tags$b("Risk Profiles:"), " Do companies winning contracts have risky profiles?"),
              tags$li(tags$b("Market Entry:"), " Are there suspicious market entry patterns by suppliers?"),
              tags$li(tags$b("Concentration:"), " Are there suspicious connections between buyers and suppliers?"),
              tags$li(tags$b("Transparency Impact:"), " Is there an observable effect of transparency and integrity issues on prices and competition?")
            )
          )
        )
      ),
      
      # Data Overview Tab
      tabItem(
        tabName = "data_overview",
        h2("Data Overview"),
        
        # FILTERS
        fluidRow(
          box(
            title = "Filters",
            width = 12,
            collapsible = TRUE,
            collapsed = FALSE,
            status = "info",
            
            fluidRow(
              column(2,
                     uiOutput("year_filter_overview")
              ),
              column(2,
                     uiOutput("market_filter_overview")
              ),
              column(2,
                     uiOutput("value_filter_overview")
              ),
              column(3,
                     uiOutput("buyer_type_filter_overview")
              ),
              column(3,
                     uiOutput("procedure_type_filter_overview")
              )
            ),
            
            fluidRow(
              column(12,
                     actionButton("apply_filters_overview", "Apply Filters", icon = icon("filter"), class = "btn-primary"),
                     actionButton("reset_filters_overview", "Reset Filters", icon = icon("undo"), class = "btn-warning"),
                     textOutput("filter_status_overview", inline = TRUE)
              )
            )
          )
        ),
        
        fluidRow(
          valueBoxOutput("n_contracts", width = 3),
          valueBoxOutput("n_buyers", width = 3),
          valueBoxOutput("n_suppliers", width = 3),
          valueBoxOutput("n_years", width = 3)
        ),
        
        fluidRow(
          box(
            title = "Contracts per Year",
            width = 6,
            solidHeader = TRUE,
            status = "primary",
            plotOutput("contracts_year_plot", height = "400px"),
            downloadButton("download_contracts_year", "Download Figure", class = "download-btn btn-sm")
          ),
          box(
            title = "Summary Statistics",
            width = 6,
            solidHeader = TRUE,
            status = "primary",
            DT::dataTableOutput("summary_table")
          )
        ),
        
        fluidRow(
          box(
            title = "Variables Present in Dataset",
            width = 12,
            solidHeader = TRUE,
            status = "info",
            p("Variables present (excluding indicator variables):"),
            uiOutput("variables_list")
          )
        )
      ),
      
      # Missing Values Tab  
      tabItem(
        tabName = "missing",
        h2("Missing Values Analysis"),
        
        # FILTERS
        fluidRow(
          box(
            title = "Filters",
            width = 12,
            collapsible = TRUE,
            collapsed = FALSE,
            status = "info",
            
            fluidRow(
              column(2,
                     uiOutput("year_filter_missing")
              ),
              column(2,
                     uiOutput("market_filter_missing")
              ),
              column(2,
                     uiOutput("value_filter_missing")
              ),
              column(3,
                     uiOutput("buyer_type_filter_missing")
              ),
              column(3,
                     uiOutput("procedure_type_filter_missing")
              )
            ),
            
            fluidRow(
              column(12,
                     actionButton("apply_filters_missing", "Apply Filters", icon = icon("filter"), class = "btn-primary"),
                     actionButton("reset_filters_missing", "Reset Filters", icon = icon("undo"), class = "btn-warning"),
                     textOutput("filter_status_missing", inline = TRUE)
              )
            )
          )
        ),
        
        div(class = "question-header",
            "Are there observable patterns of underreporting information in the data?"
        ),
        
        div(class = "description-box",
            p("The first step is to assess data completeness by examining missing values across all variables or a defined subset. 
              Given the limited number of variables in the ProAct dataset, a full review can be efficiently conducted to identify 
              any variables with significant underreporting. Each variable contributed to one of the ProAct indicators.")
        ),
        
        fluidRow(
          box(
            title = "Overall Missing Values by Variable",
            width = 12,
            solidHeader = TRUE,
            status = "warning",
            div(class = "description-box",
                "This figure summarises the share of missing values for each key variable in the dataset. 
                For every non-indicator column (all variables that do not start with 'ind_'), the pipeline computes the 
                mean of an NA indicator across all observations, i.e. the proportion of records where that field is missing. 
                Variables are displayed with human-readable labels (e.g. buyer and bidder IDs, names, locations, prices). 
                Use this plot to identify which core fields are most affected by missingness and may compromise analysis—especially 
                high-missingness identifiers, dates, and price variables."
            ),
            plotOutput("missing_overall_plot", height = "600px"),
            downloadButton("download_missing_overall", "Download Figure", class = "download-btn btn-sm")
          )
        ),
        
        fluidRow(
          box(
            title = "Missingness by Buyer Type",
            width = 12,
            solidHeader = TRUE,
            status = "warning",
            div(class = "description-box",
                "This heatmap shows how missingness varies across buyer types and variables."
            ),
            plotOutput("missing_buyer_plot", height = "700px"),
            downloadButton("download_missing_buyer", "Download Figure", class = "download-btn btn-sm")
          )
        ),
        
        fluidRow(
          box(
            title = "Missingness by Procedure Type",
            width = 12,
            solidHeader = TRUE,
            status = "warning",
            div(class = "description-box",
                "This plot shows how data quality varies across different procurement procedure types."
            ),
            plotOutput("missing_procedure_plot", height = "700px"),
            downloadButton("download_missing_procedure", "Download Figure", class = "download-btn btn-sm")
          )
        ),
        
        fluidRow(
          box(
            title = "Trends in Missing Shares Over Time",
            width = 12,
            solidHeader = TRUE,
            status = "warning",
            div(class = "description-box",
                "This line plot tracks how missingness evolves over time for the 10 variables with the highest average missing share."
            ),
            plotOutput("missing_time_plot", height = "500px"),
            downloadButton("download_missing_time", "Download Figure", class = "download-btn btn-sm")
          )
        )
      ),
      
      # Interoperability Tab
      tabItem(
        tabName = "interoperability",
        h2("Interoperability Analysis"),
        
        # FILTERS
        fluidRow(
          box(
            title = "Filters",
            width = 12,
            collapsible = TRUE,
            collapsed = FALSE,
            status = "info",
            
            fluidRow(
              column(2,
                     uiOutput("year_filter_interop")
              ),
              column(2,
                     uiOutput("market_filter_interop")
              ),
              column(2,
                     uiOutput("value_filter_interop")
              ),
              column(3,
                     uiOutput("buyer_type_filter_interop")
              ),
              column(3,
                     uiOutput("procedure_type_filter_interop")
              )
            ),
            
            fluidRow(
              column(12,
                     actionButton("apply_filters_interop", "Apply Filters", icon = icon("filter"), class = "btn-primary"),
                     actionButton("reset_filters_interop", "Reset Filters", icon = icon("undo"), class = "btn-warning"),
                     textOutput("filter_status_interop", inline = TRUE)
              )
            )
          )
        ),
        
        div(class = "question-header",
            "Can this data be matched to other registers in the country to ensure higher quality monitoring?"
        ),
        
        div(class = "description-box",
            p("The ability to match public procurement data with other registers significantly enhances the analytical power of research 
              and strengthens auditing capabilities.")
        ),
        
        fluidRow(
          box(
            title = "Interoperability Potential at Organization Level",
            width = 12,
            solidHeader = TRUE,
            status = "info",
            DT::dataTableOutput("interoperability_table")
          )
        )
      ),
      
      # Risky Profiles Tab
      tabItem(
        tabName = "risky",
        h2("Companies with Risky Profiles"),
        
        # FILTERS
        fluidRow(
          box(
            title = "Filters",
            width = 12,
            collapsible = TRUE,
            collapsed = FALSE,
            status = "info",
            
            fluidRow(
              column(2,
                     uiOutput("year_filter_risky")
              ),
              column(2,
                     uiOutput("market_filter_risky")
              ),
              column(2,
                     uiOutput("value_filter_risky")
              ),
              column(3,
                     uiOutput("buyer_type_filter_risky")
              ),
              column(3,
                     uiOutput("procedure_type_filter_risky")
              )
            ),
            
            fluidRow(
              column(12,
                     actionButton("apply_filters_risky", "Apply Filters", icon = icon("filter"), class = "btn-primary"),
                     actionButton("reset_filters_risky", "Reset Filters", icon = icon("undo"), class = "btn-warning"),
                     textOutput("filter_status_risky", inline = TRUE)
              )
            )
          )
        ),
        
        div(class = "question-header",
            "Do companies winning contracts have risky profiles?"
        ),
        
        div(class = "description-box",
            p("This analysis seeks to identify potentially suspicious patterns in company behavior, including movements between markets 
              or registration in tax haven jurisdictions.")
        ),
        
        fluidRow(
          box(
            title = "Network of Unusual Supplier Market Entries",
            width = 12,
            solidHeader = TRUE,
            status = "danger",
            div(class = "description-box",
                p("This network visualisation maps atypical movements of suppliers across CPV product clusters. 
                  Each node represents a 3-digit CPV cluster, i.e. a broad product or service market derived from 'lot_productcode'. 
                  For every bidder, a 'home' CPV cluster is defined as the cluster where the bidder wins the most awards. 
                  Entries into other clusters are treated as 'target' markets. 
                  A bidder–cluster combination is flagged as atypical if the bidder has enough overall history, but that cluster accounts for less than 5% of their awards and at most three wins there. 
                  For each bidder–cluster pair, the pipeline computes a Laplace-smoothed probability of observing that bidder in that cluster, 
                  then standardises this into a z-score (the 'surprise' metric). 
                  High z-scores indicate unusually rare entries. The network edges connect home clusters to target clusters, 
                  sized and coloured by the strength of the surprise signal. 
                  Use this visualisation to spot non-standard market diversification patterns that may merit closer scrutiny.")
            ),
            plotOutput("network_plot", height = "700px"),
            downloadButton("download_network", "Download Figure", class = "download-btn btn-sm")
          )
        ),
        
        fluidRow(
          box(
            title = "Suppliers with Unusually Diversified Market Entries",
            width = 6,
            solidHeader = TRUE,
            status = "warning",
            plotOutput("supplier_unusual_plot", height = "500px"),
            downloadButton("download_supplier_unusual", "Download Figure", class = "download-btn btn-sm")
          ),
          box(
            title = "Markets Attracting Unusual Supplier Entries",
            width = 6,
            solidHeader = TRUE,
            status = "warning",
            plotOutput("market_unusual_plot", height = "500px"),
            downloadButton("download_market_unusual", "Download Figure", class = "download-btn btn-sm")
          )
        ),
        
        div(class = "question-header",
            "Are there suspicious connections between buyers and suppliers?"
        ),
        
        fluidRow(
          box(
            title = "Top Buyers by Supplier Concentration Over Time",
            width = 12,
            solidHeader = TRUE,
            status = "danger",
            plotOutput("concentration_plot", height = "700px"),
            downloadButton("download_concentration", "Download Figure", class = "download-btn btn-sm")
          )
        )
      ),
      
      # Prices & Competition Tab
      tabItem(
        tabName = "prices_competition",
        h2("Effect on Prices and Competition"),
        
        # FILTERS
        fluidRow(
          box(
            title = "Filters",
            width = 12,
            collapsible = TRUE,
            collapsed = FALSE,
            status = "info",
            
            fluidRow(
              column(2, uiOutput("year_filter_prices")),
              column(2, uiOutput("market_filter_prices")),
              column(2, uiOutput("value_filter_prices")),
              column(3, uiOutput("buyer_type_filter_prices")),
              column(3, uiOutput("procedure_type_filter_prices"))
            ),
            
            fluidRow(
              column(12,
                     actionButton("apply_filters_prices", "Apply Filters", icon = icon("filter"), class = "btn-primary"),
                     actionButton("reset_filters_prices", "Reset Filters", icon = icon("undo"), class = "btn-warning"),
                     textOutput("filter_status_prices", inline = TRUE)
              )
            )
          )
        ),
        
        div(class = "question-header",
            "Is there an observable effect of transparency and integrity issues on prices and competition?"
        ),
        
        div(class = "description-box",
            p("While this analysis does not establish any causal relationships, it can be used to explore correlations between 
              transparency-related measures, prices, and competitiveness.")
        ),
        
        # SINGLE-BIDDING ANALYSIS
        fluidRow(
          box(
            title = "Predicted Single-Bidding by Missing Share",
            width = 12,
            solidHeader = TRUE,
            status = "info",
            div(class = "description-box",
                p("This figure summarises the estimated relationship between data missingness and the prevalence of single-bid tenders. 
                  At the buyer–year level (for years after 2014), the pipeline computes: 
                  (i) a cumulative missing-share indicator as the average of all variable-specific missing shares, and 
                  (ii) the average share of single-bid tenders using the 'ind_corr_singleb' indicator (rescaled to [0,1]). 
                  A fractional logit model with year fixed effects regresses the buyer-year single-bidding share on cumulative missingness, 
                  controlling for the number of contracts and average contract value, and clustering standard errors by buyer type. 
                  The marginal effect is visualised as a line plot, showing how the predicted probability of single-bidding changes with missingness. 
                  If the slope is positive and statistically significant, this suggests that data quality issues may be associated with reduced competition.")
            ),
            uiOutput("singleb_plot_ui"),
            uiOutput("download_singleb_ui")
          )
        ),
        
        # SINGLE-BIDDING SENSITIVITY TABLE
        fluidRow(
          box(
            title = "Single-Bidding Sensitivity Analysis",
            width = 12,
            solidHeader = TRUE,
            status = "info",
            div(class = "description-box",
                "This table shows the robustness of the single-bidding model across different specifications."
            ),
            uiOutput("singleb_sensitivity_table")
          )
        ),
        
        # RELATIVE PRICE ANALYSIS
        fluidRow(
          box(
            title = "Predicted Relative Price by Missing Share",
            width = 12,
            solidHeader = TRUE,
            status = "info",
            div(class = "description-box",
                p("This figure shows how relative prices correlate with overall missingness after controlling for key factors. 
                  For contracts with valid price information (bid price and estimated lot value), 
                  the pipeline computes 'relative_price' as bid_price / lot_estimatedprice and trims extreme or non-positive values. 
                  It then constructs 'total_missing_share' as the average of NA indicators across a predefined set of core variables, 
                  and estimates a linear fixed-effects model of relative_price on total_missing_share, 
                  controlling for procedure type, buyer type, and year (with year fixed effects and clustering by buyer). 
                  The marginal effect plot displays how predicted relative prices vary with the share of missing fields. 
                  An upward slope suggests that contracts with more missing data tend to have higher relative prices, 
                  potentially indicating issues with transparency or quality of procurement processes.")
            ),
            uiOutput("relprice_plot_ui"),
            uiOutput("download_relprice_ui")
          )
        ),
        
        # RELATIVE PRICE SENSITIVITY TABLE
        fluidRow(
          box(
            title = "Relative Price Sensitivity Analysis",
            width = 12,
            solidHeader = TRUE,
            status = "info",
            div(class = "description-box",
                "This table shows the robustness of the relative price model across different specifications."
            ),
            uiOutput("relprice_sensitivity_table")
          )
        )
      ),
      
      # Then add the server-side outputs for the sensitivity tables
      
      # Export Tab
      tabItem(
        tabName = "export",
        h2("Export Reports and Figures"),
        
        fluidRow(
          box(
            title = "Active Filters",
            width = 12,
            solidHeader = TRUE,
            status = "warning",
            uiOutput("export_filter_info")
          )
        ),
        
        fluidRow(
          box(
            title = "Generate Full Report",
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            
            p("Generate a comprehensive report containing all analysis results with currently applied filters."),
            
            fluidRow(
              column(6,
                     h4("PDF Report"),
                     p("Complete report in PDF format with all visualizations and tables."),
                     downloadButton("download_pdf_report", "Generate PDF Report", class = "btn-primary btn-lg"),
                     br(), br(),
                     uiOutput("pdf_status")
              ),
              column(6,
                     h4("Word Document"),
                     p("Editable Word document with all analysis results."),
                     downloadButton("download_word_report", "Generate Word Report", class = "btn-primary btn-lg"),
                     br(), br(),
                     uiOutput("word_status")
              )
            )
          )
        ),
        
        fluidRow(
          box(
            title = "Download All Figures (ZIP)",
            width = 12,
            solidHeader = TRUE,
            status = "info",
            
            p("Download all generated figures in high resolution (PNG format, 300 DPI) as a single ZIP file."),
            downloadButton("download_all_figures", "Download All Figures (ZIP)", class = "btn-info btn-lg"),
            br(), br(),
            p(tags$i("Includes: all plots currently displayed based on applied filters."))
          )
        )
      )
    )
  )
)

# ========================================================================
# SERVER LOGIC
# ========================================================================

server <- function(input, output, session) {
  
  # Reactive values
  results <- reactiveValues(
    data = NULL,
    analysis = NULL,
    status = "📂 No data loaded yet.\n\nPlease upload a CSV file and click 'Run Analysis' to begin.",
    country_code = NULL,
    filtered_data = NULL,
    filtered_analysis = NULL,
    value_divisor = 1  # ADD THIS LINE
  )
  
  # Helper functions for sensitivity analysis display
  has_rows <- function(df) is.data.frame(df) && nrow(df) > 0
  
  safe_first_num <- function(x) {
    if (is.null(x) || length(x) == 0) return(NA_real_)
    suppressWarnings(as.numeric(x[[1]]))
  }
  
  get_first_num <- function(df, candidates) {
    if (!is.data.frame(df)) return(NA_real_)
    for (nm in candidates) {
      if (nm %in% names(df)) return(safe_first_num(df[[nm]]))
    }
    NA_real_
  }
  
  # Sensitivity analysis UI output - Single Bidding
  output$singleb_sensitivity_table <- renderUI({
    req(results$filtered_analysis$competition$singleb_sensitivity)
    
    bundle <- results$filtered_analysis$competition$singleb_sensitivity
    
    # Check if bundle exists and has overall data
    if (is.null(bundle) || !has_rows(bundle$overall)) {
      return(p("Sensitivity bundle not available."))
    }
    
    # Calculate statistics for conclusion
    n_specs <- get_first_num(bundle$overall, c("n_specs"))
    share_pos <- get_first_num(bundle$overall, c("share_positive"))
    share_neg <- get_first_num(bundle$overall, c("share_negative"))
    median_est <- get_first_num(bundle$overall, c("median_estimate"))
    median_p <- get_first_num(bundle$overall, c("median_pvalue"))
    median_str <- get_first_num(bundle$overall, c("median_strength"))
    share_p05 <- get_first_num(bundle$overall, c("share_p_le_0.05"))
    share_p10 <- get_first_num(bundle$overall, c("share_p_le_0.1", "share_p_le_0.10"))
    share_p20 <- get_first_num(bundle$overall, c("share_p_le_0.2", "share_p_le_0.20"))
    sign_stable <- get_first_num(bundle$sign, c("share_sign_stable"))
    n_nonzero <- get_first_num(bundle$sign, c("n_nonzero"))
    
    # Determine dominant direction
    dominant_dir <- NA_character_
    if (!is.na(share_pos) && !is.na(share_neg)) {
      if (share_pos > share_neg) dominant_dir <- "positive"
      else if (share_neg > share_pos) dominant_dir <- "negative"
      else dominant_dir <- "mixed"
    }
    
    # Generate conclusion text
    conclusion_text <- ""
    conclusion_color <- "info"
    
    if (isTRUE(dominant_dir == "positive" && sign_stable == 1 && share_p10 >= 0.5 && share_pos >= 0.7)) {
      conclusion_text <- "✓ Strong and robust evidence (positive). Missingness is associated with a higher outcome in most specifications, and the direction is stable."
      conclusion_color <- "success"
    } else if (isTRUE(dominant_dir == "negative" && sign_stable == 1 && share_p10 >= 0.5 && share_neg >= 0.7)) {
      conclusion_text <- "✓ Strong and robust evidence (negative). Missingness is associated with a lower outcome in most specifications, and the direction is stable."
      conclusion_color <- "success"
    } else if (isTRUE(dominant_dir %in% c("positive","negative") && share_p10 >= 0.3 &&
                      ((dominant_dir=="positive" && share_pos>=0.6) || (dominant_dir=="negative" && share_neg>=0.6)))) {
      conclusion_text <- "⚠ Moderate evidence. The relationship is mostly in one direction, but statistical significance varies across specifications."
      conclusion_color <- "warning"
    } else if (isTRUE(dominant_dir %in% c("positive","negative") && sign_stable == 0 &&
                      ((dominant_dir=="positive" && share_pos>=0.6) || (dominant_dir=="negative" && share_neg>=0.6)))) {
      conclusion_text <- "⚠ Directionally dominant but unstable. Most estimates point the same way, but the sign flips in some specifications."
      conclusion_color <- "warning"
    } else if (!is.na(dominant_dir)) {
      conclusion_text <- "✗ No robust relationship. Results are mixed or highly sensitive to model specification."
      conclusion_color <- "danger"
    } else {
      conclusion_text <- "ℹ Sensitivity summary not available for this country."
      conclusion_color <- "info"
    }
    
    # Build UI with separate sections
    tagList(
      # Introduction
      div(class = "alert alert-info", style = "margin-bottom: 20px;",
          h4(style = "margin-top: 0;", "What is sensitivity analysis?"),
          p("Sensitivity analysis tests whether the relationship between missing data and the outcome (single-bidding) holds across different modeling choices. A robust finding should be consistent across reasonable variations in:"),
          tags$ul(
            tags$li(strong("Fixed effects (FE):"), " Controls for unobserved differences across buyers, years, etc."),
            tags$li(strong("Clustering:"), " Adjusts standard errors for correlation within groups"),
            tags$li(strong("Control variables:"), " Additional factors that might affect the outcome")
          )
      ),
      
      # Overall Summary
      div(class = "panel panel-primary",
          div(class = "panel-heading", h4(class = "panel-title", style = "margin: 0;", "Overall Summary")),
          div(class = "panel-body",
              div(style = "overflow-x: auto; max-width: 100%;",
                  renderTable({
                    summary_panel <- dplyr::bind_cols(bundle$overall, bundle$sign)
                    summary_panel
                  }, digits = 3, striped = TRUE, hover = TRUE, bordered = TRUE, width = "auto")
              )
          )
      ),
      
      # Conclusion Box
      div(class = paste0("alert alert-", conclusion_color), style = "margin-top: 20px; margin-bottom: 20px;",
          h4(style = "margin-top: 0;", "Summary for Decision-Makers"),
          p(strong(conclusion_text)),
          tags$ul(
            if (!is.na(share_pos)) tags$li(scales::percent(share_pos, accuracy = 1), " of models are positive"),
            if (!is.na(share_neg)) tags$li(scales::percent(share_neg, accuracy = 1), " of models are negative"),
            if (!is.na(share_p10)) tags$li(scales::percent(share_p10, accuracy = 1), " of models are statistically significant (p ≤ 0.10)"),
            if (!is.na(median_est)) tags$li("Median effect size: ", sprintf("%.3f", median_est)),
            if (!is.na(median_p)) tags$li("Median p-value: ", sprintf("%.3f", median_p))
          )
      ),
      
      # Detailed breakdown (collapsible)
      tags$details(
        tags$summary(strong("Show detailed breakdown by model components")),
        
        # By Fixed Effects
        div(class = "panel panel-default", style = "margin-top: 15px;",
            div(class = "panel-heading", h5(class = "panel-title", style = "margin: 0;", "By Fixed Effects (FE)")),
            div(class = "panel-body",
                p("Fixed effects control for unobserved heterogeneity. For example:"),
                tags$ul(
                  tags$li(strong("Buyer FE:"), " Controls for buyer-specific characteristics (e.g., capacity, procurement culture)"),
                  tags$li(strong("Year FE:"), " Controls for time trends (e.g., economic conditions, regulatory changes)"),
                  tags$li(strong("Buyer + Year FE:"), " Controls for both simultaneously")
                ),
                if (has_rows(bundle$by_fe)) {
                  renderTable({ bundle$by_fe }, digits = 3, striped = TRUE, hover = TRUE, bordered = TRUE, width = "auto")
                } else {
                  p(em("No fixed-effects breakdown available."))
                },
                p(em(style = "color: #666;", "Look for: Which fixed effect structure gives the most significant results? If results hold across all FE specifications, the finding is very robust."))
            )
        ),
        
        # By Clustering
        div(class = "panel panel-default",
            div(class = "panel-heading", h5(class = "panel-title", style = "margin: 0;", "By Clustering")),
            div(class = "panel-body",
                p("Clustering adjusts standard errors for correlation within groups:"),
                tags$ul(
                  tags$li(strong("Buyer clustering:"), " Accounts for correlation within the same buyer over time"),
                  tags$li(strong("Year clustering:"), " Accounts for common shocks affecting all buyers in a year"),
                  tags$li(strong("None:"), " Assumes all observations are independent (most restrictive)")
                ),
                if (has_rows(bundle$by_cluster)) {
                  renderTable({ bundle$by_cluster }, digits = 3, striped = TRUE, hover = TRUE, bordered = TRUE, width = "auto")
                } else {
                  p(em("No clustering breakdown available."))
                },
                p(em(style = "color: #666;", "Look for: If results remain significant even with buyer clustering (most conservative), the evidence is strong."))
            )
        ),
        
        # By Controls
        div(class = "panel panel-default",
            div(class = "panel-heading", h5(class = "panel-title", style = "margin: 0;", "By Controls")),
            div(class = "panel-body",
                p("Control variables are additional factors that might affect the outcome:"),
                tags$ul(
                  tags$li(strong("x_only:"), " No controls (only missingness and fixed effects)"),
                  tags$li(strong("base:"), " Core controls (contract value, buyer type, procedure type)"),
                  tags$li(strong("base_extra:"), " Extended controls (additional economic/organizational factors)")
                ),
                if (has_rows(bundle$by_controls)) {
                  renderTable({ bundle$by_controls }, digits = 3, striped = TRUE, hover = TRUE, bordered = TRUE, width = "auto")
                } else {
                  p(em("No controls breakdown available."))
                },
                p(em(style = "color: #666;", "Look for: Does the relationship hold even with rich controls? If yes, it's less likely to be driven by omitted variables."))
            )
        ),
        
        # Classification
        div(class = "panel panel-default",
            div(class = "panel-heading", h5(class = "panel-title", style = "margin: 0;", "Classification of Model Results")),
            div(class = "panel-body",
                p("This table shows how many models fall into each category:"),
                if (has_rows(bundle$classes)) {
                  renderTable({ bundle$classes }, digits = 3, striped = TRUE, hover = TRUE, bordered = TRUE, width = "auto")
                } else {
                  p(em("No classification table available."))
                },
                p(em(style = "color: #666;", "Ideal: Most models should be 'Positive & significant'. If many are 'Negative & significant' or evenly split, the evidence is inconclusive."))
            )
        ),
        
        # Top Cells
        div(class = "panel panel-default",
            div(class = "panel-heading", h5(class = "panel-title", style = "margin: 0;", "Top Model Specifications (by success rate)")),
            div(class = "panel-body",
                p("These are the combinations of fixed effects, clustering, and controls that most consistently produce significant results:"),
                if (has_rows(bundle$top_cells)) {
                  renderTable({ bundle$top_cells }, digits = 3, striped = TRUE, hover = TRUE, bordered = TRUE, width = "auto")
                } else {
                  p(em("No top-cells table available."))
                },
                p(em(style = "color: #666;", "Look for: Combinations with high share_pok (share of p-values ≤ 0.10). These are the most reliable specification choices for this dataset."))
            )
        )
      )
    )
  })
  
  # Sensitivity analysis UI output - Relative Price
  output$relprice_sensitivity_table <- renderUI({
    req(results$filtered_analysis$prices$relprice_sensitivity)
    
    bundle <- results$filtered_analysis$prices$relprice_sensitivity
    
    # Check if bundle exists and has overall data
    if (is.null(bundle) || !has_rows(bundle$overall)) {
      return(p("Sensitivity bundle not available."))
    }
    
    # Calculate statistics for conclusion
    n_specs <- get_first_num(bundle$overall, c("n_specs"))
    share_pos <- get_first_num(bundle$overall, c("share_positive"))
    share_neg <- get_first_num(bundle$overall, c("share_negative"))
    median_est <- get_first_num(bundle$overall, c("median_estimate"))
    median_p <- get_first_num(bundle$overall, c("median_pvalue"))
    median_str <- get_first_num(bundle$overall, c("median_strength"))
    share_p05 <- get_first_num(bundle$overall, c("share_p_le_0.05"))
    share_p10 <- get_first_num(bundle$overall, c("share_p_le_0.1", "share_p_le_0.10"))
    share_p20 <- get_first_num(bundle$overall, c("share_p_le_0.2", "share_p_le_0.20"))
    sign_stable <- get_first_num(bundle$sign, c("share_sign_stable"))
    n_nonzero <- get_first_num(bundle$sign, c("n_nonzero"))
    
    # Determine dominant direction
    dominant_dir <- NA_character_
    if (!is.na(share_pos) && !is.na(share_neg)) {
      if (share_pos > share_neg) dominant_dir <- "positive"
      else if (share_neg > share_pos) dominant_dir <- "negative"
      else dominant_dir <- "mixed"
    }
    
    # Generate conclusion text
    conclusion_text <- ""
    conclusion_color <- "info"
    
    if (isTRUE(dominant_dir == "positive" && sign_stable == 1 && share_p10 >= 0.5 && share_pos >= 0.7)) {
      conclusion_text <- "✓ Strong and robust evidence (positive). Missingness is associated with a higher outcome in most specifications, and the direction is stable."
      conclusion_color <- "success"
    } else if (isTRUE(dominant_dir == "negative" && sign_stable == 1 && share_p10 >= 0.5 && share_neg >= 0.7)) {
      conclusion_text <- "✓ Strong and robust evidence (negative). Missingness is associated with a lower outcome in most specifications, and the direction is stable."
      conclusion_color <- "success"
    } else if (isTRUE(dominant_dir %in% c("positive","negative") && share_p10 >= 0.3 &&
                      ((dominant_dir=="positive" && share_pos>=0.6) || (dominant_dir=="negative" && share_neg>=0.6)))) {
      conclusion_text <- "⚠ Moderate evidence. The relationship is mostly in one direction, but statistical significance varies across specifications."
      conclusion_color <- "warning"
    } else if (isTRUE(dominant_dir %in% c("positive","negative") && sign_stable == 0 &&
                      ((dominant_dir=="positive" && share_pos>=0.6) || (dominant_dir=="negative" && share_neg>=0.6)))) {
      conclusion_text <- "⚠ Directionally dominant but unstable. Most estimates point the same way, but the sign flips in some specifications."
      conclusion_color <- "warning"
    } else if (!is.na(dominant_dir)) {
      conclusion_text <- "✗ No robust relationship. Results are mixed or highly sensitive to model specification."
      conclusion_color <- "danger"
    } else {
      conclusion_text <- "ℹ Sensitivity summary not available for this country."
      conclusion_color <- "info"
    }
    
    # Build UI with separate sections
    tagList(
      # Introduction
      div(class = "alert alert-info", style = "margin-bottom: 20px;",
          h4(style = "margin-top: 0;", "What is sensitivity analysis?"),
          p("Sensitivity analysis tests whether the relationship between missing data and the outcome (relative prices) holds across different modeling choices. A robust finding should be consistent across reasonable variations in:"),
          tags$ul(
            tags$li(strong("Fixed effects (FE):"), " Controls for unobserved differences across buyers, years, etc."),
            tags$li(strong("Clustering:"), " Adjusts standard errors for correlation within groups"),
            tags$li(strong("Control variables:"), " Additional factors that might affect the outcome")
          )
      ),
      
      # Overall Summary
      div(class = "panel panel-primary",
          div(class = "panel-heading", h4(class = "panel-title", style = "margin: 0;", "Overall Summary")),
          div(class = "panel-body",
              div(style = "overflow-x: auto; max-width: 100%;",
                  renderTable({
                    summary_panel <- dplyr::bind_cols(bundle$overall, bundle$sign)
                    summary_panel
                  }, digits = 3, striped = TRUE, hover = TRUE, bordered = TRUE, width = "auto")
              )
          )
      ),
      
      # Conclusion Box
      div(class = paste0("alert alert-", conclusion_color), style = "margin-top: 20px; margin-bottom: 20px;",
          h4(style = "margin-top: 0;", "Summary for Decision-Makers"),
          p(strong(conclusion_text)),
          tags$ul(
            if (!is.na(share_pos)) tags$li(scales::percent(share_pos, accuracy = 1), " of models are positive"),
            if (!is.na(share_neg)) tags$li(scales::percent(share_neg, accuracy = 1), " of models are negative"),
            if (!is.na(share_p10)) tags$li(scales::percent(share_p10, accuracy = 1), " of models are statistically significant (p ≤ 0.10)"),
            if (!is.na(median_est)) tags$li("Median effect size: ", sprintf("%.3f", median_est)),
            if (!is.na(median_p)) tags$li("Median p-value: ", sprintf("%.3f", median_p))
          )
      ),
      
      # Detailed breakdown (collapsible)
      tags$details(
        tags$summary(strong("Show detailed breakdown by model components")),
        
        # By Fixed Effects
        div(class = "panel panel-default", style = "margin-top: 15px;",
            div(class = "panel-heading", h5(class = "panel-title", style = "margin: 0;", "By Fixed Effects (FE)")),
            div(class = "panel-body",
                p("Fixed effects control for unobserved heterogeneity. For example:"),
                tags$ul(
                  tags$li(strong("Buyer FE:"), " Controls for buyer-specific characteristics (e.g., capacity, procurement culture)"),
                  tags$li(strong("Year FE:"), " Controls for time trends (e.g., economic conditions, regulatory changes)"),
                  tags$li(strong("Buyer + Year FE:"), " Controls for both simultaneously")
                ),
                if (has_rows(bundle$by_fe)) {
                  renderTable({ bundle$by_fe }, digits = 3, striped = TRUE, hover = TRUE, bordered = TRUE, width = "auto")
                } else {
                  p(em("No fixed-effects breakdown available."))
                },
                p(em(style = "color: #666;", "Look for: Which fixed effect structure gives the most significant results? If results hold across all FE specifications, the finding is very robust."))
            )
        ),
        
        # By Clustering
        div(class = "panel panel-default",
            div(class = "panel-heading", h5(class = "panel-title", style = "margin: 0;", "By Clustering")),
            div(class = "panel-body",
                p("Clustering adjusts standard errors for correlation within groups:"),
                tags$ul(
                  tags$li(strong("Buyer clustering:"), " Accounts for correlation within the same buyer over time"),
                  tags$li(strong("Year clustering:"), " Accounts for common shocks affecting all buyers in a year"),
                  tags$li(strong("None:"), " Assumes all observations are independent (most restrictive)")
                ),
                if (has_rows(bundle$by_cluster)) {
                  renderTable({ bundle$by_cluster }, digits = 3, striped = TRUE, hover = TRUE, bordered = TRUE, width = "auto")
                } else {
                  p(em("No clustering breakdown available."))
                },
                p(em(style = "color: #666;", "Look for: If results remain significant even with buyer clustering (most conservative), the evidence is strong."))
            )
        ),
        
        # By Controls
        div(class = "panel panel-default",
            div(class = "panel-heading", h5(class = "panel-title", style = "margin: 0;", "By Controls")),
            div(class = "panel-body",
                p("Control variables are additional factors that might affect the outcome:"),
                tags$ul(
                  tags$li(strong("x_only:"), " No controls (only missingness and fixed effects)"),
                  tags$li(strong("base:"), " Core controls (contract value, buyer type, procedure type)"),
                  tags$li(strong("base_extra:"), " Extended controls (additional economic/organizational factors)")
                ),
                if (has_rows(bundle$by_controls)) {
                  renderTable({ bundle$by_controls }, digits = 3, striped = TRUE, hover = TRUE, bordered = TRUE, width = "auto")
                } else {
                  p(em("No controls breakdown available."))
                },
                p(em(style = "color: #666;", "Look for: Does the relationship hold even with rich controls? If yes, it's less likely to be driven by omitted variables."))
            )
        ),
        
        # Classification
        div(class = "panel panel-default",
            div(class = "panel-heading", h5(class = "panel-title", style = "margin: 0;", "Classification of Model Results")),
            div(class = "panel-body",
                p("This table shows how many models fall into each category:"),
                if (has_rows(bundle$classes)) {
                  renderTable({ bundle$classes }, digits = 3, striped = TRUE, hover = TRUE, bordered = TRUE, width = "auto")
                } else {
                  p(em("No classification table available."))
                },
                p(em(style = "color: #666;", "Ideal: Most models should be 'Positive & significant'. If many are 'Negative & significant' or evenly split, the evidence is inconclusive."))
            )
        ),
        
        # Top Cells
        div(class = "panel panel-default",
            div(class = "panel-heading", h5(class = "panel-title", style = "margin: 0;", "Top Model Specifications (by success rate)")),
            div(class = "panel-body",
                p("These are the combinations of fixed effects, clustering, and controls that most consistently produce significant results:"),
                if (has_rows(bundle$top_cells)) {
                  renderTable({ bundle$top_cells }, digits = 3, striped = TRUE, hover = TRUE, bordered = TRUE, width = "auto")
                } else {
                  p(em("No top-cells table available."))
                },
                p(em(style = "color: #666;", "Look for: Combinations with high share_pok (share of p-values ≤ 0.10). These are the most reliable specification choices for this dataset."))
            )
        )
      )
    )
  })
  # Store filter states for each tab
  filters <- reactiveValues(
    overview = list(year = NULL, market = NULL, value = NULL, buyer_type = NULL, procedure_type = NULL),
    missing = list(year = NULL, market = NULL, value = NULL, buyer_type = NULL, procedure_type = NULL),
    interop = list(year = NULL, market = NULL, value = NULL, buyer_type = NULL, procedure_type = NULL),
    risky = list(year = NULL, market = NULL, value = NULL, buyer_type = NULL, procedure_type = NULL),
    prices = list(year = NULL, market = NULL, value = NULL, buyer_type = NULL, procedure_type = NULL)
  )
  
  
  # ========================================================================
  # 4: Add Filter Info to Download Filenames
  # Add this helper function near the top of the server section
  # ========================================================================
  
  get_filter_caption <- function(tab_filters) {
    if (is.null(tab_filters)) return("")
    
    parts <- c()
    
    if (!is.null(tab_filters$year)) {
      parts <- c(parts, paste0("Years: ", tab_filters$year[1], "-", tab_filters$year[2]))
    }
    
    if (!is.null(tab_filters$market) && length(tab_filters$market) > 0 && "All" %ni% tab_filters$market) {
      if (length(tab_filters$market) <= 3) {
        parts <- c(parts, paste0("Markets: ", paste(tab_filters$market, collapse = ", ")))
      } else {
        parts <- c(parts, paste0("Markets: ", length(tab_filters$market), " selected"))
      }
    }
    
    if (!is.null(tab_filters$value)) {
      min_val <- tab_filters$value[1]
      max_val <- tab_filters$value[2]
      
      if (max_val >= 1e9) {
        parts <- c(parts, sprintf("Value: $%.1fB - $%.1fB", min_val/1e9, max_val/1e9))
      } else if (max_val >= 1e6) {
        parts <- c(parts, sprintf("Value: $%.1fM - $%.1fM", min_val/1e6, max_val/1e6))
      } else if (max_val >= 1e3) {
        parts <- c(parts, sprintf("Value: $%.0fK - $%.0fK", min_val/1e3, max_val/1e3))
      } else {
        parts <- c(parts, sprintf("Value: $%.0f - $%.0f", min_val, max_val))
      }
    }
    
    if (!is.null(tab_filters$buyer_type) && length(tab_filters$buyer_type) > 0 && "All" %ni% tab_filters$buyer_type) {
      if (length(tab_filters$buyer_type) <= 2) {
        parts <- c(parts, paste0("Buyers: ", paste(tab_filters$buyer_type, collapse = ", ")))
      } else {
        parts <- c(parts, paste0("Buyers: ", length(tab_filters$buyer_type), " types"))
      }
    }
    
    # ← ADD THIS SECTION FOR PROCEDURE TYPE
    if (!is.null(tab_filters$procedure_type) && length(tab_filters$procedure_type) > 0 && "All" %ni% tab_filters$procedure_type) {
      if (length(tab_filters$procedure_type) <= 2) {
        parts <- c(parts, paste0("Procedure: ", paste(tab_filters$procedure_type, collapse = ", ")))
      } else {
        parts <- c(parts, paste0("Procedure: ", length(tab_filters$procedure_type), " types"))
      }
    }
    
    if (length(parts) > 0) {
      return(paste("\nFilters Applied:", paste(parts, collapse = " | ")))
    } else {
      return("")
    }
  }
  # ========================================================================
  # DATA LOADING AND ANALYSIS
  # ========================================================================
  
  observeEvent(input$run_analysis, {
    req(input$datafile, input$country_code)
    
    # Store country code
    results$country_code <- input$country_code
    
    # Update status display
    results$status <- "🔄 Loading data..."
    
    tryCatch({
      withProgress(message = 'Running integrity analysis', value = 0, {
        # Load data
        incProgress(0.1, detail = "Loading CSV file...")
        df <- load_data(input$datafile$datapath)
        results$status <- paste0("✓ Data loaded: ", nrow(df), " rows\n🔄 Preparing data...")
        
        # Create temporary output directory
        output_dir <- tempdir()
        
        incProgress(0.2, detail = "Validating data structure...")
        results$status <- paste0("✓ Data loaded: ", nrow(df), " rows\n🔄 Running analysis pipeline...")
        
        # Run the full pipeline (this is where most time is spent)
        incProgress(0.3, detail = "Running all analysis modules (this may take several minutes)...")
        results$status <- paste0("✓ Data loaded: ", nrow(df), " rows\n🔄 Running all analysis modules...\n  • Missing values\n  • Interoperability\n  • Competition\n  • Markets\n  • Prices\n  • Regional patterns")
        
        # CRITICAL: Run pipeline FIRST (this creates tender_year!)
        analysis_results <- run_integrity_pipeline(
          df = df,
          country_code = input$country_code,
          output_dir = output_dir
        )
        
        incProgress(0.9, detail = "Finalizing results...")
        results$status <- paste0("✓ Data loaded: ", nrow(df), " rows\n🔄 Finalizing results...")
        
        # The pipeline returns PROCESSED data with tender_year
        # Store this as the base data
        results$data <- analysis_results$data  # This has tender_year!
        results$analysis <- analysis_results
        results$filtered_data <- analysis_results$data  # Initialize filtered data
        results$filtered_analysis <- analysis_results
        
        incProgress(1.0, detail = "Complete!")
        
        results$status <- paste0(
          "✓ Analysis complete!\n",
          "  • Country: ", input$country_code, "\n",
          "  • Total contracts: ", formatC(nrow(analysis_results$data), format = "d", big.mark = ","), "\n",
          "  • Unique buyers: ", formatC(length(unique(analysis_results$data$buyer_masterid)), format = "d", big.mark = ","), "\n",
          "  • Unique suppliers: ", formatC(length(unique(analysis_results$data$bidder_masterid)), format = "d", big.mark = ","), "\n",
          "  • Ready for exploration!"
        )
        
        showNotification("Analysis completed successfully! You can now explore the results in other tabs.", 
                         type = "message", duration = 5)
      })
      
    }, error = function(e) {
      results$status <- paste0("❌ Error: ", e$message)
      showNotification(paste("Error:", e$message), type = "error", duration = 10)
    })
  })
  
  # Display analysis status
  output$analysis_status <- renderText({
    results$status
  })
  
  # ========================================================================
  # FILTER UI GENERATION
  # ========================================================================
  
  # Year filter
  output$year_filter_overview <- output$year_filter_missing <- 
    output$year_filter_interop <- output$year_filter_risky <- 
    output$year_filter_prices <- renderUI({
      req(results$data)
      
      year_col <- NULL
      if ("tender_year" %in% names(results$data)) {
        year_col <- "tender_year"
      } else if ("year" %in% names(results$data)) {
        year_col <- "year"
      } else if ("cal_year" %in% names(results$data)) {
        year_col <- "cal_year"
      }
      
      if (!is.null(year_col)) {
        years <- sort(unique(results$data[[year_col]]))
        years <- years[!is.na(years)]
        
        if (length(years) > 0) {
          # Check if we have stored filter values for this tab
          current_tab <- input$sidebar
          stored_years <- NULL
          
          if (current_tab == "overview" && !is.null(filters$overview$year)) {
            stored_years <- filters$overview$year
          } else if (current_tab == "missing" && !is.null(filters$missing$year)) {
            stored_years <- filters$missing$year
          } else if (current_tab == "interoperability" && !is.null(filters$interop$year)) {
            stored_years <- filters$interop$year
          } else if (current_tab == "risky" && !is.null(filters$risky$year)) {
            stored_years <- filters$risky$year
          } else if (current_tab == "prices_competition" && !is.null(filters$prices$year)) {
            stored_years <- filters$prices$year
          }
          
          # Use stored values if available, otherwise use full range
          year_value <- if (!is.null(stored_years)) {
            stored_years
          } else {
            c(min(years), max(years))
          }
          
          sliderInput("year_range", "Year Range:", 
                      min = min(years), 
                      max = max(years),
                      value = year_value,  # ← Now uses stored values!
                      step = 1, sep = "")
        }
      }
    })
  
  # Market filter (CPV 2-digit)
  output$market_filter_overview <- output$market_filter_missing <- 
    output$market_filter_interop <- output$market_filter_risky <- 
    output$market_filter_prices <- renderUI({
      req(results$data)
      if ("lot_productcode" %in% names(results$data)) {
        cpv_2dig <- results$data %>%
          mutate(cpv_2dig = substr(lot_productcode, 1, 2)) %>%
          filter(!is.na(cpv_2dig)) %>%
          pull(cpv_2dig) %>%
          unique() %>%
          sort()
        
        pickerInput("market_filter", "Market (CPV 2-digit):",
                    choices = c("All", cpv_2dig),
                    selected = "All",
                    multiple = TRUE,
                    options = list(`actions-box` = TRUE, `live-search` = TRUE))
      }
    })
  
  # Contract value filter
  output$value_filter_overview <- output$value_filter_missing <- 
    output$value_filter_overview <- output$value_filter_missing <- 
    output$value_filter_interop <- output$value_filter_risky <- 
    output$value_filter_prices <- renderUI({
      req(results$data)
      
      price_col <- NULL
      if ("bid_priceusd" %in% names(results$data)) {
        price_col <- "bid_priceusd"
      } else if ("bid_price" %in% names(results$data)) {
        price_col <- "bid_price"
      }
      
      if (!is.null(price_col)) {
        prices <- results$data[[price_col]]
        prices <- prices[!is.na(prices) & prices > 0]
        
        if (length(prices) > 0) {
          min_val <- 0
          max_val_raw <- quantile(prices, 0.99, na.rm = TRUE)
          
          # Round and scale based on magnitude
          if (max_val_raw >= 1e9) {
            max_val <- round(max_val_raw / 1e9, 1) * 1e9
            step_val <- 1e8
            label_text <- "Contract Value Range (Billions USD):"
            pre_text <- "$"
            post_text <- "B"
            divisor <- 1e9
            results$value_divisor <- divisor  # ← ADD THIS LINE
          } else if (max_val_raw >= 1e8) {
            max_val <- round(max_val_raw / 1e7) * 1e7
            step_val <- 1e7
            label_text <- "Contract Value Range (Millions USD):"
            pre_text <- "$"
            post_text <- "M"
            divisor <- 1e6
            results$value_divisor <- divisor  # ← ADD THIS LINE
          } else if (max_val_raw >= 1e7) {
            max_val <- round(max_val_raw / 1e6) * 1e6
            step_val <- 1e6
            label_text <- "Contract Value Range (Millions USD):"
            pre_text <- "$"
            post_text <- "M"
            divisor <- 1e6
            results$value_divisor <- divisor  # ← ADD THIS LINE
          } else if (max_val_raw >= 1e6) {
            max_val <- round(max_val_raw / 1e5) * 1e5
            step_val <- 1e5
            label_text <- "Contract Value Range (Millions USD):"
            pre_text <- "$"
            post_text <- "M"
            divisor <- 1e6
            results$value_divisor <- divisor  # ← ADD THIS LINE
          } else if (max_val_raw >= 100000) {
            max_val <- round(max_val_raw / 1e4) * 1e4
            step_val <- 1e4
            label_text <- "Contract Value Range (Thousands USD):"
            pre_text <- "$"
            post_text <- "K"
            divisor <- 1e3
            results$value_divisor <- divisor  # ← ADD THIS LINE
          } else {
            max_val <- round(max_val_raw / 1e3) * 1e3
            step_val <- 1e3
            label_text <- "Contract Value Range (USD):"
            pre_text <- "$"
            post_text <- ""
            divisor <- 1
            results$value_divisor <- divisor  # ← ADD THIS LINE
          }
          
          # Create slider with scaled values
          sliderInput("value_range", label_text,
                      min = 0,
                      max = max_val / divisor,
                      value = c(0, max_val / divisor),
                      step = step_val / divisor,
                      pre = pre_text,
                      post = post_text,
                      sep = ",")
        }
      }
    })
  
  
  # Buyer type filter (using buyer_group labels)
  output$buyer_type_filter_overview <- output$buyer_type_filter_missing <- 
    output$buyer_type_filter_interop <- output$buyer_type_filter_risky <- 
    output$buyer_type_filter_prices <- renderUI({
      req(results$data)
      if ("buyer_buyertype" %in% names(results$data)) {
        # Get buyer groups using the utility function
        buyer_groups <- results$data %>%
          mutate(buyer_group = add_buyer_group(buyer_buyertype)) %>%
          pull(buyer_group) %>%
          as.character() %>%
          unique() %>%
          sort()
        
        buyer_groups <- buyer_groups[!is.na(buyer_groups)]
        
        if (length(buyer_groups) > 0) {
          pickerInput("buyer_type_filter", "Buyer Type:",
                      choices = c("All", buyer_groups),
                      selected = "All",
                      multiple = TRUE,
                      options = list(`actions-box` = TRUE))
        }
      }
    })
  
  # Procedure type filter (using procedure_type_labels)
  output$procedure_type_filter_overview <- output$procedure_type_filter_missing <- 
    output$procedure_type_filter_interop <- output$procedure_type_filter_risky <- 
    output$procedure_type_filter_prices <- renderUI({
      req(results$data)
      if ("tender_proceduretype" %in% names(results$data)) {
        # Get unique procedure types
        proc_types <- unique(results$data$tender_proceduretype)
        proc_types <- proc_types[!is.na(proc_types)]
        
        # Define procedure type labels (from integrity_utils.R)
        procedure_labels <- c(
          "OPEN" = "Open (competitive bidding)",
          "RESTRICTED" = "Restricted (limited competition)",
          "NEGOTIATED_WITH_PUBLICATION" = "Negotiated with publication",
          "NEGOTIATED_WITHOUT_PUBLICATION" = "Negotiated without publication",
          "NEGOTIATED" = "Negotiated (limited competition)",
          "COMPETITIVE_DIALOG" = "Competitive dialogue",
          "OUTRIGHT_AWARD" = "Outright award (direct purchase)",
          "OTHER" = "Other"
        )
        
        if (length(proc_types) > 0) {
          # Create labeled choices
          proc_choices <- setNames(proc_types, 
                                   ifelse(proc_types %in% names(procedure_labels),
                                          procedure_labels[proc_types],
                                          proc_types))
          
          pickerInput("procedure_type_filter", "Procedure Type:",
                      choices = c("All" = "All", proc_choices),
                      selected = "All",
                      multiple = TRUE,
                      options = list(`actions-box` = TRUE, `live-search` = TRUE))
        }
      }
    })
  observeEvent(input$sidebar, {
    current_tab <- input$sidebar
    
    tab_filters <- switch(current_tab,
                          "overview" = filters$overview,
                          "data_overview" = filters$overview,
                          "missing" = filters$missing,
                          "interoperability" = filters$interop,
                          "risky" = filters$risky,
                          "prices_competition" = filters$prices,
                          NULL
    )
    
    if (is.null(tab_filters)) return()
    
    if (!is.null(tab_filters$year) && !is.null(input$year_range)) {
      updateSliderInput(session, "year_range", value = tab_filters$year)
    }
    
    if (!is.null(tab_filters$market) && !is.null(input$market_filter)) {
      updatePickerInput(session, "market_filter", selected = tab_filters$market)
    }
    
    if (!is.null(tab_filters$value) && !is.null(input$value_range)) {
      updateSliderInput(session, "value_range", value = tab_filters$value)
    }
    
    if (!is.null(tab_filters$buyer_type) && !is.null(input$buyer_type_filter)) {
      updatePickerInput(session, "buyer_type_filter", selected = tab_filters$buyer_type)
    }
    
    if (!is.null(tab_filters$procedure_type) && !is.null(input$procedure_type_filter)) {
      updatePickerInput(session, "procedure_type_filter", selected = tab_filters$procedure_type)
    }
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  # ========================================================================
  # FILTER APPLICATION
  # ========================================================================
  
  # Helper function to get filter description
  get_filter_description <- function(filters_list) {
    parts <- c()
    
    if (!is.null(filters_list$year)) {
      parts <- c(parts, paste0("Years: ", filters_list$year[1], "-", filters_list$year[2]))
    }
    
    if (!is.null(filters_list$market) && length(filters_list$market) > 0 && "All" %ni% filters_list$market) {
      if (length(filters_list$market) <= 3) {
        parts <- c(parts, paste0("Markets: ", paste(filters_list$market, collapse = ", ")))
      } else {
        parts <- c(parts, paste0("Markets: ", length(filters_list$market), " selected"))
      }
    }
    
    if (!is.null(filters_list$value)) {
      parts <- c(parts, paste0("Value: ", scales::dollar(filters_list$value[1]), " - ", 
                               scales::dollar(filters_list$value[2])))
    }
    
    if (!is.null(filters_list$buyer_type) && length(filters_list$buyer_type) > 0 && "All" %ni% filters_list$buyer_type) {
      if (length(filters_list$buyer_type) <= 2) {
        parts <- c(parts, paste0("Buyer: ", paste(filters_list$buyer_type, collapse = ", ")))
      } else {
        parts <- c(parts, paste0("Buyer: ", length(filters_list$buyer_type), " selected"))
      }
    }
    
    if (!is.null(filters_list$procedure_type) && length(filters_list$procedure_type) > 0 && "All" %ni% filters_list$procedure_type) {
      if (length(filters_list$procedure_type) <= 2) {
        parts <- c(parts, paste0("Procedure: ", paste(filters_list$procedure_type, collapse = ", ")))
      } else {
        parts <- c(parts, paste0("Procedure: ", length(filters_list$procedure_type), " selected"))
      }
    }
    
    if (length(parts) == 0) {
      return("No filters applied")
    } else {
      return(paste(parts, collapse = " | "))
    }
  }
  
  # Apply filters for each tab
  apply_tab_filters <- function(tab_name) {
    req(results$data, results$analysis)
    
    # Get filter values
    year_range <- input$year_range
    market <- input$market_filter
    value_range <- input$value_range
    buyer_type <- input$buyer_type_filter
    procedure_type <- input$procedure_type_filter
    
    # Store filters
    filters[[tab_name]] <- list(
      year = year_range,
      market = market,
      value = value_range,
      buyer_type = buyer_type,
      procedure_type = procedure_type
    )
    
    # ✅ CRITICAL: Pass the divisor to filter_data!
    filtered_df <- filter_data(
      results$data,
      year_range = year_range,
      market = market,
      value_range = value_range,
      buyer_type = buyer_type,
      procedure_type = procedure_type,
      value_divisor = results$value_divisor  # ADD THIS PARAMETER
    )
    
    # Check if any data remains
    if (nrow(filtered_df) == 0) {
      showNotification("No data matches the selected filters. Showing unfiltered results.", 
                       type = "warning", duration = 5)
      return(NULL)
    }
    
    showNotification(paste0("Filters applied! ", nrow(filtered_df), " contracts match."), 
                     type = "message", duration = 3)
    
    # Re-run analysis on filtered data
    output_dir <- tempdir()
    
    withProgress(message = 'Re-running analysis with filters...', value = 0, {
      incProgress(0.2, detail = "Preparing filtered dataset...")
      
      incProgress(0.3, detail = "Running analysis modules (this may take a few minutes)...")
      
      filtered_analysis <- run_integrity_pipeline(
        df = filtered_df,
        country_code = results$country_code,
        output_dir = output_dir
      )
      
      incProgress(0.9, detail = "Finalizing filtered results...")
      
      # Store filtered results
      results$filtered_data <- filtered_analysis$data
      results$filtered_analysis <- filtered_analysis
      
      incProgress(1.0, detail = "Complete!")
    })
  }
  
  # Filter button observers
  observeEvent(input$apply_filters_overview, { apply_tab_filters("overview") })
  observeEvent(input$apply_filters_missing, { apply_tab_filters("missing") })
  observeEvent(input$apply_filters_interop, { apply_tab_filters("interop") })
  observeEvent(input$apply_filters_risky, { apply_tab_filters("risky") })
  observeEvent(input$apply_filters_prices, { apply_tab_filters("prices") })
  
  # Reset filter observers
  observeEvent(input$reset_filters_overview, {
    filters$overview <- list(year = NULL, market = NULL, value = NULL, buyer_type = NULL, procedure_type = NULL)
    results$filtered_data <- results$data
    results$filtered_analysis <- results$analysis
    showNotification("Filters reset", type = "message", duration = 2)
  })
  
  observeEvent(input$reset_filters_missing, {
    filters$missing <- list(year = NULL, market = NULL, value = NULL, buyer_type = NULL, procedure_type = NULL)
    results$filtered_data <- results$data
    results$filtered_analysis <- results$analysis
    showNotification("Filters reset", type = "message", duration = 2)
  })
  
  observeEvent(input$reset_filters_interop, {
    filters$interop <- list(year = NULL, market = NULL, value = NULL, buyer_type = NULL, procedure_type = NULL)
    results$filtered_data <- results$data
    results$filtered_analysis <- results$analysis
    showNotification("Filters reset", type = "message", duration = 2)
  })
  
  observeEvent(input$reset_filters_risky, {
    filters$risky <- list(year = NULL, market = NULL, value = NULL, buyer_type = NULL, procedure_type = NULL)
    results$filtered_data <- results$data
    results$filtered_analysis <- results$analysis
    showNotification("Filters reset", type = "message", duration = 2)
  })
  
  observeEvent(input$reset_filters_prices, {
    filters$prices <- list(year = NULL, market = NULL, value = NULL, buyer_type = NULL, procedure_type = NULL)
    results$filtered_data <- results$data
    results$filtered_analysis <- results$analysis
    showNotification("Filters reset", type = "message", duration = 2)
  })
  
  # Filter status outputs
  output$filter_status_overview <- renderText({
    paste("  ", get_filter_description(filters$overview))
  })
  
  output$filter_status_missing <- renderText({
    paste("  ", get_filter_description(filters$missing))
  })
  
  output$filter_status_interop <- renderText({
    paste("  ", get_filter_description(filters$interop))
  })
  
  output$filter_status_risky <- renderText({
    paste("  ", get_filter_description(filters$risky))
  })
  
  output$filter_status_prices <- renderText({
    paste("  ", get_filter_description(filters$prices))
  })
  
  # Export filter info
  output$export_filter_info <- renderUI({
    # Check which tab filters are active
    active_filters <- list()
    
    if (get_filter_description(filters$overview) != "No filters applied") {
      active_filters$overview <- get_filter_description(filters$overview)
    }
    if (get_filter_description(filters$missing) != "No filters applied") {
      active_filters$missing <- get_filter_description(filters$missing)
    }
    if (get_filter_description(filters$risky) != "No filters applied") {
      active_filters$risky <- get_filter_description(filters$risky)
    }
    if (get_filter_description(filters$prices) != "No filters applied") {
      active_filters$prices <- get_filter_description(filters$prices)
    }
    
    if (length(active_filters) == 0) {
      p(icon("info-circle"), " No filters are currently applied. Reports will include all data.")
    } else {
      tagList(
        p(icon("filter"), tags$b(" Active filters detected. Reports will reflect filtered data:")),
        tags$ul(
          lapply(names(active_filters), function(tab) {
            tags$li(tags$b(tab, ":"), active_filters[[tab]])
          })
        )
      )
    }
  })
  
  
  # ========================================================================
  # DATA OVERVIEW OUTPUTS (using filtered data)
  # ========================================================================
  
  output$n_contracts <- renderValueBox({
    req(results$filtered_data)
    valueBox(
      formatC(nrow(results$filtered_data), format = "d", big.mark = ","),
      "Total Contracts",
      icon = icon("file-contract"),
      color = "blue"
    )
  })
  
  output$n_buyers <- renderValueBox({
    req(results$filtered_data)
    n <- if ("buyer_masterid" %in% names(results$filtered_data)) {
      n_distinct(results$filtered_data$buyer_masterid, na.rm = TRUE)
    } else {
      NA
    }
    valueBox(
      formatC(n, format = "d", big.mark = ","),
      "Unique Buyers",
      icon = icon("building"),
      color = "green"
    )
  })
  
  output$n_suppliers <- renderValueBox({
    req(results$filtered_data)
    n <- if ("bidder_masterid" %in% names(results$filtered_data)) {
      n_distinct(results$filtered_data$bidder_masterid, na.rm = TRUE)
    } else {
      NA
    }
    valueBox(
      formatC(n, format = "d", big.mark = ","),
      "Unique Suppliers",
      icon = icon("truck"),
      color = "yellow"
    )
  })
  
  output$n_years <- renderValueBox({
    req(results$filtered_data)
    
    year_col <- NULL
    if ("tender_year" %in% names(results$filtered_data)) {
      year_col <- "tender_year"
    } else if ("year" %in% names(results$filtered_data)) {
      year_col <- "year"  
    } else if ("cal_year" %in% names(results$filtered_data)) {
      year_col <- "cal_year"
    }
    
    n <- if (!is.null(year_col)) {
      years <- unique(results$filtered_data[[year_col]])
      years <- years[!is.na(years)]  # CRITICAL: Remove NAs before counting
      length(years)
    } else {
      NA
    }
    
    valueBox(
      ifelse(is.na(n) || n == 0, "N/A", as.character(n)),  # CRITICAL: Convert to character
      "Years Covered",
      icon = icon("calendar"),
      color = "red"
    )
  })
  
  output$contracts_year_plot <- renderPlot({
    req(results$filtered_analysis)
    stats <- results$filtered_analysis$summary_stats$n_obs_per_year
    
    ggplot(stats, aes(x = tender_year, y = n_observations)) +
      geom_col(fill = "#3c8dbc") +
      geom_text(aes(label = formatC(n_observations, format = "d", big.mark = ",")),
                vjust = -0.5, size = 4) +
      labs(x = "Year", y = "Number of Contracts", title = "Contracts per Year") +
      standard_plot_theme() +
      scale_y_continuous(labels = scales::comma)
  })
  
  output$summary_table <- DT::renderDataTable({
    req(results$filtered_analysis)
    stats <- results$filtered_analysis$summary_stats$n_obs_per_year
    
    DT::datatable(
      stats,
      options = list(pageLength = 10, dom = 't'),
      rownames = FALSE
    ) %>%
      DT::formatCurrency("n_observations", currency = "", digits = 0)
  })
  
  output$variables_list <- renderUI({
    req(results$filtered_analysis)
    vars <- results$filtered_analysis$summary_stats$vars_present
    
    HTML(paste("<ul>",
               paste0("<li>", vars, "</li>", collapse = ""),
               "</ul>"))
  })
  
  # ========================================================================
  # MISSING VALUES OUTPUTS (using filtered analysis)
  # ========================================================================
  
  output$missing_overall_plot <- renderPlot({
    req(results$filtered_analysis$missing$overall_plot)
    results$filtered_analysis$missing$overall_plot
  })
  
  output$missing_time_plot <- renderPlot({
    req(results$filtered_analysis$missing$by_year_plot)
    results$filtered_analysis$missing$by_year_plot
  })
  
  output$missing_buyer_plot <- renderPlot({
    req(results$filtered_analysis$missing$by_buyer_plot)
    results$filtered_analysis$missing$by_buyer_plot
  })
  
  output$missing_procedure_plot <- renderPlot({
    req(results$filtered_analysis$missing$by_procedure_plot)
    results$filtered_analysis$missing$by_procedure_plot
  })
  
  # ========================================================================
  # INTEROPERABILITY OUTPUTS (using filtered analysis)
  # ========================================================================
  
  output$interoperability_table <- DT::renderDataTable({
    req(results$filtered_analysis$interoperability$org_missing)
    
    org_data <- results$filtered_analysis$interoperability$org_missing %>%
      mutate(
        `Missing share` = ifelse(
          is.na(missing_share),
          "Not available (variable missing in dataset)",
          scales::percent(missing_share, accuracy = 1)
        )
      ) %>%
      select(
        `Organization type` = organization_type,
        `ID type` = id_type,
        `Missing share`
      )
    
    DT::datatable(
      org_data,
      options = list(pageLength = 10, dom = 't'),
      rownames = FALSE
    )
  })
  
  # ========================================================================
  # RISKY PROFILES OUTPUTS (using filtered analysis)
  # ========================================================================
  
  output$network_plot <- renderPlot({
    req(results$filtered_analysis$markets$network_plot)
    results$filtered_analysis$markets$network_plot
  })
  
  output$supplier_unusual_plot <- renderPlot({
    req(results$filtered_analysis$markets$supplier_unusual_plot)
    results$filtered_analysis$markets$supplier_unusual_plot
  })
  
  output$market_unusual_plot <- renderPlot({
    req(results$filtered_analysis$markets$market_unusual_plot)
    results$filtered_analysis$markets$market_unusual_plot
  })
  
  output$concentration_plot <- renderPlot({
    req(results$filtered_analysis$competition$concentration_yearly_plot)
    results$filtered_analysis$competition$concentration_yearly_plot
  })
  
  # ========================================================================
  # PRICES & COMPETITION OUTPUTS (using filtered analysis)
  # ========================================================================
  
  output$singleb_plot_ui <- renderUI({
    if (!is.null(results$filtered_analysis$prices$singleb_plot)) {
      plotOutput("singleb_plot", height = "500px")
    } else {
      div(class = "alert alert-warning",
          style = "padding: 20px; margin: 20px;",
          icon("info-circle"),
          " The single-bidding model could not be produced with the current filters.")
    }
  })
  
  output$singleb_plot <- renderPlot({
    req(results$filtered_analysis$prices$singleb_plot)
    results$filtered_analysis$prices$singleb_plot
  })
  
  output$download_singleb_ui <- renderUI({
    if (!is.null(results$filtered_analysis$prices$singleb_plot)) {
      downloadButton("download_singleb", "Download Figure", class = "download-btn btn-sm")
    }
  })
  
  output$relprice_plot_ui <- renderUI({
    if (!is.null(results$filtered_analysis$prices$rel_price_plot)) {
      plotOutput("relprice_plot", height = "500px")
    } else {
      div(class = "alert alert-warning",
          style = "padding: 20px; margin: 20px;",
          icon("info-circle"),
          " The relative price model could not be produced with the current filters.")
    }
  })
  
  output$relprice_plot <- renderPlot({
    req(results$filtered_analysis$prices$rel_price_plot)
    results$filtered_analysis$prices$rel_price_plot
  })
  
  output$download_relprice_ui <- renderUI({
    if (!is.null(results$filtered_analysis$prices$rel_price_plot)) {
      downloadButton("download_relprice", "Download Figure", class = "download-btn btn-sm")
    }
  })
  
  # ========================================================================
  # DOWNLOAD HANDLERS - INDIVIDUAL FIGURES
  # ========================================================================
  
  output$download_contracts_year <- downloadHandler(
    filename = function() { paste0("contracts_per_year_", results$country_code, ".png") },
    content = function(file) {
      req(results$filtered_analysis)
      stats <- results$filtered_analysis$summary_stats$n_obs_per_year
      p <- ggplot(stats, aes(x = tender_year, y = n_observations)) +
        geom_col(fill = "#3c8dbc") +
        geom_text(aes(label = formatC(n_observations, format = "d", big.mark = ",")),
                  vjust = -0.5, size = 4) +
        labs(x = "Year", y = "Number of Contracts", title = "Contracts per Year") +
        standard_plot_theme() +
        scale_y_continuous(labels = scales::comma)
      ggsave(file, p, width = 10, height = 8, dpi = 300)
    }
  )
  
  output$download_missing_overall <- downloadHandler(
    filename = function() { paste0("missing_overall_", results$country_code, ".png") },
    content = function(file) {
      req(results$filtered_analysis$missing$overall_plot)
      
      filter_caption <- get_filter_caption(filters$missing)
      
      plot_to_save <- if (filter_caption != "") {
        results$filtered_analysis$missing$overall_plot + labs(caption = filter_caption)
      } else {
        results$filtered_analysis$missing$overall_plot
      }
      
      ggsave(file, plot_to_save, width = 10, height = 8, dpi = 300)
    }
  )
  
  output$download_missing_buyer <- downloadHandler(
    filename = function() { paste0("missing_buyer_", results$country_code, ".png") },
    content = function(file) {
      req(results$filtered_analysis$missing$by_buyer_plot)
      filter_caption <- get_filter_caption(filters$missing)
      plot_to_save <- if (filter_caption != "") {
        results$filtered_analysis$missing$by_buyer_plot + labs(caption = filter_caption)
      } else {
        results$filtered_analysis$missing$by_buyer_plot
      }
      ggsave(file, plot_to_save, width = 12, height = 10, dpi = 300)
    }
  )
  
  output$download_missing_procedure <- downloadHandler(
    filename = function() { paste0("missing_procedure_", results$country_code, ".png") },
    content = function(file) {
      req(results$filtered_analysis$missing$by_procedure_plot)
      filter_caption <- get_filter_caption(filters$missing)
      plot_to_save <- if (filter_caption != "") {
        results$filtered_analysis$missing$by_procedure_plot + labs(caption = filter_caption)
      } else {
        results$filtered_analysis$missing$by_procedure_plot
      }
      ggsave(file, plot_to_save, width = 10, height = 6, dpi = 300)
    }
  )
  
  # Full replacement for download_missing_time (Line ~1900):
  output$download_missing_time <- downloadHandler(
    filename = function() { paste0("missing_time_", results$country_code, ".png") },
    content = function(file) {
      req(results$filtered_analysis$missing$by_year_plot)
      filter_caption <- get_filter_caption(filters$missing)
      plot_to_save <- if (filter_caption != "") {
        results$filtered_analysis$missing$by_year_plot + labs(caption = filter_caption)
      } else {
        results$filtered_analysis$missing$by_year_plot
      }
      ggsave(file, plot_to_save, width = 10, height = 6, dpi = 300)
    }
  )
  
  # Full replacement for download_network (Line ~1907):
  output$download_network <- downloadHandler(
    filename = function() { paste0("network_", results$country_code, ".png") },
    content = function(file) {
      req(results$filtered_analysis$markets$network_plot)
      filter_caption <- get_filter_caption(filters$risky)
      plot_to_save <- if (filter_caption != "") {
        results$filtered_analysis$markets$network_plot + labs(caption = filter_caption)
      } else {
        results$filtered_analysis$markets$network_plot
      }
      ggsave(file, plot_to_save, width = 12, height = 12, dpi = 300)
    }
  )
  
  # Full replacement for download_supplier_unusual (Line ~1914):
  output$download_supplier_unusual <- downloadHandler(
    filename = function() { paste0("supplier_unusual_", results$country_code, ".png") },
    content = function(file) {
      req(results$filtered_analysis$markets$supplier_unusual_plot)
      filter_caption <- get_filter_caption(filters$risky)
      plot_to_save <- if (filter_caption != "") {
        results$filtered_analysis$markets$supplier_unusual_plot + labs(caption = filter_caption)
      } else {
        results$filtered_analysis$markets$supplier_unusual_plot
      }
      ggsave(file, plot_to_save, width = 10, height = 8, dpi = 300)
    }
  )
  
  # Full replacement for download_market_unusual (Line ~1921):
  output$download_market_unusual <- downloadHandler(
    filename = function() { paste0("market_unusual_", results$country_code, ".png") },
    content = function(file) {
      req(results$filtered_analysis$markets$market_unusual_plot)
      filter_caption <- get_filter_caption(filters$risky)
      plot_to_save <- if (filter_caption != "") {
        results$filtered_analysis$markets$market_unusual_plot + labs(caption = filter_caption)
      } else {
        results$filtered_analysis$markets$market_unusual_plot
      }
      ggsave(file, plot_to_save, width = 10, height = 8, dpi = 300)
    }
  )
  
  # Full replacement for download_concentration (Line ~1928):
  output$download_concentration <- downloadHandler(
    filename = function() { paste0("concentration_", results$country_code, ".png") },
    content = function(file) {
      req(results$filtered_analysis$competition$concentration_yearly_plot)
      filter_caption <- get_filter_caption(filters$risky)
      plot_to_save <- if (filter_caption != "") {
        results$filtered_analysis$competition$concentration_yearly_plot + labs(caption = filter_caption)
      } else {
        results$filtered_analysis$competition$concentration_yearly_plot
      }
      ggsave(file, plot_to_save, width = 12, height = 10, dpi = 300)
    }
  )
  
  # Full replacement for download_singleb (Line ~1935):
  output$download_singleb <- downloadHandler(
    filename = function() { paste0("singleb_", results$country_code, ".png") },
    content = function(file) {
      req(results$filtered_analysis$prices$singleb_plot)
      filter_caption <- get_filter_caption(filters$prices)
      plot_to_save <- if (filter_caption != "") {
        results$filtered_analysis$prices$singleb_plot + labs(caption = filter_caption)
      } else {
        results$filtered_analysis$prices$singleb_plot
      }
      ggsave(file, plot_to_save, width = 10, height = 8, dpi = 300)
    }
  )
  
  # Full replacement for download_relprice (Line ~1942):
  output$download_relprice <- downloadHandler(
    filename = function() { paste0("relprice_", results$country_code, ".png") },
    content = function(file) {
      req(results$filtered_analysis$prices$rel_price_plot)
      filter_caption <- get_filter_caption(filters$prices)
      plot_to_save <- if (filter_caption != "") {
        results$filtered_analysis$prices$rel_price_plot + labs(caption = filter_caption)
      } else {
        results$filtered_analysis$prices$rel_price_plot
      }
      ggsave(file, plot_to_save, width = 10, height = 8, dpi = 300)
    }
  )
  
  # Full replacement for download_contracts_year (Line ~1857):
  output$download_contracts_year <- downloadHandler(
    filename = function() { paste0("contracts_per_year_", results$country_code, ".png") },
    content = function(file) {
      req(results$filtered_analysis)
      stats <- results$filtered_analysis$summary_stats$n_obs_per_year
      filter_caption <- get_filter_caption(filters$overview)
      
      p <- ggplot(stats, aes(x = tender_year, y = n_observations)) +
        geom_col(fill = "#3c8dbc") +
        geom_text(aes(label = formatC(n_observations, format = "d", big.mark = ",")),
                  vjust = -0.5, size = 4) +
        labs(x = "Year", y = "Number of Contracts", title = "Contracts per Year") +
        standard_plot_theme() +
        scale_y_continuous(labels = scales::comma)
      
      if (filter_caption != "") {
        p <- p + labs(caption = filter_caption)
      }
      
      ggsave(file, p, width = 10, height = 8, dpi = 300)
    }
  )
  
  # ========================================================================
  # DOWNLOAD HANDLERS - REPORTS
  # ========================================================================
  
  output$download_pdf_report <- downloadHandler(
    filename = function() {
      paste0("procurement_integrity_report_", results$country_code, "_", 
             format(Sys.Date(), "%Y%m%d"), ".pdf")
    },
    content = function(file) {
      req(results$filtered_analysis, results$filtered_data, results$country_code)
      
      # Get filter description
      filters_text <- ""
      # ... (filter text assembly code) ...
      
      withProgress(message = 'Generating PDF report...', value = 0, {
        incProgress(0.3, detail = "Compiling document...")
        
        # CRITICAL: Pass both data and analysis separately
        success <- generate_pdf_report(
          filtered_data = results$filtered_data,
          filtered_analysis = results$filtered_analysis,
          country_code = results$country_code,
          output_file = file,
          filters_text = filters_text
        )
        
        incProgress(0.7, detail = "Finalizing...")
        
        if (!success) {
          showNotification("Error generating PDF report. Check console for details.", 
                           type = "error", duration = 10)
        } else {
          showNotification("PDF report generated successfully!", 
                           type = "message", duration = 5)
        }
      })
    }
  )
  
  output$download_word_report <- downloadHandler(
    filename = function() {
      paste0("procurement_integrity_report_", results$country_code, "_", 
             format(Sys.Date(), "%Y%m%d"), ".docx")
    },
    content = function(file) {
      req(results$filtered_analysis, results$filtered_data, results$country_code)
      
      # Get filter description
      filters_text <- ""
      # ... (filter text assembly code) ...
      
      withProgress(message = 'Generating Word document...', value = 0, {
        incProgress(0.3, detail = "Creating document...")
        
        # CRITICAL: Pass both data and analysis separately
        success <- generate_word_report(
          filtered_data = results$filtered_data,
          filtered_analysis = results$filtered_analysis,
          country_code = results$country_code,
          output_file = file,
          filters_text = filters_text
        )
        
        incProgress(0.7, detail = "Finalizing...")
        
        if (!success) {
          showNotification("Error generating Word document. Check console for details.", 
                           type = "error", duration = 10)
        } else {
          showNotification("Word document generated successfully!", 
                           type = "message", duration = 5)
        }
      })
    }
  )
  
  output$download_all_figures <- downloadHandler(
    filename = function() {
      paste0("all_figures_", results$country_code, "_", format(Sys.Date(), "%Y%m%d"), ".zip")
    },
    content = function(file) {
      req(results$filtered_analysis)
      
      withProgress(message = 'Creating ZIP file...', value = 0, {
        temp_dir <- tempfile()
        dir.create(temp_dir)
        
        plots <- list(
          list(plot = results$filtered_analysis$missing$overall_plot, name = "missing_overall"),
          list(plot = results$filtered_analysis$missing$by_buyer_plot, name = "missing_buyer"),
          list(plot = results$filtered_analysis$missing$by_year_plot, name = "missing_time"),
          list(plot = results$filtered_analysis$markets$network_plot, name = "network"),
          list(plot = results$filtered_analysis$markets$supplier_unusual_plot, name = "supplier_unusual"),
          list(plot = results$filtered_analysis$markets$market_unusual_plot, name = "market_unusual"),
          list(plot = results$filtered_analysis$competition$concentration_yearly_plot, name = "concentration"),
          list(plot = results$filtered_analysis$prices$singleb_plot, name = "singleb"),
          list(plot = results$filtered_analysis$prices$rel_price_plot, name = "relprice")
        )
        
        n_plots <- length(plots)
        for (i in seq_along(plots)) {
          incProgress(i/n_plots, detail = paste("Saving figure", i, "of", n_plots))
          
          if (!is.null(plots[[i]]$plot)) {
            file_path <- file.path(temp_dir, paste0(plots[[i]]$name, "_", results$country_code, ".png"))
            tryCatch({
              ggsave(file_path, plots[[i]]$plot, width = 10, height = 8, dpi = 300)
            }, error = function(e) {
              message("Could not save ", plots[[i]]$name)
            })
          }
        }
        
        zip::zip(zipfile = file, files = list.files(temp_dir, full.names = TRUE), 
                 mode = "cherry-pick")
        
        unlink(temp_dir, recursive = TRUE)
      })
    }
  )
}

# ========================================================================
# RUN APPLICATION
# ========================================================================

shinyApp(ui = ui, server = server)