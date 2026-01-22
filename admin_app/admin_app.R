# ========================================================================
# ADMINISTRATIVE EFFICIENCY ANALYSIS - SHINY APPLICATION  
# Exactly matching Integrity App structure and functionality
# ========================================================================

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
source("C:/Users/wb589991/OneDrive - WBG/Documents/procuR.github.io/src/utils/admin_utils.R")

# ========================================================================
# INCREASE FILE UPLOAD LIMIT
# ========================================================================
options(shiny.maxRequestSize = 1000*1024^2)

# ========================================================================
# HELPER FUNCTIONS
# ========================================================================

`%ni%` <- Negate(`%in%`)

# Filter data based on user selections
filter_data <- function(df, year_range = NULL, market = NULL, value_range = NULL, 
                        buyer_type = NULL, procedure_type = NULL, value_divisor = 1) {
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
  
  # Filter by contract value
  if (!is.null(value_range) && !is.null(value_divisor)) {
    price_col <- NULL
    if ("bid_priceusd" %in% names(df)) {
      price_col <- "bid_priceusd"
    } else if ("bid_price" %in% names(df)) {
      price_col <- "bid_price"
    }
    
    if (!is.null(price_col)) {
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
  
  # Filter by procedure type
  if (!is.null(procedure_type) && length(procedure_type) > 0 && "All" %ni% procedure_type && "tender_proceduretype" %in% names(df)) {
    filtered_df <- filtered_df %>%
      filter(tender_proceduretype %in% procedure_type)
  }
  
  return(filtered_df)
}

# Helper function to describe active filters
get_filter_description <- function(filter_list) {
  parts <- c()
  
  if (!is.null(filter_list$year) && length(filter_list$year) == 2) {
    parts <- c(parts, paste0("Years: ", filter_list$year[1], "-", filter_list$year[2]))
  }
  if (!is.null(filter_list$market) && length(filter_list$market) > 0 && "All" %ni% filter_list$market) {
    parts <- c(parts, paste0("Markets: ", paste(filter_list$market, collapse = ", ")))
  }
  if (!is.null(filter_list$value) && length(filter_list$value) == 2) {
    parts <- c(parts, paste0("Value range applied"))
  }
  if (!is.null(filter_list$buyer_type) && length(filter_list$buyer_type) > 0 && "All" %ni% filter_list$buyer_type) {
    parts <- c(parts, paste0("Buyer types: ", paste(filter_list$buyer_type, collapse = ", ")))
  }
  if (!is.null(filter_list$procedure_type) && length(filter_list$procedure_type) > 0 && "All" %ni% filter_list$procedure_type) {
    parts <- c(parts, paste0("Procedures: ", paste(filter_list$procedure_type, collapse = ", ")))
  }
  
  if (length(parts) == 0) {
    return("No filters applied")
  } else {
    return(paste(parts, collapse = "; "))
  }
}

# Helper to get filter caption for plots
get_filter_caption <- function(filter_list) {
  desc <- get_filter_description(filter_list)
  if (desc == "No filters applied") {
    return("")
  } else {
    return(paste("Filters applied:", desc))
  }
}

# ========================================================================
# REPORT GENERATION FUNCTIONS
# ========================================================================

generate_pdf_report <- function(filtered_data, filtered_analysis, country_code, 
                                output_file, filters_text = "") {
  
  # Save data for RMD to access
  saveRDS(list(
    data = filtered_data,
    analysis = filtered_analysis,
    country_code = country_code
  ), file = file.path(tempdir(), "admin_report_data.rds"))
  
  # Create a simple RMD that calls the actual report template
  temp_rmd <- tempfile(fileext = ".Rmd")
  
  rmd_content <- '---
title: "Administrative Efficiency Analysis"
output: pdf_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE, 
                      fig.width = 10, fig.height = 6)
library(ggplot2)
library(dplyr)

# Load the saved data
report_data <- readRDS(file.path(tempdir(), "admin_report_data.rds"))
df <- report_data$data
results <- list()
results[[report_data$country_code]] <- report_data$analysis
```

# Administrative Efficiency Report

**Country:** `r report_data$country_code`

## Executive Summary

This section provides an overview of the administrative barriers to the implementation of public procurement. Such barriers may arise from limited resources available to public buyers, insufficient staff capacity (e.g., staff shortages or low qualifications), an overload of procurement procedures, or inadequate regulation. The analysis focuses on identifying the most common symptoms of these administrative barriers, including the distribution of procurement procedure types, the occurrence of delays, the proportion of cancelled tenders, and other related indicators.

## Data Overview

- **Total Contracts:** `r format(nrow(df), big.mark = ",")`
- **Years Covered:** `r if("tender_year" %in% names(df)) paste(min(df$tender_year, na.rm=TRUE), "-", max(df$tender_year, na.rm=TRUE)) else "N/A"`
- **Unique Buyers:** `r if("buyer_masterid" %in% names(df)) format(length(unique(df$buyer_masterid)), big.mark = ",") else "N/A"`
- **Unique Suppliers:** `r if("bidder_masterid" %in% names(df)) format(length(unique(df$bidder_masterid)), big.mark = ",") else "N/A"`

\\newpage

# Procedure Type Analysis

## Is there an overuse of some procedure types?

This panel shows how tenders are distributed across different procedure types. A high reliance on non-competitive procedures (such as direct awards or negotiated without publication) may indicate limited competition or potential misuse of exceptional procurement rules.

### Procedure Type Share by Contract Value

This plot shows the share of total contract value awarded through each procedure type. High-value contracts awarded through non-competitive procedures may indicate opportunities for improving competition in major procurements.

```{r fig.height=6}
if (!is.null(report_data$analysis$sh)) {
  print(report_data$analysis$sh)
}
```

### Procedure Type Share by Contract Count

This plot shows the share of contract numbers for each procedure type. A high frequency of non-competitive procedures may suggest systemic issues with procurement planning or excessive use of emergency procedures.

```{r fig.height=6}
if (!is.null(report_data$analysis$p_count)) {
  print(report_data$analysis$p_count)
}
```

\\newpage

# Submission Period Analysis

## Are submission periods too short?

Very short submission periods may prevent suppliers from preparing competitive bids, reducing competition and potentially favoring incumbents who are already familiar with the buyer\'s requirements.

### Overall Submission Period Distribution

This histogram shows the distribution of submission periods (days from call for tender to bid deadline). Vertical lines mark the 25th percentile (Q1), median, and 75th percentile (Q3).

```{r fig.height=7}
if (!is.null(report_data$analysis$subm)) {
  print(report_data$analysis$subm)
}
```

### Submission Periods by Procedure Type

This faceted histogram breaks down submission periods by procedure type. Each panel shows quartiles for that specific procedure. More complex procedures (restricted, negotiated with publication) typically require longer preparation times, while open procedures should still allow sufficient time for suppliers to respond. Compare these distributions to regulatory minimums to identify systematic underuse of adequate submission windows.

```{r fig.height=7}
if (!is.null(report_data$analysis$subm_proc_facet_q)) {
  print(report_data$analysis$subm_proc_facet_q)
}
```

### Short vs Normal Submission Periods

This histogram highlights submission periods flagged as unusually short (in red) compared to normal periods (in blue). The flagging is based on country-specific thresholds or, if unavailable, the median for each procedure type. A high proportion of short submission periods may indicate that buyers are not allowing adequate time for competitive bidding, which can reduce the number of bidders and lead to less competitive outcomes.

```{r fig.height=6}
if (!is.null(report_data$analysis$subm_r)) {
  print(report_data$analysis$subm_r)
}
```

\\newpage

## Which buyers set the shortest submission periods?

These stacked bar charts show which buyer groups most frequently use short submission periods. The left panel displays counts, while the right panel shows contract values. If certain buyer types (e.g., national vs. regional) systematically use short deadlines, this may point to specific capacity constraints or procurement practices that merit closer examination. High-value contracts with short deadlines are particularly concerning as they affect larger market segments.

```{r fig.height=7}
if (!is.null(report_data$analysis$combined_short_buyer)) {
  print(report_data$analysis$combined_short_buyer)
}
```

\\newpage

# Decision Period Analysis

## Are decision periods too long?

Long decision periods can delay project implementation and create uncertainty for suppliers. While some procedures naturally take longer due to complexity or legal requirements, consistently prolonged decisions may indicate administrative bottlenecks or lack of resources.

### Overall Decision Period Distribution

This histogram shows the distribution of decision periods (days from bid deadline to award decision).

```{r fig.height=7}
if (!is.null(report_data$analysis$decp)) {
  print(report_data$analysis$decp)
}
```

### Decision Periods by Procedure Type

This faceted histogram shows decision periods broken down by procedure type, with quartile markers. This allows you to see whether more complex procedures take appropriately longer to evaluate, and whether simpler procedures are decided more quickly. Pay attention to procedure types where decisions are routinely much slower than others, or where complex tenders appear to be decided as quickly as simple ones.

```{r fig.height=7}
if (!is.null(report_data$analysis$decp_proc_facet_q)) {
  print(report_data$analysis$decp_proc_facet_q)
}
```

### Long vs Normal Decision Periods

This histogram highlights decision periods flagged as unusually long (in red) compared to normal periods (in green). A high proportion of long decisions suggests systematic delays, which can undermine predictability for suppliers and stall public projects.

```{r fig.height=6}
if (!is.null(report_data$analysis$decp_r)) {
  print(report_data$analysis$decp_r)
}
```

\\newpage

## Which buyers have the longest decision periods?

These stacked bar charts show which buyer groups most frequently experience long decision periods. The left panel displays contract counts, while the right panel shows contract values. High shares of delayed decisions in a specific buyer group may indicate bottlenecks in evaluation, internal approval, or oversight procedures that need to be addressed.

```{r fig.height=7}
if (!is.null(report_data$analysis$combined_dec_plot)) {
  print(report_data$analysis$combined_dec_plot)
}
```

\\newpage

# Regression Analysis

## Is administrative efficiency linked to competition?

While this analysis does not establish any causal relationships, it can be used to explore correlations between administrative practices and competitiveness.

### Effect of Short Submission Periods on Single Bidding

This figure summarizes how the likelihood of single bidding changes when submission periods are unusually short. The model-based prediction accounts for differences in buyer type, procedure type, and year. If the line for short deadlines sits noticeably higher, this suggests that tight timelines may be restricting competition and making single bidding more likely. The shaded area represents the 95% confidence interval around the prediction.

```{r fig.height=7}
if (!is.null(report_data$analysis$plot_short_reg)) {
  print(report_data$analysis$plot_short_reg)
}
```

### Effect of Long Decision Periods on Single Bidding

This figure assesses whether very long decision periods are associated with single bidding. The horizontal axis compares tenders decided within a normal time frame to those with delayed decisions. The vertical axis shows the predicted probability of single bidding, after accounting for buyer type and year. A higher predicted probability for delayed decisions may indicate strategic behavior or weakened competitive pressure in tenders that drag on.

```{r fig.height=7}
if (!is.null(report_data$analysis$plot_long_reg)) {
  print(report_data$analysis$plot_long_reg)
}
```

\\newpage

# Conclusions

This analysis provides insights into administrative efficiency patterns in public procurement. Key findings include:

- The distribution and trends in procedure type usage
- Patterns in submission and decision period lengths  
- Buyer-level variations in procedural timelines
- Statistical relationships between administrative practices and competition

These findings can inform targeted interventions to improve administrative efficiency while maintaining competitive procurement processes.
'

writeLines(rmd_content, temp_rmd)

success <- tryCatch({
  rmarkdown::render(temp_rmd, output_file = output_file, quiet = FALSE)
  TRUE
}, error = function(e) {
  message("Error generating PDF report: ", e$message)
  print(e)
  FALSE
})

return(success)
}

generate_word_report <- function(filtered_data, filtered_analysis, country_code, 
                                 output_file, filters_text = "") {
  
  # Save data for RMD to access
  saveRDS(list(
    data = filtered_data,
    analysis = filtered_analysis,
    country_code = country_code
  ), file = file.path(tempdir(), "admin_report_data.rds"))
  
  # Create a simple RMD
  temp_rmd <- tempfile(fileext = ".Rmd")
  
  rmd_content <- '---
title: "Administrative Efficiency Analysis"
output: word_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE, 
                      fig.width = 10, fig.height = 6)
library(ggplot2)
library(dplyr)

# Load the saved data
report_data <- readRDS(file.path(tempdir(), "admin_report_data.rds"))
df <- report_data$data
results <- list()
results[[report_data$country_code]] <- report_data$analysis
```

# Administrative Efficiency Report

**Country:** `r report_data$country_code`

## Executive Summary

This section provides an overview of the administrative barriers to the implementation of public procurement. Such barriers may arise from limited resources available to public buyers, insufficient staff capacity (e.g., staff shortages or low qualifications), an overload of procurement procedures, or inadequate regulation. The analysis focuses on identifying the most common symptoms of these administrative barriers, including the distribution of procurement procedure types, the occurrence of delays, the proportion of cancelled tenders, and other related indicators.

## Data Overview

- **Total Contracts:** `r format(nrow(df), big.mark = ",")`
- **Years Covered:** `r if("tender_year" %in% names(df)) paste(min(df$tender_year, na.rm=TRUE), "-", max(df$tender_year, na.rm=TRUE)) else "N/A"`
- **Unique Buyers:** `r if("buyer_masterid" %in% names(df)) format(length(unique(df$buyer_masterid)), big.mark = ",") else "N/A"`
- **Unique Suppliers:** `r if("bidder_masterid" %in% names(df)) format(length(unique(df$bidder_masterid)), big.mark = ",") else "N/A"`

# Procedure Type Analysis

## Is there an overuse of some procedure types?

This panel shows how tenders are distributed across different procedure types. A high reliance on non-competitive procedures (such as direct awards or negotiated without publication) may indicate limited competition or potential misuse of exceptional procurement rules.

### Procedure Type Share by Contract Value

This plot shows the share of total contract value awarded through each procedure type. High-value contracts awarded through non-competitive procedures may indicate opportunities for improving competition in major procurements.

```{r fig.height=6}
if (!is.null(report_data$analysis$sh)) {
  print(report_data$analysis$sh)
}
```

### Procedure Type Share by Contract Count

This plot shows the share of contract numbers for each procedure type. A high frequency of non-competitive procedures may suggest systemic issues with procurement planning or excessive use of emergency procedures.

```{r fig.height=6}
if (!is.null(report_data$analysis$p_count)) {
  print(report_data$analysis$p_count)
}
```

# Submission Period Analysis

## Are submission periods too short?

Very short submission periods may prevent suppliers from preparing competitive bids, reducing competition and potentially favoring incumbents who are already familiar with the buyer\'s requirements.

### Overall Submission Period Distribution

This histogram shows the distribution of submission periods (days from call for tender to bid deadline). Vertical lines mark the 25th percentile (Q1), median, and 75th percentile (Q3).

```{r fig.height=7}
if (!is.null(report_data$analysis$subm)) {
  print(report_data$analysis$subm)
}
```

### Submission Periods by Procedure Type

This faceted histogram breaks down submission periods by procedure type. Each panel shows quartiles for that specific procedure. More complex procedures (restricted, negotiated with publication) typically require longer preparation times, while open procedures should still allow sufficient time for suppliers to respond. Compare these distributions to regulatory minimums to identify systematic underuse of adequate submission windows.

```{r fig.height=7}
if (!is.null(report_data$analysis$subm_proc_facet_q)) {
  print(report_data$analysis$subm_proc_facet_q)
}
```

### Short vs Normal Submission Periods

This histogram highlights submission periods flagged as unusually short (in red) compared to normal periods (in blue). The flagging is based on country-specific thresholds or, if unavailable, the median for each procedure type. A high proportion of short submission periods may indicate that buyers are not allowing adequate time for competitive bidding, which can reduce the number of bidders and lead to less competitive outcomes.

```{r fig.height=6}
if (!is.null(report_data$analysis$subm_r)) {
  print(report_data$analysis$subm_r)
}
```

## Which buyers set the shortest submission periods?

These stacked bar charts show which buyer groups most frequently use short submission periods. The left panel displays counts, while the right panel shows contract values. If certain buyer types (e.g., national vs. regional) systematically use short deadlines, this may point to specific capacity constraints or procurement practices that merit closer examination. High-value contracts with short deadlines are particularly concerning as they affect larger market segments.

```{r fig.height=7}
if (!is.null(report_data$analysis$combined_short_buyer)) {
  print(report_data$analysis$combined_short_buyer)
}
```

# Decision Period Analysis

## Are decision periods too long?

Long decision periods can delay project implementation and create uncertainty for suppliers. While some procedures naturally take longer due to complexity or legal requirements, consistently prolonged decisions may indicate administrative bottlenecks or lack of resources.

### Overall Decision Period Distribution

This histogram shows the distribution of decision periods (days from bid deadline to award decision).

```{r fig.height=7}
if (!is.null(report_data$analysis$decp)) {
  print(report_data$analysis$decp)
}
```

### Decision Periods by Procedure Type

This faceted histogram shows decision periods broken down by procedure type, with quartile markers. This allows you to see whether more complex procedures take appropriately longer to evaluate, and whether simpler procedures are decided more quickly. Pay attention to procedure types where decisions are routinely much slower than others, or where complex tenders appear to be decided as quickly as simple ones.

```{r fig.height=7}
if (!is.null(report_data$analysis$decp_proc_facet_q)) {
  print(report_data$analysis$decp_proc_facet_q)
}
```

### Long vs Normal Decision Periods

This histogram highlights decision periods flagged as unusually long (in red) compared to normal periods (in green). A high proportion of long decisions suggests systematic delays, which can undermine predictability for suppliers and stall public projects.

```{r fig.height=6}
if (!is.null(report_data$analysis$decp_r)) {
  print(report_data$analysis$decp_r)
}
```

## Which buyers have the longest decision periods?

These stacked bar charts show which buyer groups most frequently experience long decision periods. The left panel displays contract counts, while the right panel shows contract values. High shares of delayed decisions in a specific buyer group may indicate bottlenecks in evaluation, internal approval, or oversight procedures that need to be addressed.

```{r fig.height=7}
if (!is.null(report_data$analysis$combined_dec_plot)) {
  print(report_data$analysis$combined_dec_plot)
}
```

# Regression Analysis

## Is administrative efficiency linked to competition?

While this analysis does not establish any causal relationships, it can be used to explore correlations between administrative practices and competitiveness.

### Effect of Short Submission Periods on Single Bidding

This figure summarizes how the likelihood of single bidding changes when submission periods are unusually short. The model-based prediction accounts for differences in buyer type, procedure type, and year. If the line for short deadlines sits noticeably higher, this suggests that tight timelines may be restricting competition and making single bidding more likely. The shaded area represents the 95% confidence interval around the prediction.

```{r fig.height=7}
if (!is.null(report_data$analysis$plot_short_reg)) {
  print(report_data$analysis$plot_short_reg)
}
```

### Effect of Long Decision Periods on Single Bidding

This figure assesses whether very long decision periods are associated with single bidding. The horizontal axis compares tenders decided within a normal time frame to those with delayed decisions. The vertical axis shows the predicted probability of single bidding, after accounting for buyer type and year. A higher predicted probability for delayed decisions may indicate strategic behavior or weakened competitive pressure in tenders that drag on.

```{r fig.height=7}
if (!is.null(report_data$analysis$plot_long_reg)) {
  print(report_data$analysis$plot_long_reg)
}
```

# Conclusions

This analysis provides insights into administrative efficiency patterns in public procurement. Key findings include:

- The distribution and trends in procedure type usage
- Patterns in submission and decision period lengths  
- Buyer-level variations in procedural timelines
- Statistical relationships between administrative practices and competition

These findings can inform targeted interventions to improve administrative efficiency while maintaining competitive procurement processes.
'

writeLines(rmd_content, temp_rmd)

success <- tryCatch({
  rmarkdown::render(temp_rmd, output_file = output_file, quiet = FALSE)
  TRUE
}, error = function(e) {
  message("Error generating Word report: ", e$message)
  print(e)
  FALSE
})

return(success)
}

# ========================================================================
# UI
# ========================================================================

ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(title = "Administrative Efficiency Analysis", titleWidth = 350),
  
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      id = "sidebar",
      menuItem("Setup", tabName = "setup", icon = icon("cog")),
      menuItem("Overview", tabName = "overview", icon = icon("home")),
      menuItem("Data Overview", tabName = "data_overview", icon = icon("table")),
      menuItem("Procedure Types", tabName = "procedures", icon = icon("list-check")),
      menuItem("Submission Periods", tabName = "submission", icon = icon("clock")),
      menuItem("Decision Periods", tabName = "decision", icon = icon("gavel")),
      menuItem("Regression Analysis", tabName = "regression", icon = icon("chart-line")),
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
      # ====================================================================
      # SETUP TAB
      # ====================================================================
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
                     h4("Filtering Options:"),
                     tags$ul(
                       tags$li("Filter by year range"),
                       tags$li("Filter by market (CPV codes)"),
                       tags$li("Filter by contract value"),
                       tags$li("Filter by buyer type"),
                       tags$li("Filter by procedure type")
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
      
      # ====================================================================
      # OVERVIEW TAB
      # ====================================================================
      tabItem(
        tabName = "overview",
        h2("Administrative Efficiency Analysis"),
        
        fluidRow(
          box(
            width = 12,
            status = "primary",
            p(style = "font-size: 16px;",
              "This section provides an overview of the administrative barriers to the implementation of public procurement. 
              Such barriers may arise from limited resources available to public buyers, insufficient staff capacity 
              (e.g., staff shortages or low qualifications), an overload of procurement procedures, or inadequate regulation. 
              The analysis focuses on identifying the most common symptoms of these administrative barriers, including the 
              distribution of procurement procedure types, the occurrence of delays, the proportion of cancelled tenders, 
              and other related indicators."
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
              tags$li(tags$b("Procedure Mix:"), " Is there an overuse of some procedure types?"),
              tags$li(tags$b("Submission Periods:"), " Are submission periods too short?"),
              tags$li(tags$b("Buyer Analysis:"), " Which buyers set the shortest submission periods?"),
              tags$li(tags$b("Decision Periods:"), " Are decision periods too long?"),
              tags$li(tags$b("Buyer Delays:"), " Which buyers have the longest decision periods?"),
              tags$li(tags$b("Competition Impact:"), " Is administrative efficiency linked to competition?")
            )
          )
        )
      ),
      
      # ====================================================================
      # DATA OVERVIEW TAB
      # ====================================================================
      tabItem(
        tabName = "data_overview",
        h2("Data Overview"),
        
        # FILTERS BOX
        fluidRow(
          box(
            title = "Filters",
            width = 12,
            collapsible = TRUE,
            collapsed = FALSE,
            status = "info",
            
            fluidRow(
              column(2, uiOutput("year_filter_overview")),
              column(2, uiOutput("market_filter_overview")),
              column(2, uiOutput("value_filter_overview")),
              column(3, uiOutput("buyer_type_filter_overview")),
              column(3, uiOutput("procedure_type_filter_overview"))
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
        )
      ),
      
      # ====================================================================
      # PROCEDURE TYPES TAB
      # ====================================================================
      tabItem(
        tabName = "procedures",
        h2("Procedure Type Analysis"),
        
        # FILTERS BOX
        fluidRow(
          box(
            title = "Filters",
            width = 12,
            collapsible = TRUE,
            collapsed = FALSE,
            status = "info",
            
            fluidRow(
              column(2, uiOutput("year_filter_proc")),
              column(2, uiOutput("market_filter_proc")),
              column(2, uiOutput("value_filter_proc")),
              column(3, uiOutput("buyer_type_filter_proc")),
              column(3, uiOutput("procedure_type_filter_proc"))
            ),
            
            fluidRow(
              column(12,
                     actionButton("apply_filters_proc", "Apply Filters", icon = icon("filter"), class = "btn-primary"),
                     actionButton("reset_filters_proc", "Reset Filters", icon = icon("undo"), class = "btn-warning"),
                     textOutput("filter_status_proc", inline = TRUE)
              )
            )
          )
        ),
        
        div(class = "question-header",
            "Is there an overuse of some procedure types?"
        ),
        
        fluidRow(
          box(
            title = "Procedure Type Share by Contract Value",
            width = 6,
            solidHeader = TRUE,
            status = "primary",
            div(class = "description-box",
                p("This plot shows the share of total contract value awarded through each procedure type. 
                  High-value contracts awarded through non-competitive procedures may indicate opportunities 
                  for improving competition in major procurements.")
            ),
            plotOutput("procedure_share_value_plot", height = "600px"),
            downloadButton("download_proc_share_value", "Download Figure", class = "download-btn btn-sm")
          ),
          box(
            title = "Procedure Type Share by Contract Count",
            width = 6,
            solidHeader = TRUE,
            status = "primary",
            div(class = "description-box",
                p("This plot shows the share of contract numbers for each procedure type. 
                  A high frequency of non-competitive procedures may suggest systemic issues with 
                  procurement planning or excessive use of emergency procedures.")
            ),
            plotOutput("procedure_share_count_plot", height = "600px"),
            downloadButton("download_proc_share_count", "Download Figure", class = "download-btn btn-sm")
          )
        ),
        
        fluidRow(
          box(
            title = "Procedure Type Distribution (Combined)",
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            div(class = "description-box",
                p("This combined panel shows both value share (left) and count share (right) side by side.")
            ),
            plotOutput("procedure_combined_plot", height = "600px"),
            downloadButton("download_proc_combined", "Download Figure", class = "download-btn btn-sm")
          )
        ),
        
        fluidRow(
          box(
            title = "Procedure Type Share Over Time",
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            div(class = "description-box",
                p("This plot tracks how the share of different procedure types has evolved over time. 
                  Tracking changes can reveal the impact of reforms or shifts in procurement practices.")
            ),
            plotOutput("procedure_share_time_plot", height = "600px"),
            downloadButton("download_proc_share_time", "Download Figure", class = "download-btn btn-sm")
          )
        )
      ),
      
      # ====================================================================
      # SUBMISSION PERIODS TAB
      # ====================================================================
      tabItem(
        tabName = "submission",
        h2("Submission Period Analysis"),
        
        # FILTERS BOX
        fluidRow(
          box(
            title = "Filters",
            width = 12,
            collapsible = TRUE,
            collapsed = FALSE,
            status = "info",
            
            fluidRow(
              column(2, uiOutput("year_filter_subm")),
              column(2, uiOutput("market_filter_subm")),
              column(2, uiOutput("value_filter_subm")),
              column(3, uiOutput("buyer_type_filter_subm")),
              column(3, uiOutput("procedure_type_filter_subm"))
            ),
            
            fluidRow(
              column(12,
                     actionButton("apply_filters_subm", "Apply Filters", icon = icon("filter"), class = "btn-primary"),
                     actionButton("reset_filters_subm", "Reset Filters", icon = icon("undo"), class = "btn-warning"),
                     textOutput("filter_status_subm", inline = TRUE)
              )
            )
          )
        ),
        
        div(class = "question-header",
            "Are submission periods too short?"
        ),
        
        fluidRow(
          box(
            title = "Overall Submission Period Distribution",
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            div(class = "description-box",
                p("This histogram shows the distribution of submission periods (days from call for tender to bid deadline). 
                  Vertical lines mark the 25th percentile (Q1), median, and 75th percentile (Q3). 
                  Very short submission periods may prevent suppliers from preparing competitive bids, 
                  reducing competition and potentially favoring incumbents who are already familiar with the buyer's requirements.")
            ),
            plotOutput("submission_dist_plot", height = "600px"),
            downloadButton("download_subm_dist", "Download Figure", class = "download-btn btn-sm")
          )
        ),
        
        fluidRow(
          box(
            title = "Submission Periods by Procedure Type",
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            div(class = "description-box",
                p("This faceted histogram breaks down submission periods by procedure type. 
                  Each panel shows quartiles for that specific procedure. 
                  More complex procedures (restricted, negotiated with publication) typically require longer preparation times, 
                  while open procedures should still allow sufficient time for suppliers to respond. 
                  Compare these distributions to regulatory minimums to identify systematic underuse of adequate submission windows.")
            ),
            plotOutput("submission_proc_plot", height = "600px"),
            downloadButton("download_subm_proc", "Download Figure", class = "download-btn btn-sm")
          )
        ),
        
        fluidRow(
          box(
            title = "Short vs Normal Submission Periods",
            width = 12,
            solidHeader = TRUE,
            status = "warning",
            div(class = "description-box",
                p("This histogram highlights submission periods flagged as unusually short (in red) compared to normal periods (in blue). 
                  The flagging is based on country-specific thresholds or, if unavailable, the median for each procedure type. 
                  A high proportion of short submission periods may indicate that buyers are not allowing adequate time for competitive bidding, 
                  which can reduce the number of bidders and lead to less competitive outcomes.")
            ),
            plotOutput("submission_short_plot", height = "500px"),
            downloadButton("download_subm_short", "Download Figure", class = "download-btn btn-sm")
          )
        ),
        
        div(class = "question-header",
            "Which buyers set the shortest submission periods?"
        ),
        
        fluidRow(
          box(
            title = "Short Submission Periods by Buyer Group",
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            div(class = "description-box",
                p("These stacked bar charts show which buyer groups most frequently use short submission periods. 
                  The left panel displays counts, while the right panel shows contract values. 
                  If certain buyer types (e.g., national vs. regional) systematically use short deadlines, 
                  this may point to specific capacity constraints or procurement practices that merit closer examination. 
                  High-value contracts with short deadlines are particularly concerning as they affect larger market segments.")
            ),
            plotOutput("buyer_short_plot", height = "600px"),
            downloadButton("download_buyer_short", "Download Figure", class = "download-btn btn-sm")
          )
        )
      ),
      
      # ====================================================================
      # DECISION PERIODS TAB
      # ====================================================================
      tabItem(
        tabName = "decision",
        h2("Decision Period Analysis"),
        
        # FILTERS BOX
        fluidRow(
          box(
            title = "Filters",
            width = 12,
            collapsible = TRUE,
            collapsed = FALSE,
            status = "info",
            
            fluidRow(
              column(2, uiOutput("year_filter_dec")),
              column(2, uiOutput("market_filter_dec")),
              column(2, uiOutput("value_filter_dec")),
              column(3, uiOutput("buyer_type_filter_dec")),
              column(3, uiOutput("procedure_type_filter_dec"))
            ),
            
            fluidRow(
              column(12,
                     actionButton("apply_filters_dec", "Apply Filters", icon = icon("filter"), class = "btn-primary"),
                     actionButton("reset_filters_dec", "Reset Filters", icon = icon("undo"), class = "btn-warning"),
                     textOutput("filter_status_dec", inline = TRUE)
              )
            )
          )
        ),
        
        div(class = "question-header",
            "Are decision periods too long?"
        ),
        
        fluidRow(
          box(
            title = "Overall Decision Period Distribution",
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            div(class = "description-box",
                p("This histogram shows the distribution of decision periods (days from bid deadline to award decision). 
                  Long decision periods can delay project implementation and create uncertainty for suppliers. 
                  While some procedures naturally take longer due to complexity or legal requirements, 
                  consistently prolonged decisions may indicate administrative bottlenecks or lack of resources.")
            ),
            plotOutput("decision_dist_plot", height = "600px"),
            downloadButton("download_dec_dist", "Download Figure", class = "download-btn btn-sm")
          )
        ),
        
        fluidRow(
          box(
            title = "Decision Periods by Procedure Type",
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            div(class = "description-box",
                p("This faceted histogram shows decision periods broken down by procedure type, with quartile markers. 
                  This allows you to see whether more complex procedures take appropriately longer to evaluate, 
                  and whether simpler procedures are decided more quickly. 
                  Pay attention to procedure types where decisions are routinely much slower than others, 
                  or where complex tenders appear to be decided as quickly as simple ones.")
            ),
            plotOutput("decision_proc_plot", height = "600px"),
            downloadButton("download_dec_proc", "Download Figure", class = "download-btn btn-sm")
          )
        ),
        
        fluidRow(
          box(
            title = "Long vs Normal Decision Periods",
            width = 12,
            solidHeader = TRUE,
            status = "warning",
            div(class = "description-box",
                p("This histogram highlights decision periods flagged as unusually long (in red) compared to normal periods (in green). 
                  A high proportion of long decisions suggests systematic delays, 
                  which can undermine predictability for suppliers and stall public projects.")
            ),
            plotOutput("decision_long_plot", height = "500px"),
            downloadButton("download_dec_long", "Download Figure", class = "download-btn btn-sm")
          )
        ),
        
        div(class = "question-header",
            "Which buyers have the longest decision periods?"
        ),
        
        fluidRow(
          box(
            title = "Long Decision Periods by Buyer Group",
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            div(class = "description-box",
                p("These stacked bar charts show which buyer groups most frequently experience long decision periods. 
                  The left panel displays contract counts, while the right panel shows contract values. 
                  High shares of delayed decisions in a specific buyer group may indicate bottlenecks in evaluation, 
                  internal approval, or oversight procedures that need to be addressed.")
            ),
            plotOutput("buyer_long_plot", height = "600px"),
            downloadButton("download_buyer_long", "Download Figure", class = "download-btn btn-sm")
          )
        )
      ),
      
      # ====================================================================
      # REGRESSION ANALYSIS TAB
      # ====================================================================
      tabItem(
        tabName = "regression",
        h2("Administrative Efficiency and Competition"),
        
        # FILTERS BOX
        fluidRow(
          box(
            title = "Filters",
            width = 12,
            collapsible = TRUE,
            collapsed = FALSE,
            status = "info",
            
            fluidRow(
              column(2, uiOutput("year_filter_reg")),
              column(2, uiOutput("market_filter_reg")),
              column(2, uiOutput("value_filter_reg")),
              column(3, uiOutput("buyer_type_filter_reg")),
              column(3, uiOutput("procedure_type_filter_reg"))
            ),
            
            fluidRow(
              column(12,
                     actionButton("apply_filters_reg", "Apply Filters", icon = icon("filter"), class = "btn-primary"),
                     actionButton("reset_filters_reg", "Reset Filters", icon = icon("undo"), class = "btn-warning"),
                     textOutput("filter_status_reg", inline = TRUE)
              )
            )
          )
        ),
        
        div(class = "question-header",
            "Is administrative efficiency linked to competition?"
        ),
        
        # SHORT SUBMISSION PERIOD REGRESSION
        fluidRow(
          box(
            title = "Effect of Short Submission Periods on Single Bidding",
            width = 12,
            solidHeader = TRUE,
            status = "info",
            div(class = "description-box",
                p("This figure summarizes how the likelihood of single bidding changes when submission periods are unusually short. 
                  The model-based prediction accounts for differences in buyer type, procedure type, and year. 
                  If the line for short deadlines sits noticeably higher, this suggests that tight timelines 
                  may be restricting competition and making single bidding more likely. 
                  The shaded area represents the 95% confidence interval around the prediction.")
            ),
            uiOutput("short_reg_plot_ui"),
            uiOutput("download_short_reg_ui")
          )
        ),
        
        # SHORT SUBMISSION SENSITIVITY
        fluidRow(
          box(
            title = "Sensitivity Analysis: Short Submission Period Model",
            width = 12,
            solidHeader = TRUE,
            status = "info",
            collapsible = TRUE,
            collapsed = TRUE,
            div(class = "description-box",
                p("This section tests the robustness of the short submission period finding across different model specifications. 
                  The pipeline runs multiple versions of the logit model with varying combinations of: 
                  fixed effects (buyer FE, year FE, or both), clustering (by buyer, year, or buyer type), 
                  and control variables (procedure type, buyer type). 
                  Use this analysis to verify that the core finding about short submission periods and single-bidding 
                  holds across reasonable modeling choices and is not driven by a single specification.")
            ),
            uiOutput("sensitivity_short_ui")
          )
        ),
        
        # LONG DECISION PERIOD REGRESSION
        fluidRow(
          box(
            title = "Effect of Long Decision Periods on Single Bidding",
            width = 12,
            solidHeader = TRUE,
            status = "info",
            div(class = "description-box",
                p("This figure assesses whether very long decision periods are associated with single bidding. 
                  The horizontal axis compares tenders decided within a normal time frame to those with delayed decisions. 
                  The vertical axis shows the predicted probability of single bidding, after accounting for buyer type and year. 
                  A higher predicted probability for delayed decisions may indicate strategic behavior 
                  or weakened competitive pressure in tenders that drag on.")
            ),
            uiOutput("long_reg_plot_ui"),
            uiOutput("download_long_reg_ui")
          )
        ),
        
        # LONG DECISION SENSITIVITY
        fluidRow(
          box(
            title = "Sensitivity Analysis: Long Decision Period Model",
            width = 12,
            solidHeader = TRUE,
            status = "info",
            collapsible = TRUE,
            collapsed = TRUE,
            div(class = "description-box",
                p("This section tests the robustness of the long decision period finding across different model specifications. 
                  The pipeline runs multiple versions of the logit model with varying combinations of: 
                  fixed effects (buyer FE, year FE, or both), clustering (by buyer, year, or buyer type), 
                  and control variables (procedure type, buyer type). 
                  Use this analysis to assess whether the relationship between long decision periods and single bidding 
                  is consistent across different modeling assumptions.")
            ),
            uiOutput("sensitivity_long_ui")
          )
        )
      ),
      
      # ====================================================================
      # EXPORT TAB
      # ====================================================================
      tabItem(
        tabName = "export",
        h2("Export Reports and Figures"),
        
        fluidRow(
          box(
            title = "Complete Reports",
            width = 6,
            solidHeader = TRUE,
            status = "primary",
            p("Generate comprehensive analysis reports in PDF or Word format."),
            downloadButton("download_pdf_report", "Download PDF Report", 
                           class = "btn-primary btn-lg download-btn"),
            br(), br(),
            downloadButton("download_word_report", "Download Word Report", 
                           class = "btn-info btn-lg download-btn")
          ),
          box(
            title = "All Figures",
            width = 6,
            solidHeader = TRUE,
            status = "success",
            p("Download all analysis figures as a ZIP file."),
            downloadButton("download_all_figures", "Download All Figures (ZIP)", 
                           class = "btn-success btn-lg download-btn")
          )
        ),
        
        fluidRow(
          box(
            title = "Report Contents",
            width = 12,
            solidHeader = TRUE,
            status = "info",
            p("The reports will include:"),
            tags$ul(
              tags$li("Executive summary"),
              tags$li("Data overview and statistics"),
              tags$li("Procedure type analysis"),
              tags$li("Submission period analysis"),
              tags$li("Decision period analysis"),
              tags$li("Buyer-level breakdowns"),
              tags$li("Regression results and sensitivity analysis"),
              tags$li("All visualizations")
            )
          )
        )
      )
    )
  )
)

# ========================================================================
# SERVER (PART 1 - DATA LOADING AND REACTIVE VALUES)
# ========================================================================

server <- function(input, output, session) {
  
  # Reactive values
  results <- reactiveValues(
    data = NULL,
    analysis = NULL,
    filtered_data = NULL,
    filtered_analysis = NULL,
    country_code = NULL,
    value_divisor = 1
  )
  
  filters <- reactiveValues(
    overview = list(year = NULL, market = NULL, value = NULL, buyer_type = NULL, procedure_type = NULL),
    proc = list(year = NULL, market = NULL, value = NULL, buyer_type = NULL, procedure_type = NULL),
    subm = list(year = NULL, market = NULL, value = NULL, buyer_type = NULL, procedure_type = NULL),
    dec = list(year = NULL, market = NULL, value = NULL, buyer_type = NULL, procedure_type = NULL),
    reg = list(year = NULL, market = NULL, value = NULL, buyer_type = NULL, procedure_type = NULL)
  )
  
  # Data loading
  observeEvent(input$run_analysis, {
    req(input$datafile, input$country_code)
    
    withProgress(message = 'Loading data...', value = 0, {
      incProgress(0.2, detail = "Reading file...")
      
      tryCatch({
        df <- fread(
          input$datafile$datapath,
          keepLeadingZeros = TRUE,
          encoding = "UTF-8",
          stringsAsFactors = FALSE,
          showProgress = FALSE,
          na.strings = c("", "-", "NA")
        )
        
        dup_cols <- duplicated(names(df))
        if (any(dup_cols)) {
          df <- df[, !dup_cols, with = FALSE]
        }
        
        # Convert to regular dataframe and add tender_year
        df <- as.data.frame(df)
        df <- add_tender_year(df)
        
        incProgress(0.3, detail = "Running analysis...")
        
        country_code <- toupper(input$country_code)
        
        analysis_results <- run_admin_efficiency_pipeline(
          df = df,
          country_code = country_code,
          output_dir = tempdir()
        )
        
        incProgress(0.9, detail = "Finalizing...")
        
        results$data <- df
        results$analysis <- analysis_results
        results$filtered_data <- df
        results$filtered_analysis <- analysis_results
        results$country_code <- country_code
        
        output$analysis_status <- renderText({
          paste0("✓ Analysis complete!\n",
                 "Country: ", country_code, "\n",
                 "Rows: ", formatC(nrow(df), format = "d", big.mark = ","), "\n",
                 "Columns: ", ncol(df))
        })
        
        showNotification("Analysis complete! Navigate to other tabs to view results.", 
                         type = "message", duration = 5)
        
      }, error = function(e) {
        output$analysis_status <- renderText({
          paste("Error:", e$message)
        })
        showNotification(paste("Error:", e$message), type = "error", duration = NULL)
      })
    })
  })
  
  # ========================================================================
  # FILTER UI GENERATION (ALL TABS)
  # ========================================================================
  
  # Year filters
  output$year_filter_overview <- output$year_filter_proc <- 
    output$year_filter_subm <- output$year_filter_dec <- 
    output$year_filter_reg <- renderUI({
      req(results$data)
      
      year_col <- NULL
      if ("tender_year" %in% names(results$data)) year_col <- "tender_year"
      else if ("year" %in% names(results$data)) year_col <- "year"
      else if ("cal_year" %in% names(results$data)) year_col <- "cal_year"
      
      if (!is.null(year_col)) {
        years <- results$data[[year_col]]
        years <- years[!is.na(years)]
        
        if (length(years) > 0) {
          sliderInput("year_range", "Year Range:",
                      min = min(years),
                      max = max(years),
                      value = c(min(years), max(years)),
                      step = 1,
                      sep = "")
        }
      }
    })
  
  # Market filters
  output$market_filter_overview <- output$market_filter_proc <- 
    output$market_filter_subm <- output$market_filter_dec <- 
    output$market_filter_reg <- renderUI({
      req(results$data)
      
      if ("lot_productcode" %in% names(results$data)) {
        cpv_codes <- unique(substr(results$data$lot_productcode, 1, 2))
        cpv_codes <- cpv_codes[!is.na(cpv_codes) & cpv_codes != ""]
        cpv_codes <- sort(cpv_codes)
        
        if (length(cpv_codes) > 0) {
          pickerInput("market_filter", "Market (CPV):",
                      choices = c("All", cpv_codes),
                      selected = "All",
                      multiple = TRUE,
                      options = list(`actions-box` = TRUE))
        }
      }
    })
  
  # Value filters - EXACTLY LIKE INTEGRITY APP
  output$value_filter_overview <- output$value_filter_proc <- 
    output$value_filter_subm <- output$value_filter_dec <- 
    output$value_filter_reg <- renderUI({
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
          
          # Scale based on magnitude - EXACTLY LIKE INTEGRITY APP
          if (max_val_raw >= 1e9) {
            max_val <- round(max_val_raw / 1e9, 1) * 1e9
            step_val <- 1e8
            label_text <- "Contract Value Range (Billions USD):"
            pre_text <- "$"
            post_text <- "B"
            divisor <- 1e9
          } else if (max_val_raw >= 1e8) {
            max_val <- round(max_val_raw / 1e7) * 1e7
            step_val <- 1e7
            label_text <- "Contract Value Range (Millions USD):"
            pre_text <- "$"
            post_text <- "M"
            divisor <- 1e6
          } else if (max_val_raw >= 1e7) {
            max_val <- round(max_val_raw / 1e6) * 1e6
            step_val <- 1e6
            label_text <- "Contract Value Range (Millions USD):"
            pre_text <- "$"
            post_text <- "M"
            divisor <- 1e6
          } else if (max_val_raw >= 1e6) {
            max_val <- round(max_val_raw / 1e5) * 1e5
            step_val <- 1e5
            label_text <- "Contract Value Range (Millions USD):"
            pre_text <- "$"
            post_text <- "M"
            divisor <- 1e6
          } else if (max_val_raw >= 100000) {
            max_val <- round(max_val_raw / 1e4) * 1e4
            step_val <- 1e4
            label_text <- "Contract Value Range (Thousands USD):"
            pre_text <- "$"
            post_text <- "K"
            divisor <- 1e3
          } else {
            max_val <- round(max_val_raw / 1e3) * 1e3
            step_val <- 1e3
            label_text <- "Contract Value Range (USD):"
            pre_text <- "$"
            post_text <- ""
            divisor <- 1
          }
          
          results$value_divisor <- divisor
          
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
  
  # Buyer type filters
  output$buyer_type_filter_overview <- output$buyer_type_filter_proc <- 
    output$buyer_type_filter_subm <- output$buyer_type_filter_dec <- 
    output$buyer_type_filter_reg <- renderUI({
      req(results$data)
      
      if ("buyer_buyertype" %in% names(results$data)) {
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
  
  # Procedure type filters
  output$procedure_type_filter_overview <- output$procedure_type_filter_proc <- 
    output$procedure_type_filter_subm <- output$procedure_type_filter_dec <- 
    output$procedure_type_filter_reg <- renderUI({
      req(results$data)
      
      if ("tender_proceduretype" %in% names(results$data)) {
        proc_types <- unique(results$data$tender_proceduretype)
        proc_types <- proc_types[!is.na(proc_types)]
        proc_types <- sort(proc_types)
        
        if (length(proc_types) > 0) {
          pickerInput("procedure_type_filter", "Procedure Type:",
                      choices = c("All", proc_types),
                      selected = "All",
                      multiple = TRUE,
                      options = list(`actions-box` = TRUE, `live-search` = TRUE))
        }
      }
    })
  
  # ========================================================================
  # FILTER APPLICATION (SEPARATE FOR EACH TAB)
  # ========================================================================
  
  # Helper to apply filters for a tab
  apply_tab_filters <- function(tab_name) {
    req(results$data, results$analysis)
    
    current_filters <- list(
      year = input$year_range,
      market = input$market_filter,
      value = input$value_range,
      buyer_type = input$buyer_type_filter,
      procedure_type = input$procedure_type_filter
    )
    
    # Store filters for this tab
    if (tab_name == "overview") filters$overview <- current_filters
    else if (tab_name == "proc") filters$proc <- current_filters
    else if (tab_name == "subm") filters$subm <- current_filters
    else if (tab_name == "dec") filters$dec <- current_filters
    else if (tab_name == "reg") filters$reg <- current_filters
    
    # Apply filters to data
    filtered <- filter_data(
      df = results$data,
      year_range = current_filters$year,
      market = current_filters$market,
      value_range = current_filters$value,
      buyer_type = current_filters$buyer_type,
      procedure_type = current_filters$procedure_type,
      value_divisor = results$value_divisor
    )
    
    # Ensure tender_year is in filtered data
    if (!"tender_year" %in% names(filtered)) {
      filtered <- add_tender_year(filtered)
    }
    
    # Re-run analysis on filtered data
    withProgress(message = 'Applying filters...', value = 0, {
      incProgress(0.5, detail = "Re-analyzing data...")
      
      tryCatch({
        filtered_analysis <- run_admin_efficiency_pipeline(
          df = filtered,
          country_code = results$country_code,
          output_dir = tempdir()
        )
        
        results$filtered_data <- filtered
        results$filtered_analysis <- filtered_analysis
        
        showNotification(paste("Filters applied:", nrow(filtered), "contracts"), 
                         type = "message", duration = 3)
      }, error = function(e) {
        showNotification(paste("Error applying filters:", e$message), 
                         type = "error", duration = 5)
      })
    })
  }
  
  # Apply filter observers
  observeEvent(input$apply_filters_overview, { apply_tab_filters("overview") })
  observeEvent(input$apply_filters_proc, { apply_tab_filters("proc") })
  observeEvent(input$apply_filters_subm, { apply_tab_filters("subm") })
  observeEvent(input$apply_filters_dec, { apply_tab_filters("dec") })
  observeEvent(input$apply_filters_reg, { apply_tab_filters("reg") })
  
  # Reset filter observers
  observeEvent(input$reset_filters_overview, {
    filters$overview <- list(year = NULL, market = NULL, value = NULL, buyer_type = NULL, procedure_type = NULL)
    results$filtered_data <- results$data
    results$filtered_analysis <- results$analysis
    showNotification("Filters reset", type = "message", duration = 2)
  })
  
  observeEvent(input$reset_filters_proc, {
    filters$proc <- list(year = NULL, market = NULL, value = NULL, buyer_type = NULL, procedure_type = NULL)
    results$filtered_data <- results$data
    results$filtered_analysis <- results$analysis
    showNotification("Filters reset", type = "message", duration = 2)
  })
  
  observeEvent(input$reset_filters_subm, {
    filters$subm <- list(year = NULL, market = NULL, value = NULL, buyer_type = NULL, procedure_type = NULL)
    results$filtered_data <- results$data
    results$filtered_analysis <- results$analysis
    showNotification("Filters reset", type = "message", duration = 2)
  })
  
  observeEvent(input$reset_filters_dec, {
    filters$dec <- list(year = NULL, market = NULL, value = NULL, buyer_type = NULL, procedure_type = NULL)
    results$filtered_data <- results$data
    results$filtered_analysis <- results$analysis
    showNotification("Filters reset", type = "message", duration = 2)
  })
  
  observeEvent(input$reset_filters_reg, {
    filters$reg <- list(year = NULL, market = NULL, value = NULL, buyer_type = NULL, procedure_type = NULL)
    results$filtered_data <- results$data
    results$filtered_analysis <- results$analysis
    showNotification("Filters reset", type = "message", duration = 2)
  })
  
  # Filter status outputs
  output$filter_status_overview <- renderText({
    paste("  ", get_filter_description(filters$overview))
  })
  
  output$filter_status_proc <- renderText({
    paste("  ", get_filter_description(filters$proc))
  })
  
  output$filter_status_subm <- renderText({
    paste("  ", get_filter_description(filters$subm))
  })
  
  output$filter_status_dec <- renderText({
    paste("  ", get_filter_description(filters$dec))
  })
  
  output$filter_status_reg <- renderText({
    paste("  ", get_filter_description(filters$reg))
  })
  
  # ========================================================================
  # DATA OVERVIEW OUTPUTS
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
    df <- results$filtered_data
    n_buyers <- if ("buyer_masterid" %in% names(df)) {
      length(unique(df$buyer_masterid))
    } else "N/A"
    valueBox(
      formatC(n_buyers, format = "d", big.mark = ","),
      "Unique Buyers",
      icon = icon("building"),
      color = "green"
    )
  })
  
  output$n_suppliers <- renderValueBox({
    req(results$filtered_data)
    df <- results$filtered_data
    n_suppliers <- if ("bidder_masterid" %in% names(df)) {
      length(unique(df$bidder_masterid))
    } else "N/A"
    valueBox(
      formatC(n_suppliers, format = "d", big.mark = ","),
      "Unique Suppliers",
      icon = icon("truck"),
      color = "yellow"
    )
  })
  
  output$n_years <- renderValueBox({
    req(results$filtered_data)
    df <- results$filtered_data
    
    year_col <- NULL
    if ("tender_year" %in% names(df)) year_col <- "tender_year"
    else if ("year" %in% names(df)) year_col <- "year"
    else if ("cal_year" %in% names(df)) year_col <- "cal_year"
    
    years <- if (!is.null(year_col)) {
      unique(df[[year_col]][!is.na(df[[year_col]])])
    } else NA
    
    year_range <- if (length(years) > 0) {
      paste(min(years), "-", max(years))
    } else "N/A"
    
    valueBox(
      year_range,
      "Years Covered",
      icon = icon("calendar"),
      color = "red"
    )
  })
  
  # Contracts per year plot - GENERATED FROM PIPELINE
  output$contracts_year_plot <- renderPlot({
    req(results$filtered_analysis)
    req(results$filtered_analysis$summary_stats)
    
    if (!is.null(results$filtered_analysis$summary_stats$n_obs_per_year)) {
      stats <- results$filtered_analysis$summary_stats$n_obs_per_year
      
      ggplot(stats, aes(x = tender_year, y = n_observations)) +
        geom_col(fill = "#3c8dbc") +
        geom_text(aes(label = formatC(n_observations, format = "d", big.mark = ",")),
                  vjust = -0.5, size = 4) +
        labs(x = "Year", y = "Number of Contracts", title = "Contracts per Year") +
        theme_minimal(base_size = 14) +
        scale_y_continuous(labels = scales::comma)
    }
  })
  
  output$summary_table <- DT::renderDataTable({
    req(results$filtered_analysis$summary_stats)
    stats <- results$filtered_analysis$summary_stats
    
    # Build rows one by one to avoid dimension mismatch
    rows <- list()
    
    # n_unique_buyers
    if (!is.null(stats$n_unique_buyers)) {
      rows[[length(rows) + 1]] <- data.frame(
        Metric = "Unique Buyers",
        Value = format(stats$n_unique_buyers, big.mark = ","),
        stringsAsFactors = FALSE
      )
    }
    
    # n_unique_bidders
    if (!is.null(stats$n_unique_bidders)) {
      rows[[length(rows) + 1]] <- data.frame(
        Metric = "Unique Bidders",
        Value = format(stats$n_unique_bidders, big.mark = ","),
        stringsAsFactors = FALSE
      )
    }
    
    # Year range from n_obs_per_year
    if (!is.null(stats$n_obs_per_year) && nrow(stats$n_obs_per_year) > 0) {
      years <- stats$n_obs_per_year$tender_year
      rows[[length(rows) + 1]] <- data.frame(
        Metric = "Year Range",
        Value = paste(min(years), "-", max(years)),
        stringsAsFactors = FALSE
      )
      
      rows[[length(rows) + 1]] <- data.frame(
        Metric = "Total Years",
        Value = as.character(length(unique(years))),
        stringsAsFactors = FALSE
      )
    }
    
    # Combine all rows
    if (length(rows) > 0) {
      stats_df <- do.call(rbind, rows)
    } else {
      stats_df <- data.frame(
        Metric = "No summary statistics available",
        Value = "",
        stringsAsFactors = FALSE
      )
    }
    
    datatable(stats_df, options = list(pageLength = 20, dom = 't'), rownames = FALSE)
  })
  
  # ========================================================================
  # PROCEDURE TYPES OUTPUTS
  # ========================================================================
  
  output$procedure_share_value_plot <- renderPlot({
    req(results$filtered_analysis$sh)
    print(results$filtered_analysis$sh)
  })
  
  output$procedure_share_count_plot <- renderPlot({
    req(results$filtered_analysis$p_count)
    print(results$filtered_analysis$p_count)
  })
  
  output$procedure_combined_plot <- renderPlot({
    req(results$filtered_analysis$combined_proc)
    print(results$filtered_analysis$combined_proc)
  })
  
  output$procedure_share_time_plot <- renderPlot({
    req(results$filtered_analysis$sh)
    print(results$filtered_analysis$sh)
  })
  
  # ========================================================================
  # SUBMISSION PERIODS OUTPUTS
  # ========================================================================
  
  output$submission_dist_plot <- renderPlot({
    req(results$filtered_analysis$subm)
    print(results$filtered_analysis$subm)
  })
  
  output$submission_proc_plot <- renderPlot({
    req(results$filtered_analysis$subm_proc_facet_q)
    print(results$filtered_analysis$subm_proc_facet_q)
  })
  
  output$submission_short_plot <- renderPlot({
    req(results$filtered_analysis$subm_r)
    print(results$filtered_analysis$subm_r)
  })
  
  output$buyer_short_plot <- renderPlot({
    req(results$filtered_analysis$combined_short_buyer)
    print(results$filtered_analysis$combined_short_buyer)
  })
  
  # ========================================================================
  # DECISION PERIODS OUTPUTS
  # ========================================================================
  
  output$decision_dist_plot <- renderPlot({
    req(results$filtered_analysis$decp)
    print(results$filtered_analysis$decp)
  })
  
  output$decision_proc_plot <- renderPlot({
    req(results$filtered_analysis$decp_proc_facet_q)
    print(results$filtered_analysis$decp_proc_facet_q)
  })
  
  output$decision_long_plot <- renderPlot({
    req(results$filtered_analysis$decp_r)
    print(results$filtered_analysis$decp_r)
  })
  
  output$buyer_long_plot <- renderPlot({
    req(results$filtered_analysis$combined_dec_plot)
    print(results$filtered_analysis$combined_dec_plot)
  })
  
  # ========================================================================
  # REGRESSION OUTPUTS - WITH SENSITIVITY ANALYSIS LIKE INTEGRITY APP
  # ========================================================================
  
  output$short_reg_plot_ui <- renderUI({
    if (!is.null(results$filtered_analysis$plot_short_reg)) {
      plotOutput("short_reg_plot", height = "600px")
    } else {
      p("No regression results available (insufficient data or model did not converge).")
    }
  })
  
  output$short_reg_plot <- renderPlot({
    req(results$filtered_analysis$plot_short_reg)
    print(results$filtered_analysis$plot_short_reg)
  })
  
  output$download_short_reg_ui <- renderUI({
    if (!is.null(results$filtered_analysis$plot_short_reg)) {
      downloadButton("download_short_reg", "Download Figure", class = "download-btn btn-sm")
    }
  })
  
  # Sensitivity analysis for short submission - EXACTLY LIKE INTEGRITY APP
  output$sensitivity_short_ui <- renderUI({
    req(results$filtered_analysis$sensitivity_short)
    
    bundle <- results$filtered_analysis$sensitivity_short
    
    # Helper to check if table has rows
    has_rows <- function(tbl) {
      !is.null(tbl) && is.data.frame(tbl) && nrow(tbl) > 0
    }
    
    # Extract key metrics
    share_pos <- if (has_rows(bundle$overall)) bundle$overall$share_positive else NA
    share_neg <- if (has_rows(bundle$overall)) bundle$overall$share_negative else NA
    share_p10 <- if (has_rows(bundle$overall)) {
      cols <- names(bundle$overall)
      p10_col <- cols[grepl("share_p_le_0\\.1", cols)]
      if (length(p10_col) > 0) bundle$overall[[p10_col[1]]] else NA
    } else NA
    median_est <- if (has_rows(bundle$overall)) bundle$overall$median_estimate else NA
    median_p <- if (has_rows(bundle$overall)) bundle$overall$median_pvalue else NA
    sign_stable <- if (has_rows(bundle$sign)) bundle$sign$share_sign_stable else NA
    
    # Determine conclusion
    if (!is.na(share_pos) && !is.na(share_p10) && !is.na(sign_stable)) {
      if (share_pos >= 0.7 && share_p10 >= 0.6 && sign_stable == 1) {
        conclusion_text <- "✓ Strong and robust evidence. The relationship is positive and holds across most specifications."
        conclusion_color <- "success"
      } else if (share_pos >= 0.6 && share_p10 >= 0.3) {
        conclusion_text <- "⚠ Moderate evidence. The relationship is mostly positive but varies somewhat across specifications."
        conclusion_color <- "warning"
      } else {
        conclusion_text <- "✗ Weak or mixed evidence. Results vary substantially across specifications."
        conclusion_color <- "danger"
      }
    } else {
      conclusion_text <- "ℹ Sensitivity summary not available."
      conclusion_color <- "info"
    }
    
    tagList(
      # Introduction
      div(class = "alert alert-info", style = "margin-bottom: 20px;",
          h4(style = "margin-top: 0;", "What is sensitivity analysis?"),
          p("Sensitivity analysis tests whether the relationship between short submission periods and single bidding holds across different modeling choices. A robust finding should be consistent across reasonable variations in:"),
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
                  if (has_rows(bundle$overall) && has_rows(bundle$sign)) {
                    renderTable({
                      dplyr::bind_cols(bundle$overall, bundle$sign)
                    }, digits = 3, striped = TRUE, hover = TRUE, bordered = TRUE, width = "auto")
                  } else {
                    p(em("Summary statistics not available."))
                  }
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
                p("Fixed effects control for unobserved heterogeneity:"),
                tags$ul(
                  tags$li(strong("Buyer FE:"), " Controls for buyer-specific characteristics"),
                  tags$li(strong("Year FE:"), " Controls for time trends"),
                  tags$li(strong("Buyer + Year FE:"), " Controls for both simultaneously")
                ),
                if (has_rows(bundle$by_fe)) {
                  renderTable({ bundle$by_fe }, digits = 3, striped = TRUE, hover = TRUE, bordered = TRUE, width = "auto")
                } else {
                  p(em("No fixed-effects breakdown available."))
                }
            )
        ),
        
        # By Clustering
        div(class = "panel panel-default",
            div(class = "panel-heading", h5(class = "panel-title", style = "margin: 0;", "By Clustering")),
            div(class = "panel-body",
                p("Clustering adjusts standard errors for correlation within groups:"),
                tags$ul(
                  tags$li(strong("Buyer clustering:"), " Accounts for correlation within the same buyer over time"),
                  tags$li(strong("Year clustering:"), " Accounts for common shocks affecting all buyers in a year")
                ),
                if (has_rows(bundle$by_cluster)) {
                  renderTable({ bundle$by_cluster }, digits = 3, striped = TRUE, hover = TRUE, bordered = TRUE, width = "auto")
                } else {
                  p(em("No clustering breakdown available."))
                }
            )
        ),
        
        # By Controls
        div(class = "panel panel-default",
            div(class = "panel-heading", h5(class = "panel-title", style = "margin: 0;", "By Control Variables")),
            div(class = "panel-body",
                p("Control variables account for confounding factors:"),
                if (has_rows(bundle$by_controls)) {
                  renderTable({ bundle$by_controls }, digits = 3, striped = TRUE, hover = TRUE, bordered = TRUE, width = "auto")
                } else {
                  p(em("No controls breakdown available."))
                }
            )
        )
      )
    )
  })
  
  output$long_reg_plot_ui <- renderUI({
    if (!is.null(results$filtered_analysis$plot_long_reg)) {
      plotOutput("long_reg_plot", height = "600px")
    } else {
      p("No regression results available (insufficient data or model did not converge).")
    }
  })
  
  output$long_reg_plot <- renderPlot({
    req(results$filtered_analysis$plot_long_reg)
    print(results$filtered_analysis$plot_long_reg)
  })
  
  output$download_long_reg_ui <- renderUI({
    if (!is.null(results$filtered_analysis$plot_long_reg)) {
      downloadButton("download_long_reg", "Download Figure", class = "download-btn btn-sm")
    }
  })
  
  # Sensitivity analysis for long decision - EXACTLY LIKE INTEGRITY APP
  output$sensitivity_long_ui <- renderUI({
    req(results$filtered_analysis$sensitivity_long)
    
    bundle <- results$filtered_analysis$sensitivity_long
    
    has_rows <- function(tbl) {
      !is.null(tbl) && is.data.frame(tbl) && nrow(tbl) > 0
    }
    
    share_pos <- if (has_rows(bundle$overall)) bundle$overall$share_positive else NA
    share_neg <- if (has_rows(bundle$overall)) bundle$overall$share_negative else NA
    share_p10 <- if (has_rows(bundle$overall)) {
      cols <- names(bundle$overall)
      p10_col <- cols[grepl("share_p_le_0\\.1", cols)]
      if (length(p10_col) > 0) bundle$overall[[p10_col[1]]] else NA
    } else NA
    median_est <- if (has_rows(bundle$overall)) bundle$overall$median_estimate else NA
    median_p <- if (has_rows(bundle$overall)) bundle$overall$median_pvalue else NA
    sign_stable <- if (has_rows(bundle$sign)) bundle$sign$share_sign_stable else NA
    
    if (!is.na(share_pos) && !is.na(share_p10) && !is.na(sign_stable)) {
      if (share_pos >= 0.7 && share_p10 >= 0.6 && sign_stable == 1) {
        conclusion_text <- "✓ Strong and robust evidence. The relationship is positive and holds across most specifications."
        conclusion_color <- "success"
      } else if (share_pos >= 0.6 && share_p10 >= 0.3) {
        conclusion_text <- "⚠ Moderate evidence. The relationship is mostly positive but varies somewhat across specifications."
        conclusion_color <- "warning"
      } else {
        conclusion_text <- "✗ Weak or mixed evidence. Results vary substantially across specifications."
        conclusion_color <- "danger"
      }
    } else {
      conclusion_text <- "ℹ Sensitivity summary not available."
      conclusion_color <- "info"
    }
    
    tagList(
      div(class = "alert alert-info", style = "margin-bottom: 20px;",
          h4(style = "margin-top: 0;", "What is sensitivity analysis?"),
          p("Sensitivity analysis tests whether the relationship between long decision periods and single bidding holds across different modeling choices."),
          tags$ul(
            tags$li(strong("Fixed effects (FE):"), " Controls for unobserved differences across buyers, years, etc."),
            tags$li(strong("Clustering:"), " Adjusts standard errors for correlation within groups"),
            tags$li(strong("Control variables:"), " Additional factors that might affect the outcome")
          )
      ),
      
      div(class = "panel panel-primary",
          div(class = "panel-heading", h4(class = "panel-title", style = "margin: 0;", "Overall Summary")),
          div(class = "panel-body",
              div(style = "overflow-x: auto; max-width: 100%;",
                  if (has_rows(bundle$overall) && has_rows(bundle$sign)) {
                    renderTable({
                      dplyr::bind_cols(bundle$overall, bundle$sign)
                    }, digits = 3, striped = TRUE, hover = TRUE, bordered = TRUE, width = "auto")
                  } else {
                    p(em("Summary statistics not available."))
                  }
              )
          )
      ),
      
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
      
      tags$details(
        tags$summary(strong("Show detailed breakdown by model components")),
        
        div(class = "panel panel-default", style = "margin-top: 15px;",
            div(class = "panel-heading", h5(class = "panel-title", style = "margin: 0;", "By Fixed Effects (FE)")),
            div(class = "panel-body",
                if (has_rows(bundle$by_fe)) {
                  renderTable({ bundle$by_fe }, digits = 3, striped = TRUE, hover = TRUE, bordered = TRUE, width = "auto")
                } else {
                  p(em("No fixed-effects breakdown available."))
                }
            )
        ),
        
        div(class = "panel panel-default",
            div(class = "panel-heading", h5(class = "panel-title", style = "margin: 0;", "By Clustering")),
            div(class = "panel-body",
                if (has_rows(bundle$by_cluster)) {
                  renderTable({ bundle$by_cluster }, digits = 3, striped = TRUE, hover = TRUE, bordered = TRUE, width = "auto")
                } else {
                  p(em("No clustering breakdown available."))
                }
            )
        ),
        
        div(class = "panel panel-default",
            div(class = "panel-heading", h5(class = "panel-title", style = "margin: 0;", "By Control Variables")),
            div(class = "panel-body",
                if (has_rows(bundle$by_controls)) {
                  renderTable({ bundle$by_controls }, digits = 3, striped = TRUE, hover = TRUE, bordered = TRUE, width = "auto")
                } else {
                  p(em("No controls breakdown available."))
                }
            )
        )
      )
    )
  })
  
  # ========================================================================
  # DOWNLOAD HANDLERS
  # ========================================================================
  
  # Helper to get the appropriate filter for current tab
  get_current_tab_filter <- function() {
    current_tab <- input$sidebar
    
    switch(current_tab,
           "data_overview" = filters$overview,
           "procedures" = filters$proc,
           "submission" = filters$subm,
           "decision" = filters$dec,
           "regression" = filters$reg,
           list(year = NULL, market = NULL, value = NULL, buyer_type = NULL, procedure_type = NULL)
    )
  }
  
  output$download_contracts_year <- downloadHandler(
    filename = function() { paste0("contracts_per_year_", results$country_code, ".png") },
    content = function(file) {
      req(results$filtered_analysis$summary_stats$n_obs_per_year)
      stats <- results$filtered_analysis$summary_stats$n_obs_per_year
      
      filter_caption <- get_filter_caption(get_current_tab_filter())
      
      p <- ggplot(stats, aes(x = tender_year, y = n_observations)) +
        geom_col(fill = "#3c8dbc") +
        geom_text(aes(label = formatC(n_observations, format = "d", big.mark = ",")),
                  vjust = -0.5, size = 4) +
        labs(x = "Year", y = "Number of Contracts", title = "Contracts per Year") +
        theme_minimal(base_size = 14) +
        scale_y_continuous(labels = scales::comma)
      
      if (filter_caption != "") {
        p <- p + labs(caption = filter_caption)
      }
      
      ggsave(file, p, width = 10, height = 8, dpi = 300)
    }
  )
  
  output$download_proc_share_value <- downloadHandler(
    filename = function() { paste0("procedure_share_value_", results$country_code, ".png") },
    content = function(file) {
      req(results$filtered_analysis$sh)
      filter_caption <- get_filter_caption(filters$proc)
      plot_to_save <- if (filter_caption != "") {
        results$filtered_analysis$sh + labs(caption = filter_caption)
      } else {
        results$filtered_analysis$sh
      }
      ggsave(file, plot_to_save, width = 10, height = 8, dpi = 300)
    }
  )
  
  output$download_proc_share_count <- downloadHandler(
    filename = function() { paste0("procedure_share_count_", results$country_code, ".png") },
    content = function(file) {
      req(results$filtered_analysis$p_count)
      filter_caption <- get_filter_caption(filters$proc)
      plot_to_save <- if (filter_caption != "") {
        results$filtered_analysis$p_count + labs(caption = filter_caption)
      } else {
        results$filtered_analysis$p_count
      }
      ggsave(file, plot_to_save, width = 10, height = 8, dpi = 300)
    }
  )
  
  output$download_proc_combined <- downloadHandler(
    filename = function() { paste0("procedure_combined_", results$country_code, ".png") },
    content = function(file) {
      req(results$filtered_analysis$combined_proc)
      filter_caption <- get_filter_caption(filters$proc)
      plot_to_save <- if (filter_caption != "") {
        results$filtered_analysis$combined_proc + labs(caption = filter_caption)
      } else {
        results$filtered_analysis$combined_proc
      }
      ggsave(file, plot_to_save, width = 12, height = 10, dpi = 300)
    }
  )
  
  output$download_proc_share_time <- downloadHandler(
    filename = function() { paste0("procedure_share_time_", results$country_code, ".png") },
    content = function(file) {
      req(results$filtered_analysis$sh)
      filter_caption <- get_filter_caption(filters$proc)
      plot_to_save <- if (filter_caption != "") {
        results$filtered_analysis$sh + labs(caption = filter_caption)
      } else {
        results$filtered_analysis$sh
      }
      ggsave(file, plot_to_save, width = 12, height = 10, dpi = 300)
    }
  )
  
  output$download_subm_dist <- downloadHandler(
    filename = function() { paste0("submission_dist_", results$country_code, ".png") },
    content = function(file) {
      req(results$filtered_analysis$subm)
      filter_caption <- get_filter_caption(filters$subm)
      plot_to_save <- if (filter_caption != "") {
        results$filtered_analysis$subm + labs(caption = filter_caption)
      } else {
        results$filtered_analysis$subm
      }
      ggsave(file, plot_to_save, width = 12, height = 10, dpi = 300)
    }
  )
  
  output$download_subm_proc <- downloadHandler(
    filename = function() { paste0("submission_proc_", results$country_code, ".png") },
    content = function(file) {
      req(results$filtered_analysis$subm_proc_facet_q)
      filter_caption <- get_filter_caption(filters$subm)
      plot_to_save <- if (filter_caption != "") {
        results$filtered_analysis$subm_proc_facet_q + labs(caption = filter_caption)
      } else {
        results$filtered_analysis$subm_proc_facet_q
      }
      ggsave(file, plot_to_save, width = 12, height = 10, dpi = 300)
    }
  )
  
  output$download_subm_short <- downloadHandler(
    filename = function() { paste0("submission_short_", results$country_code, ".png") },
    content = function(file) {
      req(results$filtered_analysis$subm_r)
      filter_caption <- get_filter_caption(filters$subm)
      plot_to_save <- if (filter_caption != "") {
        results$filtered_analysis$subm_r + labs(caption = filter_caption)
      } else {
        results$filtered_analysis$subm_r
      }
      ggsave(file, plot_to_save, width = 10, height = 8, dpi = 300)
    }
  )
  
  output$download_buyer_short <- downloadHandler(
    filename = function() { paste0("buyer_short_", results$country_code, ".png") },
    content = function(file) {
      req(results$filtered_analysis$combined_short_buyer)
      filter_caption <- get_filter_caption(filters$subm)
      plot_to_save <- if (filter_caption != "") {
        results$filtered_analysis$combined_short_buyer + labs(caption = filter_caption)
      } else {
        results$filtered_analysis$combined_short_buyer
      }
      ggsave(file, plot_to_save, width = 12, height = 10, dpi = 300)
    }
  )
  
  output$download_dec_dist <- downloadHandler(
    filename = function() { paste0("decision_dist_", results$country_code, ".png") },
    content = function(file) {
      req(results$filtered_analysis$decp)
      filter_caption <- get_filter_caption(filters$dec)
      plot_to_save <- if (filter_caption != "") {
        results$filtered_analysis$decp + labs(caption = filter_caption)
      } else {
        results$filtered_analysis$decp
      }
      ggsave(file, plot_to_save, width = 12, height = 10, dpi = 300)
    }
  )
  
  output$download_dec_proc <- downloadHandler(
    filename = function() { paste0("decision_proc_", results$country_code, ".png") },
    content = function(file) {
      req(results$filtered_analysis$decp_proc_facet_q)
      filter_caption <- get_filter_caption(filters$dec)
      plot_to_save <- if (filter_caption != "") {
        results$filtered_analysis$decp_proc_facet_q + labs(caption = filter_caption)
      } else {
        results$filtered_analysis$decp_proc_facet_q
      }
      ggsave(file, plot_to_save, width = 12, height = 10, dpi = 300)
    }
  )
  
  output$download_dec_long <- downloadHandler(
    filename = function() { paste0("decision_long_", results$country_code, ".png") },
    content = function(file) {
      req(results$filtered_analysis$decp_r)
      filter_caption <- get_filter_caption(filters$dec)
      plot_to_save <- if (filter_caption != "") {
        results$filtered_analysis$decp_r + labs(caption = filter_caption)
      } else {
        results$filtered_analysis$decp_r
      }
      ggsave(file, plot_to_save, width = 10, height = 8, dpi = 300)
    }
  )
  
  output$download_buyer_long <- downloadHandler(
    filename = function() { paste0("buyer_long_", results$country_code, ".png") },
    content = function(file) {
      req(results$filtered_analysis$combined_dec_plot)
      filter_caption <- get_filter_caption(filters$dec)
      plot_to_save <- if (filter_caption != "") {
        results$filtered_analysis$combined_dec_plot + labs(caption = filter_caption)
      } else {
        results$filtered_analysis$combined_dec_plot
      }
      ggsave(file, plot_to_save, width = 12, height = 10, dpi = 300)
    }
  )
  
  output$download_short_reg <- downloadHandler(
    filename = function() { paste0("short_reg_", results$country_code, ".png") },
    content = function(file) {
      req(results$filtered_analysis$plot_short_reg)
      filter_caption <- get_filter_caption(filters$reg)
      plot_to_save <- if (filter_caption != "") {
        results$filtered_analysis$plot_short_reg + labs(caption = filter_caption)
      } else {
        results$filtered_analysis$plot_short_reg
      }
      ggsave(file, plot_to_save, width = 10, height = 8, dpi = 300)
    }
  )
  
  output$download_long_reg <- downloadHandler(
    filename = function() { paste0("long_reg_", results$country_code, ".png") },
    content = function(file) {
      req(results$filtered_analysis$plot_long_reg)
      filter_caption <- get_filter_caption(filters$reg)
      plot_to_save <- if (filter_caption != "") {
        results$filtered_analysis$plot_long_reg + labs(caption = filter_caption)
      } else {
        results$filtered_analysis$plot_long_reg
      }
      ggsave(file, plot_to_save, width = 10, height = 8, dpi = 300)
    }
  )
  
  output$download_pdf_report <- downloadHandler(
    filename = function() {
      paste0("admin_efficiency_report_", results$country_code, "_", 
             format(Sys.Date(), "%Y%m%d"), ".pdf")
    },
    content = function(file) {
      req(results$filtered_analysis, results$filtered_data, results$country_code)
      
      withProgress(message = 'Generating PDF report...', value = 0, {
        incProgress(0.3, detail = "Compiling document...")
        
        success <- generate_pdf_report(
          filtered_data = results$filtered_data,
          filtered_analysis = results$filtered_analysis,
          country_code = results$country_code,
          output_file = file,
          filters_text = ""
        )
        
        incProgress(0.7, detail = "Finalizing...")
        
        if (!success) {
          showNotification("Error generating PDF report.", type = "error", duration = 10)
        } else {
          showNotification("PDF report generated!", type = "message", duration = 5)
        }
      })
    }
  )
  
  output$download_word_report <- downloadHandler(
    filename = function() {
      paste0("admin_efficiency_report_", results$country_code, "_", 
             format(Sys.Date(), "%Y%m%d"), ".docx")
    },
    content = function(file) {
      req(results$filtered_analysis, results$filtered_data, results$country_code)
      
      withProgress(message = 'Generating Word document...', value = 0, {
        incProgress(0.3, detail = "Creating document...")
        
        success <- generate_word_report(
          filtered_data = results$filtered_data,
          filtered_analysis = results$filtered_analysis,
          country_code = results$country_code,
          output_file = file,
          filters_text = ""
        )
        
        incProgress(0.7, detail = "Finalizing...")
        
        if (!success) {
          showNotification("Error generating Word document.", type = "error", duration = 10)
        } else {
          showNotification("Word document generated!", type = "message", duration = 5)
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
          list(plot = results$filtered_analysis$sh, name = "procedure_share"),
          list(plot = results$filtered_analysis$p_count, name = "procedure_count"),
          list(plot = results$filtered_analysis$combined_proc, name = "procedure_combined"),
          list(plot = results$filtered_analysis$subm, name = "submission_dist"),
          list(plot = results$filtered_analysis$subm_proc_facet_q, name = "submission_proc"),
          list(plot = results$filtered_analysis$subm_r, name = "submission_short"),
          list(plot = results$filtered_analysis$combined_short_buyer, name = "buyer_short"),
          list(plot = results$filtered_analysis$decp, name = "decision_dist"),
          list(plot = results$filtered_analysis$decp_proc_facet_q, name = "decision_proc"),
          list(plot = results$filtered_analysis$decp_r, name = "decision_long"),
          list(plot = results$filtered_analysis$combined_dec_plot, name = "buyer_long"),
          list(plot = results$filtered_analysis$plot_short_reg, name = "short_reg"),
          list(plot = results$filtered_analysis$plot_long_reg, name = "long_reg")
        )
        
        n_plots <- length(plots)
        for (i in seq_along(plots)) {
          incProgress(i/n_plots, detail = paste("Saving figure", i, "of", n_plots))
          
          if (!is.null(plots[[i]]$plot)) {
            file_path <- file.path(temp_dir, paste0(plots[[i]]$name, "_", results$country_code, ".png"))
            tryCatch({
              ggsave(file_path, plots[[i]]$plot, width = 12, height = 10, dpi = 300)
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