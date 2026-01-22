# ========================================================================
# ECONOMIC OUTCOMES SHINY APP
# ========================================================================
# 
# This app provides an interactive interface for analyzing economic outcomes
# in public procurement, including market composition, supplier dynamics,
# buyer-supplier networks, relative price diagnostics, and competition metrics.
#
# Author: Generated based on econ_out_utils.R and econ_out_report.Rmd
# Date: January 2026
# ========================================================================

library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(dplyr)
library(data.table)
library(scales)
library(officer)
library(flextable)
library(rmarkdown)

# ========================================================================
# INCREASE FILE UPLOAD LIMIT
# ========================================================================
options(shiny.maxRequestSize = 1000*1024^2)

# Try to source the utils file - show clear error if missing
utils_loaded <- tryCatch({
  # Update this path to your actual econ_out_utils.R location
  source("C:/Users/wb589991/OneDrive - WBG/Documents/procuR.github.io/src/utils/econ_out_utils.R")
  TRUE
}, error = function(e) {
  warning("Could not load econ_out_utils.R - ", e$message)
  FALSE
})

# Create placeholder functions if utils not loaded
if (!utils_loaded) {
  warning("Running in demo mode without utils. Please update the source() path on line 15.")
  
  # Placeholder function
  run_economic_efficiency_pipeline <- function(...) {
    stop("econ_out_utils.R not loaded. Please update the source() path in the app file.")
  }
  
  build_cpv_lookup <- function(...) {
    return(NULL)
  }
}

# ========================================================================
# HELPER FUNCTIONS
# ========================================================================

# Helper to create filter caption text
get_filter_caption <- function(filters) {
  if (is.null(filters)) return("")
  
  parts <- character()
  
  if (!is.null(filters$year) && length(filters$year) == 2) {
    parts <- c(parts, sprintf("Years: %d-%d", filters$year[1], filters$year[2]))
  }
  
  if (!is.null(filters$market) && length(filters$market) > 0) {
    parts <- c(parts, sprintf("Markets: %s", paste(filters$market, collapse = ", ")))
  }
  
  if (!is.null(filters$value) && length(filters$value) == 2) {
    val_text <- sprintf("Value range applied")
    parts <- c(parts, val_text)
  }
  
  if (!is.null(filters$buyer_type) && length(filters$buyer_type) > 0) {
    parts <- c(parts, sprintf("Buyer types: %s", paste(filters$buyer_type, collapse = ", ")))
  }
  
  if (!is.null(filters$procedure_type) && length(filters$procedure_type) > 0) {
    parts <- c(parts, sprintf("Procedures: %s", paste(filters$procedure_type, collapse = ", ")))
  }
  
  if (length(parts) == 0) return("")
  
  paste("Filters applied:", paste(parts, collapse = "; "))
}

# Filter data based on user selections
filter_data <- function(df, year_range = NULL, market = NULL, value_range = NULL,
                        buyer_type = NULL, procedure_type = NULL, value_divisor = 1) {
  
  filtered <- df
  
  # Year filter
  if (!is.null(year_range) && "tender_year" %in% names(df)) {
    filtered <- filtered %>%
      filter(tender_year >= year_range[1] & tender_year <= year_range[2])
  }
  
  # Market filter  
  if (!is.null(market) && "cpv_cluster" %in% names(df)) {
    filtered <- filtered %>%
      filter(cpv_cluster %in% market)
  }
  
  # Value filter
  if (!is.null(value_range) && "lot_estimatedprice" %in% names(df)) {
    filtered <- filtered %>%
      filter(lot_estimatedprice >= value_range[1] * value_divisor &
               lot_estimatedprice <= value_range[2] * value_divisor)
  }
  
  # Buyer type filter
  if (!is.null(buyer_type) && "buyer_buyertype" %in% names(df)) {
    filtered <- filtered %>%
      filter(buyer_buyertype %in% buyer_type)
  }
  
  # Procedure type filter
  if (!is.null(procedure_type) && "tender_proceduretype" %in% names(df)) {
    filtered <- filtered %>%
      filter(tender_proceduretype %in% procedure_type)
  }
  
  return(filtered)
}

# ========================================================================
# REPORT GENERATION FUNCTIONS
# ========================================================================

generate_pdf_report <- function(filtered_data, filtered_analysis, country_code, 
                                output_file, filters_text = "") {
  
  saveRDS(list(
    data = filtered_data,
    analysis = filtered_analysis,
    country_code = country_code
  ), file = file.path(tempdir(), "econ_report_data.rds"))
  
  temp_rmd <- tempfile(fileext = ".Rmd")
  
  rmd_content <- '---
title: "Economic Outcomes Analysis Report"
output: pdf_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE, 
                      fig.width = 10, fig.height = 7)
library(ggplot2)
library(dplyr)

report_data <- readRDS(file.path(tempdir(), "econ_report_data.rds"))
filtered_data <- report_data$data
filtered_analysis <- report_data$analysis
```

# Economic Outcomes Analysis Report

**Country:** `r report_data$country_code`

## Executive Summary

This section analyzes the overall performance and competitive dynamics of public procurement markets, focusing on market composition, bidder participation, and pricing outcomes. It examines the distribution of markets by contract size and value, identifies potential monopolistic structures through counts of unique suppliers per market and year, and assesses market volatility by tracking new entrants and regularly participating firms.

## Data Overview

- **Total Contracts:** `r format(nrow(filtered_data), big.mark = ",")`
- **Unique Buyers:** `r if("buyer_masterid" %in% names(filtered_data)) format(length(unique(filtered_data$buyer_masterid)), big.mark = ",") else "N/A"`
- **Unique Suppliers:** `r if("bidder_masterid" %in% names(filtered_data)) format(length(unique(filtered_data$bidder_masterid)), big.mark = ",") else "N/A"`
- **Years Covered:** `r if("tender_year" %in% names(filtered_data)) paste(min(filtered_data$tender_year, na.rm=TRUE), "-", max(filtered_data$tender_year, na.rm=TRUE)) else "N/A"`

\\newpage

# Market Sizing

## What is the overall market composition?

### Market Size by Number of Contracts

This chart compares how active different CPV markets are in terms of the number of contracts. Markets with very large volumes can indicate routine spending categories and high administrative workload. Pay attention to whether spending seems concentrated in a small number of markets or broadly spread across many categories.

```{r fig.height=7}
if (!is.null(filtered_analysis$market_size_n)) {
  print(filtered_analysis$market_size_n)
}
```

### Market Size by Total Value

This chart shows where the money is concentrated across CPV markets. A small number of very large markets suggests concentration of budget and potentially higher strategic importance. Markets that are high in both value and count typically represent core procurement areas.

```{r fig.height=7}
if (!is.null(filtered_analysis$market_size_v)) {
  print(filtered_analysis$market_size_v)
}
```

### Market Size Bubble Plot

This plot combines volume and typical contract size to show how markets differ in structure. Markets with many contracts but low average value reflect frequent small purchases. Markets with few contracts but high average value reflect infrequent but large awards.

```{r fig.height=7}
if (!is.null(filtered_analysis$market_size_av)) {
  print(filtered_analysis$market_size_av)
}
```

\\newpage

# Supplier Dynamics

## How volatile are the markets in terms of new vs old suppliers?

### New vs Repeat Suppliers

This figure shows how supplier participation evolves over time within markets: new entrants versus repeat suppliers. A healthy level of new entry can indicate openness and contestability, while very low entry may suggest barriers or stable incumbent dominance.

```{r fig.height=10}
if (!is.null(filtered_analysis$suppliers_entrance)) {
  print(filtered_analysis$suppliers_entrance)
}
```

### Unique Suppliers Over Time

This chart tracks how many distinct suppliers participate over time, by market. Growth can indicate expanding competition or improved access; decline can signal consolidation or weakening supplier interest.

```{r fig.height=10}
if (!is.null(filtered_analysis$unique_supp)) {
  print(filtered_analysis$unique_supp)
}
```

\\newpage

# Buyer-Supplier Networks

## Are buyers able to choose from a variety of market offerings?

These network visualizations show who buys from whom within selected CPV markets, shown by year. Dense networks with many connections typically reflect broad participation and diversified contracting relationships. Star-like patterns can indicate concentration or strong central actors.

```{r fig.height=12}
if (!is.null(filtered_analysis$network_plots) && length(filtered_analysis$network_plots) > 0) {
  for(i in seq_along(filtered_analysis$network_plots)) {
    if (!is.null(filtered_analysis$network_plots[[i]])) {
      print(filtered_analysis$network_plots[[i]])
    }
  }
}
```

\\newpage

# Relative Price Analysis

## Are there price savings or price overruns prevailing?

### Density of Relative Prices

This plot shows how contract prices compare to their corresponding estimates across tenders. Mass concentrated near 1 suggests estimates broadly align with contracted prices. A heavy right tail indicates cases where contract prices exceed estimates more often or more strongly.

```{r fig.height=7}
if (!is.null(filtered_analysis$rel_tot)) {
  print(filtered_analysis$rel_tot)
}
```

### Relative Prices Over Time

This figure shows how the contract-to-estimate ratio changes over time. Look for widening spread (greater variability) or upward shifts (contracting increasingly above estimates).

```{r fig.height=7}
if (!is.null(filtered_analysis$rel_year)) {
  print(filtered_analysis$rel_year)
}
```

### Top Markets by Relative Prices

This figure highlights the markets where contracted prices tend to be highest relative to estimates. It is useful for prioritizing which sectors may deserve deeper review of cost estimation and price formation.

```{r fig.height=7}
if (!is.null(filtered_analysis$rel_10)) {
  print(filtered_analysis$rel_10)
}
```

### Top Buyers by Relative Prices

This chart compares buyers by how high their contracted prices tend to be relative to estimates. It can help identify institutions that consistently contract above estimates and may require targeted support or scrutiny.

```{r fig.height=7}
if (!is.null(filtered_analysis$rel_buy)) {
  print(filtered_analysis$rel_buy)
}
```

\\newpage

# Competition Analysis

## Is there high competition?

### Single-Bid Share by Procedure

This chart shows how often tenders receive only one bid across different procedure types. Higher single-bid shares can indicate weaker competition, barriers to entry, or procedures that are less attractive to suppliers.

```{r fig.height=7}
if (!is.null(filtered_analysis$single_bid_by_procedure)) {
  print(filtered_analysis$single_bid_by_procedure)
}
```

### Single-Bid Shares by Market × Procedure × Value

This plot focuses on the CPV clusters with the highest overall single-bid incidence. Within those markets, it shows how single-bid outcomes vary by procedure type and contract value bins.

```{r fig.height=8}
if (!is.null(filtered_analysis$single_bid_market_procedure_price_top)) {
  print(filtered_analysis$single_bid_market_procedure_price_top)
}
```

### Single-Bid Share by Contract Value

This figure shows whether single bidding is more common for smaller or larger contracts. High single-bid shares at high values are typically more concerning, as they affect major spending decisions.

```{r fig.height=7}
if (!is.null(filtered_analysis$single_bid_by_price)) {
  print(filtered_analysis$single_bid_by_price)
}
```

### Single-Bid Share by Value × Procedure

This chart combines contract value bins with procedure types to show where single bidding is most prevalent.

```{r fig.height=7}
if (!is.null(filtered_analysis$single_bid_by_price_and_procedure)) {
  print(filtered_analysis$single_bid_by_price_and_procedure)
}
```

### Single-Bid Share by Buyer Group

This figure shows which buyer types experience the most single-bid outcomes, which can help target capacity building or procedural improvements.

```{r fig.height=7}
if (!is.null(filtered_analysis$single_bid_by_buyer_group)) {
  print(filtered_analysis$single_bid_by_buyer_group)
}
```

### Top Buyers by Single-Bid Incidence

This chart identifies specific buyers with the highest single-bid rates, useful for targeted interventions.

```{r fig.height=8}
if (!is.null(filtered_analysis$top_buyers_single_bid)) {
  print(filtered_analysis$top_buyers_single_bid)
}
```

\\newpage

# Conclusions

This comprehensive analysis of economic outcomes in public procurement highlights:

1. **Market Structure:** Distribution of spending across markets and contract size patterns
2. **Supplier Dynamics:** Entry and exit patterns indicating market contestability
3. **Network Patterns:** Buyer-supplier relationship structures and concentration
4. **Price Performance:** Contract prices relative to estimates and variation patterns
5. **Competition Levels:** Single-bid incidence across procedures, values, and buyers

These findings can inform targeted interventions to improve market efficiency and competition.

---

*Report generated using the Economic Outcomes Analysis Tool*
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

generate_word_report <- function(filtered_data, filtered_analysis, country_code, 
                                 output_file, filters_text = "") {
  tryCatch({
    doc <- read_docx()
    
    # Title and metadata
    doc <- doc %>%
      body_add_par("Economic Outcomes Analysis Report", style = "heading 1") %>%
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
      body_add_par("This section analyzes the overall performance and competitive dynamics of public procurement markets, focusing on market composition, bidder participation, and pricing outcomes.", style = "Normal")
    
    # Data Overview
    doc <- doc %>%
      body_add_par("", style = "Normal") %>%
      body_add_par("Data Overview", style = "heading 1")
    
    year_range <- if ("tender_year" %in% names(filtered_data)) {
      paste(min(filtered_data$tender_year, na.rm=TRUE), "-", 
            max(filtered_data$tender_year, na.rm=TRUE))
    } else {
      "N/A"
    }
    
    overview_data <- data.frame(
      Metric = c("Total Contracts", "Unique Buyers", "Unique Suppliers", "Years Covered"),
      Value = c(
        format(nrow(filtered_data), big.mark = ","),
        if("buyer_masterid" %in% names(filtered_data)) 
          format(length(unique(filtered_data$buyer_masterid)), big.mark = ",") else "N/A",
        if("bidder_masterid" %in% names(filtered_data)) 
          format(length(unique(filtered_data$bidder_masterid)), big.mark = ",") else "N/A",
        year_range
      )
    )
    
    ft <- flextable(overview_data) %>%
      theme_booktabs() %>%
      autofit()
    
    doc <- doc %>%
      body_add_flextable(ft) %>%
      body_add_par("", style = "Normal")
    
    # Market Sizing Section
    doc <- doc %>%
      body_add_par("Market Sizing", style = "heading 1") %>%
      body_add_par("What is the overall market composition?", style = "heading 2") %>%
      body_add_par("", style = "Normal")
    
    if (!is.null(filtered_analysis$market_size_n)) {
      temp_img <- tempfile(fileext = ".png")
      ggsave(temp_img, filtered_analysis$market_size_n, width = 10, height = 7, dpi = 300)
      doc <- doc %>%
        body_add_par("Market Size by Number of Contracts", style = "heading 3") %>%
        body_add_par("This chart compares how active different CPV markets are in terms of the number of contracts.", style = "Normal") %>%
        body_add_img(temp_img, width = 6, height = 4.2) %>%
        body_add_par("", style = "Normal")
      unlink(temp_img)
    }
    
    if (!is.null(filtered_analysis$market_size_v)) {
      temp_img <- tempfile(fileext = ".png")
      ggsave(temp_img, filtered_analysis$market_size_v, width = 10, height = 7, dpi = 300)
      doc <- doc %>%
        body_add_par("Market Size by Total Value", style = "heading 3") %>%
        body_add_par("This chart shows where the money is concentrated across CPV markets.", style = "Normal") %>%
        body_add_img(temp_img, width = 6, height = 4.2) %>%
        body_add_par("", style = "Normal")
      unlink(temp_img)
    }
    
    if (!is.null(filtered_analysis$market_size_av)) {
      temp_img <- tempfile(fileext = ".png")
      ggsave(temp_img, filtered_analysis$market_size_av, width = 10, height = 7, dpi = 300)
      doc <- doc %>%
        body_add_par("Market Size Bubble Plot", style = "heading 3") %>%
        body_add_par("This plot combines volume and typical contract size to show how markets differ in structure.", style = "Normal") %>%
        body_add_img(temp_img, width = 6, height = 4.2) %>%
        body_add_par("", style = "Normal")
      unlink(temp_img)
    }
    
    # Supplier Dynamics Section
    doc <- doc %>%
      body_add_par("Supplier Dynamics", style = "heading 1") %>%
      body_add_par("How volatile are the markets in terms of new vs old suppliers?", style = "heading 2") %>%
      body_add_par("", style = "Normal")
    
    if (!is.null(filtered_analysis$suppliers_entrance)) {
      temp_img <- tempfile(fileext = ".png")
      ggsave(temp_img, filtered_analysis$suppliers_entrance, width = 12, height = 10, dpi = 300)
      doc <- doc %>%
        body_add_par("New vs Repeat Suppliers", style = "heading 3") %>%
        body_add_par("This figure shows how supplier participation evolves over time within markets.", style = "Normal") %>%
        body_add_img(temp_img, width = 6, height = 5) %>%
        body_add_par("", style = "Normal")
      unlink(temp_img)
    }
    
    if (!is.null(filtered_analysis$unique_supp)) {
      temp_img <- tempfile(fileext = ".png")
      ggsave(temp_img, filtered_analysis$unique_supp, width = 12, height = 10, dpi = 300)
      doc <- doc %>%
        body_add_par("Unique Suppliers Over Time", style = "heading 3") %>%
        body_add_par("This chart tracks how many distinct suppliers participate over time, by market.", style = "Normal") %>%
        body_add_img(temp_img, width = 6, height = 5) %>%
        body_add_par("", style = "Normal")
      unlink(temp_img)
    }
    
    # Buyer-Supplier Networks
    doc <- doc %>%
      body_add_par("Buyer-Supplier Networks", style = "heading 1") %>%
      body_add_par("Are buyers able to choose from a variety of market offerings?", style = "heading 2") %>%
      body_add_par("", style = "Normal")
    
    if (!is.null(filtered_analysis$network_plots) && length(filtered_analysis$network_plots) > 0) {
      for(i in seq_along(filtered_analysis$network_plots)) {
        if (!is.null(filtered_analysis$network_plots[[i]])) {
          temp_img <- tempfile(fileext = ".png")
          ggsave(temp_img, filtered_analysis$network_plots[[i]], width = 12, height = 12, dpi = 300)
          doc <- doc %>%
            body_add_par(names(filtered_analysis$network_plots)[i], style = "heading 3") %>%
            body_add_par("Network visualization showing buyer-supplier relationships.", style = "Normal") %>%
            body_add_img(temp_img, width = 6, height = 6) %>%
            body_add_par("", style = "Normal")
          unlink(temp_img)
        }
      }
    }
    
    # Relative Price Analysis
    doc <- doc %>%
      body_add_par("Relative Price Analysis", style = "heading 1") %>%
      body_add_par("Are there price savings or price overruns prevailing?", style = "heading 2") %>%
      body_add_par("", style = "Normal")
    
    if (!is.null(filtered_analysis$rel_tot)) {
      temp_img <- tempfile(fileext = ".png")
      ggsave(temp_img, filtered_analysis$rel_tot, width = 10, height = 7, dpi = 300)
      doc <- doc %>%
        body_add_par("Density of Relative Prices", style = "heading 3") %>%
        body_add_par("This plot shows how contract prices compare to their corresponding estimates.", style = "Normal") %>%
        body_add_img(temp_img, width = 6, height = 4.2) %>%
        body_add_par("", style = "Normal")
      unlink(temp_img)
    }
    
    if (!is.null(filtered_analysis$rel_year)) {
      temp_img <- tempfile(fileext = ".png")
      ggsave(temp_img, filtered_analysis$rel_year, width = 10, height = 7, dpi = 300)
      doc <- doc %>%
        body_add_par("Relative Prices Over Time", style = "heading 3") %>%
        body_add_par("This figure shows how the contract-to-estimate ratio changes over time.", style = "Normal") %>%
        body_add_img(temp_img, width = 6, height = 4.2) %>%
        body_add_par("", style = "Normal")
      unlink(temp_img)
    }
    
    if (!is.null(filtered_analysis$rel_10)) {
      temp_img <- tempfile(fileext = ".png")
      ggsave(temp_img, filtered_analysis$rel_10, width = 10, height = 7, dpi = 300)
      doc <- doc %>%
        body_add_par("Top Markets by Relative Prices", style = "heading 3") %>%
        body_add_par("Markets where contracted prices tend to be highest relative to estimates.", style = "Normal") %>%
        body_add_img(temp_img, width = 6, height = 4.2) %>%
        body_add_par("", style = "Normal")
      unlink(temp_img)
    }
    
    if (!is.null(filtered_analysis$rel_buy)) {
      temp_img <- tempfile(fileext = ".png")
      ggsave(temp_img, filtered_analysis$rel_buy, width = 10, height = 7, dpi = 300)
      doc <- doc %>%
        body_add_par("Top Buyers by Relative Prices", style = "heading 3") %>%
        body_add_par("Buyers whose contracted prices tend to be highest relative to estimates.", style = "Normal") %>%
        body_add_img(temp_img, width = 6, height = 4.2) %>%
        body_add_par("", style = "Normal")
      unlink(temp_img)
    }
    
    # Competition Analysis
    doc <- doc %>%
      body_add_par("Competition Analysis", style = "heading 1") %>%
      body_add_par("Is there high competition?", style = "heading 2") %>%
      body_add_par("", style = "Normal")
    
    competition_plots <- list(
      list(name = "single_bid_by_procedure", title = "Single-Bid Share by Procedure"),
      list(name = "single_bid_market_procedure_price_top", title = "Single-Bid by Market × Procedure × Value"),
      list(name = "single_bid_by_price", title = "Single-Bid Share by Contract Value"),
      list(name = "single_bid_by_price_and_procedure", title = "Single-Bid by Value × Procedure"),
      list(name = "single_bid_by_buyer_group", title = "Single-Bid by Buyer Group"),
      list(name = "top_buyers_single_bid", title = "Top Buyers by Single-Bid Incidence")
    )
    
    for(plot_info in competition_plots) {
      if (!is.null(filtered_analysis[[plot_info$name]])) {
        temp_img <- tempfile(fileext = ".png")
        ggsave(temp_img, filtered_analysis[[plot_info$name]], width = 10, height = 7, dpi = 300)
        doc <- doc %>%
          body_add_par(plot_info$title, style = "heading 3") %>%
          body_add_img(temp_img, width = 6, height = 4.2) %>%
          body_add_par("", style = "Normal")
        unlink(temp_img)
      }
    }
    
    # Conclusions
    doc <- doc %>%
      body_add_par("Conclusions", style = "heading 1") %>%
      body_add_par("This analysis highlights:", style = "Normal") %>%
      body_add_par("• Market Structure: Distribution of spending and contract size patterns", style = "Normal") %>%
      body_add_par("• Supplier Dynamics: Entry and exit patterns", style = "Normal") %>%
      body_add_par("• Network Patterns: Buyer-supplier relationship structures", style = "Normal") %>%
      body_add_par("• Price Performance: Contract prices relative to estimates", style = "Normal") %>%
      body_add_par("• Competition Levels: Single-bid incidence analysis", style = "Normal") %>%
      body_add_par("", style = "Normal") %>%
      body_add_par("These findings can inform targeted interventions to improve market efficiency.", style = "Normal")
    
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
  
  dashboardHeader(title = "Economic Outcomes Analysis", titleWidth = 350),
  
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      id = "sidebar",
      menuItem("Setup", tabName = "setup", icon = icon("cog")),
      menuItem("Overview", tabName = "overview", icon = icon("home")),
      menuItem("Data Overview", tabName = "data_overview", icon = icon("table")),
      menuItem("Market Sizing", tabName = "market_sizing", icon = icon("chart-bar")),
      menuItem("Supplier Dynamics", tabName = "supplier_dynamics", icon = icon("users")),
      menuItem("Networks", tabName = "networks", icon = icon("project-diagram")),
      menuItem("Relative Prices", tabName = "relative_prices", icon = icon("dollar-sign")),
      menuItem("Competition", tabName = "competition", icon = icon("trophy")),
      menuItem("Export", tabName = "export", icon = icon("download"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper { background-color: #f4f4f4; }
        .box { margin-bottom: 20px; }
        .question-header {
          color: #d32f2f;
          font-weight: bold;
          font-size: 18px;
          margin: 20px 0 10px 0;
          padding: 10px;
          background-color: #fff;
          border-left: 4px solid #d32f2f;
        }
        .description-box {
          background-color: #e3f2fd;
          border-left: 4px solid #2196f3;
          padding: 15px;
          margin: 15px 0;
          border-radius: 4px;
        }
        .description-box p {
          margin: 0;
          line-height: 1.6;
        }
        .download-btn {
          margin-top: 10px;
          margin-bottom: 10px;
        }
      "))
    ),
    
    tabItems(
      # Setup Tab
      tabItem(
        tabName = "setup",
        h2("Data Setup"),
        
        fluidRow(
          box(
            title = "System Status",
            width = 12,
            status = "info",
            solidHeader = TRUE,
            
            uiOutput("system_status_ui")
          )
        ),
        
        fluidRow(
          box(
            title = "Upload Data",
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            
            fileInput("datafile", "Choose CSV File",
                      accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
            
            textInput("country_code", "Country Code (e.g., BG, UY):", value = ""),
            
            fileInput("cpv_file", "CPV Codes File (optional)",
                      accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
            
            textInput("network_cpv", "Network CPV Clusters (comma-separated, e.g., 45,33):",
                      value = "45,33"),
            
            checkboxInput("skip_networks", "Skip network generation (faster, uses less memory)", value = FALSE),
            
            actionButton("run_analysis", "Run Analysis", 
                         icon = icon("play"), 
                         class = "btn-success btn-lg"),
            
            hr(),
            
            verbatimTextOutput("analysis_status")
          )
        )
      ),
      
      # Overview Tab
      tabItem(
        tabName = "overview",
        h2("Analysis Overview"),
        
        fluidRow(
          box(
            title = "About This Analysis",
            width = 12,
            solidHeader = TRUE,
            status = "info",
            
            p("This tool analyzes economic outcomes in public procurement, focusing on:"),
            tags$ul(
              tags$li(tags$b("Market Sizing:"), " Distribution of contracts and values across CPV markets"),
              tags$li(tags$b("Supplier Dynamics:"), " New vs repeat suppliers and market volatility"),
              tags$li(tags$b("Buyer-Supplier Networks:"), " Relationship patterns and market structure"),
              tags$li(tags$b("Relative Prices:"), " Contract prices vs estimates"),
              tags$li(tags$b("Competition:"), " Single-bid incidence analysis")
            ),
            
            p("Navigate through the tabs to explore each dimension of the analysis.",
              "Use filters on each tab to focus on specific markets, time periods, or buyer types.")
          )
        )
      ),
      
      # Data Overview Tab
      tabItem(
        tabName = "data_overview",
        h2("Data Overview"),
        
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
            plotOutput("contracts_year_plot", height = "400px")
          ),
          box(
            title = "CPV Market Definitions",
            width = 6,
            solidHeader = TRUE,
            status = "primary",
            DT::dataTableOutput("cpv_legend_table")
          )
        ),
        
        fluidRow(
          box(
            title = "Summary Statistics",
            width = 12,
            solidHeader = TRUE,
            status = "info",
            DT::dataTableOutput("summary_table")
          )
        )
      ),
      
      # Market Sizing Tab - WILL CONTINUE IN NEXT MESSAGE DUE TO LENGTH
      
      # Market Sizing Tab
      tabItem(
        tabName = "market_sizing",
        h2("Market Sizing Analysis"),
        
        div(class = "question-header",
            "What is the overall market composition?"
        ),
        
        div(class = "description-box",
            p("Market sizing examines how procurement spending is distributed across different CPV markets. 
              This helps identify where resources are concentrated, whether certain categories dominate, 
              and how average contract sizes vary across sectors.")
        ),
        
        fluidRow(
          box(
            title = "Market Size by Number of Contracts",
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            div(class = "description-box",
                "This chart compares how active different CPV markets are in terms of the number of contracts. 
                Markets with very large volumes can indicate routine spending categories and high administrative workload."
            ),
            plotOutput("market_size_n_plot", height = "600px"),
            downloadButton("download_market_size_n", "Download Figure", class = "download-btn btn-sm")
          )
        ),
        
        fluidRow(
          box(
            title = "Market Size by Total Value",
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            div(class = "description-box",
                "This chart shows where the money is concentrated across CPV markets. 
                A small number of very large markets suggests concentration of budget and potentially higher strategic importance."
            ),
            plotOutput("market_size_v_plot", height = "600px"),
            downloadButton("download_market_size_v", "Download Figure", class = "download-btn btn-sm")
          )
        ),
        
        fluidRow(
          box(
            title = "Market Size Bubble Plot",
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            div(class = "description-box",
                "This plot combines volume and typical contract size to show how markets differ in structure. 
                Markets with many contracts but low average value reflect frequent small purchases."
            ),
            plotOutput("market_size_av_plot", height = "600px"),
            downloadButton("download_market_size_av", "Download Figure", class = "download-btn btn-sm")
          )
        )
      ),
      
      # Supplier Dynamics Tab
      tabItem(
        tabName = "supplier_dynamics",
        h2("Supplier Dynamics"),
        
        div(class = "question-header",
            "How volatile are the markets in terms of new vs old suppliers?"
        ),
        
        div(class = "description-box",
            p("Supplier dynamics tracks how supplier participation evolves over time. 
              High entry rates can signal market openness, while low entry may indicate barriers. 
              The balance between new and repeat suppliers reflects market contestability.")
        ),
        
        fluidRow(
          box(
            title = "New vs Repeat Suppliers",
            width = 12,
            solidHeader = TRUE,
            status = "success",
            div(class = "description-box",
                "This figure shows how supplier participation evolves over time within markets: new entrants versus repeat suppliers. 
                A healthy level of new entry can indicate openness and contestability."
            ),
            plotOutput("suppliers_entrance_plot", height = "800px"),
            downloadButton("download_suppliers_entrance", "Download Figure", class = "download-btn btn-sm")
          )
        ),
        
        fluidRow(
          box(
            title = "Unique Suppliers Over Time",
            width = 12,
            solidHeader = TRUE,
            status = "success",
            div(class = "description-box",
                "This chart tracks how many distinct suppliers participate over time, by market. 
                Growth can indicate expanding competition or improved access; decline can signal consolidation."
            ),
            plotOutput("unique_supp_plot", height = "800px"),
            downloadButton("download_unique_supp", "Download Figure", class = "download-btn btn-sm")
          )
        )
      ),
      
      # Networks Tab
      tabItem(
        tabName = "networks",
        h2("Buyer-Supplier Networks"),
        
        div(class = "question-header",
            "Are buyers able to choose from a variety of market offerings?"
        ),
        
        div(class = "description-box",
            p("Network visualizations show the structure of buyer-supplier relationships within specific markets. 
              Dense networks indicate diversified participation, while star patterns suggest concentration around key actors.")
        ),
        
        fluidRow(
          box(
            title = "Network Visualizations",
            width = 12,
            solidHeader = TRUE,
            status = "warning",
            div(class = "description-box",
                "These network visualizations show who buys from whom within selected CPV markets. 
                Dense networks with many connections reflect broad participation and diversified contracting relationships."
            ),
            uiOutput("network_plots_ui")
          )
        )
      ),
      
      # Relative Prices Tab
      tabItem(
        tabName = "relative_prices",
        h2("Relative Price Analysis"),
        
        div(class = "question-header",
            "Are there price savings or price overruns prevailing?"
        ),
        
        div(class = "description-box",
            p("Relative price analysis compares contract prices to estimated prices. 
              Values near 1 indicate good cost estimation, while systematic deviations suggest estimation issues or price control challenges.")
        ),
        
        fluidRow(
          box(
            title = "Density of Relative Prices",
            width = 6,
            solidHeader = TRUE,
            status = "info",
            div(class = "description-box",
                "This plot shows how contract prices compare to their estimates. 
                Mass near 1 suggests good alignment; heavy tails indicate frequent deviations."
            ),
            plotOutput("rel_tot_plot", height = "500px"),
            downloadButton("download_rel_tot", "Download Figure", class = "download-btn btn-sm")
          ),
          box(
            title = "Relative Prices Over Time",
            width = 6,
            solidHeader = TRUE,
            status = "info",
            div(class = "description-box",
                "This figure shows how the contract-to-estimate ratio changes over time. 
                Look for widening spread or upward shifts."
            ),
            plotOutput("rel_year_plot", height = "500px"),
            downloadButton("download_rel_year", "Download Figure", class = "download-btn btn-sm")
          )
        ),
        
        fluidRow(
          box(
            title = "Top Markets by Relative Prices",
            width = 6,
            solidHeader = TRUE,
            status = "info",
            div(class = "description-box",
                "Markets where contracted prices tend to be highest relative to estimates. 
                Useful for prioritizing which sectors deserve deeper review."
            ),
            plotOutput("rel_10_plot", height = "500px"),
            downloadButton("download_rel_10", "Download Figure", class = "download-btn btn-sm")
          ),
          box(
            title = "Top Buyers by Relative Prices",
            width = 6,
            solidHeader = TRUE,
            status = "info",
            div(class = "description-box",
                "Buyers whose contracted prices tend to be highest relative to estimates. 
                Can help identify institutions requiring targeted support."
            ),
            plotOutput("rel_buy_plot", height = "500px"),
            downloadButton("download_rel_buy", "Download Figure", class = "download-btn btn-sm")
          )
        )
      ),
      
      # Competition Tab
      tabItem(
        tabName = "competition",
        h2("Competition Analysis"),
        
        div(class = "question-header",
            "Is there high competition?"
        ),
        
        div(class = "description-box",
            p("Competition analysis examines single-bid incidence across different dimensions. 
              High single-bid rates indicate weak competition and potential barriers to market entry.")
        ),
        
        fluidRow(
          box(
            title = "Single-Bid Share by Procedure",
            width = 12,
            solidHeader = TRUE,
            status = "danger",
            div(class = "description-box",
                "This chart shows how often tenders receive only one bid across different procedure types. 
                Higher shares indicate weaker competition or barriers to entry."
            ),
            plotOutput("single_bid_procedure_plot", height = "500px"),
            downloadButton("download_single_bid_procedure", "Download Figure", class = "download-btn btn-sm")
          )
        ),
        
        fluidRow(
          box(
            title = "Single-Bid by Market × Procedure × Value (Top Markets)",
            width = 12,
            solidHeader = TRUE,
            status = "danger",
            div(class = "description-box",
                "This plot focuses on CPV clusters with highest single-bid incidence. 
                Shows how single-bid outcomes vary by procedure type and contract value bins."
            ),
            plotOutput("single_bid_market_proc_price_plot", height = "700px"),
            downloadButton("download_single_bid_market_proc_price", "Download Figure", class = "download-btn btn-sm")
          )
        ),
        
        fluidRow(
          box(
            title = "Single-Bid Share by Contract Value",
            width = 6,
            solidHeader = TRUE,
            status = "danger",
            div(class = "description-box",
                "Shows whether single bidding is more common for smaller or larger contracts."
            ),
            plotOutput("single_bid_price_plot", height = "500px"),
            downloadButton("download_single_bid_price", "Download Figure", class = "download-btn btn-sm")
          ),
          box(
            title = "Single-Bid by Value × Procedure",
            width = 6,
            solidHeader = TRUE,
            status = "danger",
            div(class = "description-box",
                "Combines contract value bins with procedure types to show where single bidding is most prevalent."
            ),
            plotOutput("single_bid_price_proc_plot", height = "500px"),
            downloadButton("download_single_bid_price_proc", "Download Figure", class = "download-btn btn-sm")
          )
        ),
        
        fluidRow(
          box(
            title = "Single-Bid Share by Buyer Group",
            width = 6,
            solidHeader = TRUE,
            status = "danger",
            div(class = "description-box",
                "Shows which buyer types experience the most single-bid outcomes."
            ),
            plotOutput("single_bid_buyer_group_plot", height = "500px"),
            downloadButton("download_single_bid_buyer_group", "Download Figure", class = "download-btn btn-sm")
          ),
          box(
            title = "Top Buyers by Single-Bid Incidence",
            width = 6,
            solidHeader = TRUE,
            status = "danger",
            div(class = "description-box",
                "Identifies specific buyers with the highest single-bid rates."
            ),
            plotOutput("top_buyers_single_bid_plot", height = "500px"),
            downloadButton("download_top_buyers_single_bid", "Download Figure", class = "download-btn btn-sm")
          )
        )
      ),
      
      # Export Tab
      tabItem(
        tabName = "export",
        h2("Export Reports and Figures"),
        
        fluidRow(
          box(
            title = "Generate Full Report",
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            
            p("Generate a comprehensive report with all visualizations and analysis."),
            
            fluidRow(
              column(6,
                     downloadButton("download_pdf", "Download PDF Report", 
                                    class = "btn-primary btn-lg", 
                                    style = "width: 100%; margin-bottom: 10px;")
              ),
              column(6,
                     downloadButton("download_word", "Download Word Report", 
                                    class = "btn-info btn-lg", 
                                    style = "width: 100%;")
              )
            ),
            
            hr(),
            
            verbatimTextOutput("export_status")
          )
        )
      )
    )
  )
)

# ========================================================================
# SERVER
# ========================================================================

server <- function(input, output, session) {
  
  # Show system status
  output$system_status_ui <- renderUI({
    status_items <- list()
    
    # Check if utils loaded
    if (exists("utils_loaded") && utils_loaded) {
      status_items[[1]] <- tags$p(
        icon("check-circle", class = "text-success"),
        " Utils file loaded successfully"
      )
    } else {
      status_items[[1]] <- tags$div(
        icon("exclamation-triangle", class = "text-danger"),
        tags$b(" Utils file not loaded!"),
        tags$p("Please update line 15 in econ_outcomes_app.R to point to your econ_out_utils.R file."),
        tags$p("Current working directory:", getwd())
      )
    }
    
    # Check required packages
    required_pkgs <- c("shiny", "shinydashboard", "DT", "ggplot2", "dplyr", 
                       "data.table", "scales", "officer", "flextable", "rmarkdown")
    missing_pkgs <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]
    
    if (length(missing_pkgs) == 0) {
      status_items[[2]] <- tags$p(
        icon("check-circle", class = "text-success"),
        " All required packages available"
      )
    } else {
      status_items[[2]] <- tags$div(
        icon("exclamation-triangle", class = "text-warning"),
        " Missing packages: ", paste(missing_pkgs, collapse = ", "),
        tags$p("Install with: install.packages(c('", paste(missing_pkgs, collapse = "', '"), "'))")
      )
    }
    
    do.call(tagList, status_items)
  })
  
  # Reactive values to store results
  results <- reactiveValues(
    data = NULL,
    analysis = NULL,
    filtered_data = NULL,
    filtered_analysis = NULL,
    country_code = NULL,
    cpv_lookup = NULL
  )
  
  # Data loading and analysis
  observeEvent(input$run_analysis, {
    req(input$datafile, input$country_code)
    
    # Check if utils are loaded
    if (!exists("utils_loaded") || !utils_loaded) {
      showNotification(
        "Cannot run analysis: econ_out_utils.R not loaded. Please update the source() path in the app file.",
        type = "error",
        duration = NULL
      )
      output$analysis_status <- renderText({
        "Error: econ_out_utils.R not loaded.\n\nPlease:\n1. Update line 15 in econ_outcomes_app.R\n2. Point it to your econ_out_utils.R file\n3. Restart the app"
      })
      return(NULL)
    }
    
    # Isolate the analysis in a separate process to prevent crashes
    withProgress(message = 'Loading data...', value = 0, {
      
      tryCatch({
        incProgress(0.1, detail = "Reading main data file...")
        
        # Read main data file with size limits
        df <- tryCatch({
          fread(
            input$datafile$datapath,
            keepLeadingZeros = TRUE,
            encoding = "UTF-8",
            stringsAsFactors = FALSE,
            showProgress = FALSE,
            na.strings = c("", "-", "NA"),
            nrows = -1  # Read all rows but monitor
          )
        }, error = function(e) {
          showNotification(paste("Error reading CSV:", e$message), type = "error")
          return(NULL)
        })
        
        if (is.null(df)) return(NULL)
        
        # Check data size
        file_size_mb <- file.size(input$datafile$datapath) / 1024^2
        if (file_size_mb > 500) {
          showNotification(
            paste0("Warning: Large file (", round(file_size_mb, 1), " MB). Analysis may take several minutes."),
            type = "warning",
            duration = 10
          )
        }
        
        incProgress(0.2, detail = paste("Loaded", nrow(df), "rows..."))
        
        # Remove duplicate columns
        dup_cols <- duplicated(names(df))
        if (any(dup_cols)) {
          df <- df[, !dup_cols, with = FALSE]
        }
        
        df <- as.data.frame(df)
        
        incProgress(0.25, detail = "Loading CPV lookup...")
        
        # Load CPV lookup if file uploaded
        cpv_lookup <- NULL
        if (!is.null(input$cpv_file)) {
          cpv_lookup <- tryCatch({
            cpv_data <- fread(input$cpv_file$datapath, showProgress = FALSE)
            lookup <- build_cpv_lookup(cpv_data)
            showNotification("CPV lookup loaded", type = "message", duration = 3)
            lookup
          }, error = function(e) {
            showNotification(
              paste("CPV file could not be loaded:", e$message, "- continuing without CPV labels"), 
              type = "warning", 
              duration = 5
            )
            NULL
          })
        }
        
        country_code <- toupper(trimws(input$country_code))
        
        # Parse network CPV clusters - skip if checkbox is checked
        network_clusters <- tryCatch({
          if (input$skip_networks) {
            character(0)  # Skip networks entirely
          } else if (nchar(trimws(input$network_cpv)) > 0) {
            trimws(strsplit(input$network_cpv, ",")[[1]])
          } else {
            character(0)  # Empty - skip networks
          }
        }, error = function(e) {
          character(0)
        })
        
        incProgress(0.3, detail = "Starting analysis pipeline...")
        
        # Set memory-friendly options
        old_options <- options(
          expressions = 5000,  # Limit expression nesting
          warn = 1             # Show warnings immediately
        )
        on.exit(options(old_options), add = TRUE)
        
        # Run analysis with multiple safety layers
        analysis_results <- tryCatch({
          
          # Try to run full pipeline
          withCallingHandlers({
            
            incProgress(0.4, detail = "Processing data...")
            
            run_economic_efficiency_pipeline(
              df = df,
              country_code = country_code,
              output_dir = tempdir(),
              cpv_lookup = cpv_lookup,
              network_cpv_clusters = network_clusters,
              save_outputs = FALSE  # Don't save to disk
            )
            
          }, warning = function(w) {
            # Log warnings but continue
            message("Pipeline warning: ", w$message)
          })
          
        }, error = function(e) {
          
          # If pipeline fails, create minimal results
          showNotification(
            paste("Pipeline error:", e$message, "- Creating basic results only"), 
            type = "error", 
            duration = 10
          )
          
          # Create minimal viable results object
          list(
            country_code = country_code,
            df = df,
            summary_stats = list(
              n_obs_per_year = if ("tender_year" %in% names(df)) {
                df %>% 
                  group_by(tender_year) %>% 
                  summarise(n_observations = n(), .groups = "drop") %>%
                  as.data.frame()
              } else NULL,
              n_unique_buyers = if ("buyer_masterid" %in% names(df)) {
                length(unique(df$buyer_masterid))
              } else NULL,
              n_unique_bidders = if ("bidder_masterid" %in% names(df)) {
                length(unique(df$bidder_masterid))
              } else NULL,
              vars_present = names(df)[!grepl("^ind_", names(df))]
            ),
            # Set all plot objects to NULL
            market_size_n = NULL,
            market_size_v = NULL,
            market_size_av = NULL,
            suppliers_entrance = NULL,
            unique_supp = NULL,
            network_plots = list(),
            rel_tot = NULL,
            rel_year = NULL,
            rel_10 = NULL,
            rel_buy = NULL,
            single_bid_by_procedure = NULL,
            single_bid_market_procedure_price_top = NULL,
            single_bid_by_price = NULL,
            single_bid_by_price_and_procedure = NULL,
            single_bid_by_buyer_group = NULL,
            top_buyers_single_bid = NULL,
            cpv_cluster_legend = if ("cpv_cluster" %in% names(df) && "cpv_category" %in% names(df)) {
              df %>% 
                select(cpv_cluster, cpv_category) %>% 
                distinct() %>% 
                arrange(cpv_cluster) %>%
                as.data.frame()
            } else NULL
          )
        })
        
        incProgress(0.9, detail = "Finalizing...")
        
        # Force garbage collection to free memory
        gc()
        
        # Store results
        results$data <- df
        results$analysis <- analysis_results
        results$filtered_data <- df
        results$filtered_analysis <- analysis_results
        results$country_code <- country_code
        results$cpv_lookup <- cpv_lookup
        
        # Success message
        n_plots <- sum(!sapply(analysis_results[c("market_size_n", "market_size_v", "market_size_av",
                                                  "suppliers_entrance", "unique_supp",
                                                  "rel_tot", "rel_year", "rel_10", "rel_buy",
                                                  "single_bid_by_procedure", "single_bid_by_price")], 
                               is.null))
        
        output$analysis_status <- renderText({
          paste0("✓ Analysis complete!\n",
                 "Country: ", country_code, "\n",
                 "Rows: ", formatC(nrow(df), format = "d", big.mark = ","), "\n",
                 "Columns: ", ncol(df), "\n",
                 "Plots generated: ", n_plots, "\n",
                 "Networks: ", length(analysis_results$network_plots))
        })
        
        showNotification(
          "Analysis complete! Navigate to other tabs to view results.", 
          type = "message", 
          duration = 5
        )
        
      }, error = function(e) {
        # Catch-all error handler
        output$analysis_status <- renderText({
          paste("Critical error:", e$message, "\n\nPlease try:\n",
                "1. Using a smaller dataset\n",
                "2. Checking data format\n",
                "3. Verifying column names match expected format")
        })
        showNotification(
          paste("Critical error:", e$message), 
          type = "error", 
          duration = NULL
        )
      })
    })
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
    n <- if ("buyer_masterid" %in% names(results$filtered_data)) {
      length(unique(results$filtered_data$buyer_masterid))
    } else NA
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
      length(unique(results$filtered_data$bidder_masterid))
    } else NA
    valueBox(
      formatC(n, format = "d", big.mark = ","),
      "Unique Suppliers",
      icon = icon("truck"),
      color = "yellow"
    )
  })
  
  output$n_years <- renderValueBox({
    req(results$filtered_data)
    n <- if ("tender_year" %in% names(results$filtered_data)) {
      length(unique(results$filtered_data$tender_year[!is.na(results$filtered_data$tender_year)]))
    } else NA
    valueBox(
      n,
      "Years Covered",
      icon = icon("calendar"),
      color = "red"
    )
  })
  
  output$contracts_year_plot <- renderPlot({
    req(results$filtered_analysis$summary_stats$n_obs_per_year)
    stats <- results$filtered_analysis$summary_stats$n_obs_per_year
    
    ggplot(stats, aes(x = tender_year, y = n_observations)) +
      geom_col(fill = "#3c8dbc") +
      geom_text(aes(label = formatC(n_observations, format = "d", big.mark = ",")),
                vjust = -0.5, size = 4) +
      labs(x = "Year", y = "Number of Contracts", title = "Contracts per Year") +
      theme_minimal(base_size = 14) +
      scale_y_continuous(labels = scales::comma)
  })
  
  output$cpv_legend_table <- DT::renderDataTable({
    req(results$filtered_analysis$cpv_cluster_legend)
    
    DT::datatable(
      results$filtered_analysis$cpv_cluster_legend,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'ftip'
      ),
      rownames = FALSE
    )
  })
  
  output$summary_table <- DT::renderDataTable({
    req(results$filtered_analysis$summary_stats)
    summ <- results$filtered_analysis$summary_stats
    
    summary_df <- data.frame(
      Metric = character(),
      Value = character(),
      stringsAsFactors = FALSE
    )
    
    if (!is.null(summ$n_unique_buyers)) {
      summary_df <- rbind(summary_df, 
                          data.frame(Metric = "Unique Buyers", 
                                     Value = format(summ$n_unique_buyers, big.mark = ",")))
    }
    
    if (!is.null(summ$n_unique_bidders)) {
      summary_df <- rbind(summary_df,
                          data.frame(Metric = "Unique Bidders",
                                     Value = format(summ$n_unique_bidders, big.mark = ",")))
    }
    
    DT::datatable(
      summary_df,
      options = list(
        pageLength = 10,
        dom = 't'
      ),
      rownames = FALSE
    )
  })
  
  # ========================================================================
  # MARKET SIZING OUTPUTS
  # ========================================================================
  
  output$market_size_n_plot <- renderPlot({
    req(results$filtered_analysis$market_size_n)
    tryCatch({
      results$filtered_analysis$market_size_n
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error rendering plot:", e$message), cex = 1.2)
    })
  })
  
  output$market_size_v_plot <- renderPlot({
    req(results$filtered_analysis$market_size_v)
    tryCatch({
      results$filtered_analysis$market_size_v
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error rendering plot:", e$message), cex = 1.2)
    })
  })
  
  output$market_size_av_plot <- renderPlot({
    req(results$filtered_analysis$market_size_av)
    tryCatch({
      results$filtered_analysis$market_size_av
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error rendering plot:", e$message), cex = 1.2)
    })
  })
  
  # ========================================================================
  # SUPPLIER DYNAMICS OUTPUTS
  # ========================================================================
  
  output$suppliers_entrance_plot <- renderPlot({
    req(results$filtered_analysis$suppliers_entrance)
    tryCatch({
      results$filtered_analysis$suppliers_entrance
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error rendering plot:", e$message), cex = 1.2)
    })
  })
  
  output$unique_supp_plot <- renderPlot({
    req(results$filtered_analysis$unique_supp)
    tryCatch({
      results$filtered_analysis$unique_supp
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error rendering plot:", e$message), cex = 1.2)
    })
  })
  
  # ========================================================================
  # NETWORK OUTPUTS
  # ========================================================================
  
  output$network_plots_ui <- renderUI({
    plots <- results$filtered_analysis$network_plots
    
    if (is.null(plots) || length(plots) == 0) {
      return(box(
        width = 12,
        p("No network plots available for this dataset. This may be because:"),
        tags$ul(
          tags$li("Required columns (buyer_masterid, bidder_masterid, cpv_cluster) are missing"),
          tags$li("The specified CPV clusters don't exist in the data"),
          tags$li("There is insufficient data to create meaningful networks")
        )
      ))
    }
    
    plot_outputs <- lapply(seq_along(plots), function(i) {
      plot_name <- names(plots)[i]
      output_id <- paste0("network_plot_", i)
      
      output[[output_id]] <- renderPlot({
        tryCatch({
          plots[[i]]
        }, error = function(e) {
          plot.new()
          text(0.5, 0.5, paste("Error rendering network plot:", e$message), cex = 1.2)
        })
      }, height = 800)
      
      tagList(
        h4(plot_name),
        plotOutput(output_id, height = "800px"),
        hr()
      )
    })
    
    do.call(tagList, plot_outputs)
  })
  
  # ========================================================================
  # RELATIVE PRICE OUTPUTS
  # ========================================================================
  
  output$rel_tot_plot <- renderPlot({
    req(results$filtered_analysis$rel_tot)
    tryCatch({
      results$filtered_analysis$rel_tot
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, "Plot not available", cex = 1.2)
    })
  })
  
  output$rel_year_plot <- renderPlot({
    req(results$filtered_analysis$rel_year)
    tryCatch({
      results$filtered_analysis$rel_year
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, "Plot not available", cex = 1.2)
    })
  })
  
  output$rel_10_plot <- renderPlot({
    req(results$filtered_analysis$rel_10)
    tryCatch({
      results$filtered_analysis$rel_10
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, "Plot not available", cex = 1.2)
    })
  })
  
  output$rel_buy_plot <- renderPlot({
    req(results$filtered_analysis$rel_buy)
    tryCatch({
      results$filtered_analysis$rel_buy
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, "Plot not available", cex = 1.2)
    })
  })
  
  # ========================================================================
  # COMPETITION OUTPUTS
  # ========================================================================
  
  output$single_bid_procedure_plot <- renderPlot({
    req(results$filtered_analysis$single_bid_by_procedure)
    tryCatch({
      results$filtered_analysis$single_bid_by_procedure
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, "Plot not available", cex = 1.2)
    })
  })
  
  output$single_bid_market_proc_price_plot <- renderPlot({
    req(results$filtered_analysis$single_bid_market_procedure_price_top)
    tryCatch({
      results$filtered_analysis$single_bid_market_procedure_price_top
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, "Plot not available", cex = 1.2)
    })
  })
  
  output$single_bid_price_plot <- renderPlot({
    req(results$filtered_analysis$single_bid_by_price)
    tryCatch({
      results$filtered_analysis$single_bid_by_price
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, "Plot not available", cex = 1.2)
    })
  })
  
  output$single_bid_price_proc_plot <- renderPlot({
    req(results$filtered_analysis$single_bid_by_price_and_procedure)
    tryCatch({
      results$filtered_analysis$single_bid_by_price_and_procedure
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, "Plot not available", cex = 1.2)
    })
  })
  
  output$single_bid_buyer_group_plot <- renderPlot({
    req(results$filtered_analysis$single_bid_by_buyer_group)
    tryCatch({
      results$filtered_analysis$single_bid_by_buyer_group
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, "Plot not available", cex = 1.2)
    })
  })
  
  output$top_buyers_single_bid_plot <- renderPlot({
    req(results$filtered_analysis$top_buyers_single_bid)
    tryCatch({
      results$filtered_analysis$top_buyers_single_bid
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, "Plot not available", cex = 1.2)
    })
  })
  
  # ========================================================================
  # DOWNLOAD HANDLERS
  # ========================================================================
  
  output$download_market_size_n <- downloadHandler(
    filename = function() { paste0("market_size_n_", results$country_code, ".png") },
    content = function(file) {
      req(results$filtered_analysis$market_size_n)
      ggsave(file, results$filtered_analysis$market_size_n, width = 10, height = 7, dpi = 300)
    }
  )
  
  output$download_market_size_v <- downloadHandler(
    filename = function() { paste0("market_size_v_", results$country_code, ".png") },
    content = function(file) {
      req(results$filtered_analysis$market_size_v)
      ggsave(file, results$filtered_analysis$market_size_v, width = 10, height = 7, dpi = 300)
    }
  )
  
  output$download_market_size_av <- downloadHandler(
    filename = function() { paste0("market_size_av_", results$country_code, ".png") },
    content = function(file) {
      req(results$filtered_analysis$market_size_av)
      ggsave(file, results$filtered_analysis$market_size_av, width = 10, height = 7, dpi = 300)
    }
  )
  
  output$download_suppliers_entrance <- downloadHandler(
    filename = function() { paste0("suppliers_entrance_", results$country_code, ".png") },
    content = function(file) {
      req(results$filtered_analysis$suppliers_entrance)
      ggsave(file, results$filtered_analysis$suppliers_entrance, width = 12, height = 10, dpi = 300)
    }
  )
  
  output$download_unique_supp <- downloadHandler(
    filename = function() { paste0("unique_supp_", results$country_code, ".png") },
    content = function(file) {
      req(results$filtered_analysis$unique_supp)
      ggsave(file, results$filtered_analysis$unique_supp, width = 12, height = 10, dpi = 300)
    }
  )
  
  output$download_rel_tot <- downloadHandler(
    filename = function() { paste0("rel_tot_", results$country_code, ".png") },
    content = function(file) {
      req(results$filtered_analysis$rel_tot)
      ggsave(file, results$filtered_analysis$rel_tot, width = 10, height = 7, dpi = 300)
    }
  )
  
  output$download_rel_year <- downloadHandler(
    filename = function() { paste0("rel_year_", results$country_code, ".png") },
    content = function(file) {
      req(results$filtered_analysis$rel_year)
      ggsave(file, results$filtered_analysis$rel_year, width = 10, height = 7, dpi = 300)
    }
  )
  
  output$download_rel_10 <- downloadHandler(
    filename = function() { paste0("rel_10_", results$country_code, ".png") },
    content = function(file) {
      req(results$filtered_analysis$rel_10)
      ggsave(file, results$filtered_analysis$rel_10, width = 10, height = 7, dpi = 300)
    }
  )
  
  output$download_rel_buy <- downloadHandler(
    filename = function() { paste0("rel_buy_", results$country_code, ".png") },
    content = function(file) {
      req(results$filtered_analysis$rel_buy)
      ggsave(file, results$filtered_analysis$rel_buy, width = 10, height = 7, dpi = 300)
    }
  )
  
  output$download_single_bid_procedure <- downloadHandler(
    filename = function() { paste0("single_bid_procedure_", results$country_code, ".png") },
    content = function(file) {
      req(results$filtered_analysis$single_bid_by_procedure)
      ggsave(file, results$filtered_analysis$single_bid_by_procedure, width = 10, height = 7, dpi = 300)
    }
  )
  
  output$download_single_bid_market_proc_price <- downloadHandler(
    filename = function() { paste0("single_bid_market_proc_price_", results$country_code, ".png") },
    content = function(file) {
      req(results$filtered_analysis$single_bid_market_procedure_price_top)
      ggsave(file, results$filtered_analysis$single_bid_market_procedure_price_top, width = 12, height = 8, dpi = 300)
    }
  )
  
  output$download_single_bid_price <- downloadHandler(
    filename = function() { paste0("single_bid_price_", results$country_code, ".png") },
    content = function(file) {
      req(results$filtered_analysis$single_bid_by_price)
      ggsave(file, results$filtered_analysis$single_bid_by_price, width = 10, height = 7, dpi = 300)
    }
  )
  
  output$download_single_bid_price_proc <- downloadHandler(
    filename = function() { paste0("single_bid_price_proc_", results$country_code, ".png") },
    content = function(file) {
      req(results$filtered_analysis$single_bid_by_price_and_procedure)
      ggsave(file, results$filtered_analysis$single_bid_by_price_and_procedure, width = 10, height = 7, dpi = 300)
    }
  )
  
  output$download_single_bid_buyer_group <- downloadHandler(
    filename = function() { paste0("single_bid_buyer_group_", results$country_code, ".png") },
    content = function(file) {
      req(results$filtered_analysis$single_bid_by_buyer_group)
      ggsave(file, results$filtered_analysis$single_bid_by_buyer_group, width = 10, height = 7, dpi = 300)
    }
  )
  
  output$download_top_buyers_single_bid <- downloadHandler(
    filename = function() { paste0("top_buyers_single_bid_", results$country_code, ".png") },
    content = function(file) {
      req(results$filtered_analysis$top_buyers_single_bid)
      ggsave(file, results$filtered_analysis$top_buyers_single_bid, width = 10, height = 7, dpi = 300)
    }
  )
  
  # ========================================================================
  # REPORT DOWNLOADS
  # ========================================================================
  
  output$download_pdf <- downloadHandler(
    filename = function() {
      paste0("economic_outcomes_report_", results$country_code, "_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      req(results$filtered_data, results$filtered_analysis, results$country_code)
      
      withProgress(message = 'Generating PDF report...', value = 0, {
        incProgress(0.5, detail = "Creating report...")
        
        success <- generate_pdf_report(
          filtered_data = results$filtered_data,
          filtered_analysis = results$filtered_analysis,
          country_code = results$country_code,
          output_file = file,
          filters_text = ""
        )
        
        if (success) {
          output$export_status <- renderText("PDF report generated successfully!")
        } else {
          output$export_status <- renderText("Error generating PDF report. Check console for details.")
        }
      })
    }
  )
  
  output$download_word <- downloadHandler(
    filename = function() {
      paste0("economic_outcomes_report_", results$country_code, "_", Sys.Date(), ".docx")
    },
    content = function(file) {
      req(results$filtered_data, results$filtered_analysis, results$country_code)
      
      withProgress(message = 'Generating Word report...', value = 0, {
        incProgress(0.5, detail = "Creating report...")
        
        success <- generate_word_report(
          filtered_data = results$filtered_data,
          filtered_analysis = results$filtered_analysis,
          country_code = results$country_code,
          output_file = file,
          filters_text = ""
        )
        
        if (success) {
          output$export_status <- renderText("Word report generated successfully!")
        } else {
          output$export_status <- renderText("Error generating Word report. Check console for details.")
        }
      })
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
