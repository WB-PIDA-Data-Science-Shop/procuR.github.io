# ========================================================================
# ECONOMIC OUTCOMES SHINY APP (WITH FILTERS)
# ========================================================================
# 
# This app provides an interactive interface for analyzing economic outcomes
# in public procurement, including market composition, supplier dynamics,
# buyer-supplier networks, relative price diagnostics, and competition metrics.
#
# Author: Generated based on econ_out_utils.R and econ_out_report.Rmd
# Date: January 2026
# Modified: Added filter functionality following admin_app pattern
# ========================================================================

library(shiny)
library(shinydashboard)
library(shinyWidgets)
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
  source("econ_out_utils.R")
  TRUE
}, error = function(e) {
  warning("Could not load econ_out_utils.R - ", e$message)
  FALSE
})

# Create placeholder functions if utils not loaded
if (!utils_loaded) {
  warning("Running in demo mode without utils. Please update the source() path on line 32.")
  
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

`%ni%` <- Negate(`%in%`)

# Helper to create filter caption text
get_filter_caption <- function(filters) {
  if (is.null(filters)) return("")
  
  parts <- character()
  
  if (!is.null(filters$year) && length(filters$year) == 2) {
    parts <- c(parts, sprintf("Years: %d-%d", filters$year[1], filters$year[2]))
  }
  
  if (!is.null(filters$market) && length(filters$market) > 0 && "All" %ni% filters$market) {
    parts <- c(parts, sprintf("Markets: %s", paste(filters$market, collapse = ", ")))
  }
  
  if (!is.null(filters$value) && length(filters$value) == 2) {
    val_text <- sprintf("Value range applied")
    parts <- c(parts, val_text)
  }
  
  if (!is.null(filters$buyer_type) && length(filters$buyer_type) > 0 && "All" %ni% filters$buyer_type) {
    parts <- c(parts, sprintf("Buyer types: %s", paste(filters$buyer_type, collapse = ", ")))
  }
  
  if (!is.null(filters$procedure_type) && length(filters$procedure_type) > 0 && "All" %ni% filters$procedure_type) {
    parts <- c(parts, sprintf("Procedures: %s", paste(filters$procedure_type, collapse = ", ")))
  }
  
  if (length(parts) == 0) return("")
  
  paste("Filters applied:", paste(parts, collapse = "; "))
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

# Filter data based on user selections
filter_data <- function(df, year_range = NULL, market = NULL, value_range = NULL,
                        buyer_type = NULL, procedure_type = NULL, value_divisor = 1) {
  
  filtered <- df
  
  # Year filter
  if (!is.null(year_range) && "tender_year" %in% names(df)) {
    filtered <- filtered %>%
      filter(tender_year >= year_range[1] & tender_year <= year_range[2])
  }
  
  # Market filter (using cpv_cluster column added by pipeline)
  if (!is.null(market) && length(market) > 0 && "All" %ni% market && "cpv_cluster" %in% names(df)) {
    filtered <- filtered %>%
      filter(cpv_cluster %in% market)
  }
  
  # Value filter
  if (!is.null(value_range) && "lot_estimatedprice" %in% names(df)) {
    filtered <- filtered %>%
      filter(!is.na(lot_estimatedprice)) %>%
      filter(lot_estimatedprice >= value_range[1] * value_divisor &
               lot_estimatedprice <= value_range[2] * value_divisor)
  }
  
  # Buyer type filter
  if (!is.null(buyer_type) && length(buyer_type) > 0 && "All" %ni% buyer_type && "buyer_buyertype" %in% names(df)) {
    filtered <- filtered %>%
      filter(buyer_buyertype %in% buyer_type)
  }
  
  # Procedure type filter
  if (!is.null(procedure_type) && length(procedure_type) > 0 && "All" %ni% procedure_type && "tender_proceduretype" %in% names(df)) {
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

This figure illustrates the entrance dynamics across CPV markets and years, separating contracts awarded to new suppliers vs. those given to repeat suppliers. High volatility with many new entrants may signal competitive contestability, while heavy reliance on repeat suppliers suggests stable but potentially closed markets.

```{r fig.height=10}
if (!is.null(filtered_analysis$suppliers_entrance)) {
  print(filtered_analysis$suppliers_entrance)
}
```

### Unique Suppliers Over Time

This chart tracks how many distinct suppliers participate in each market-year combination. A declining trend in unique suppliers may indicate market consolidation or reduced entry, while an increasing trend suggests more competitive openness.

```{r fig.height=10}
if (!is.null(filtered_analysis$unique_supp)) {
  print(filtered_analysis$unique_supp)
}
```

\\newpage

# Buyer-Supplier Networks

## Are buyers able to choose from a variety of market offerings?

This section uses network diagrams to visualize the connections between buyers and suppliers in selected CPV markets. Dense, well-connected networks suggest diverse sourcing opportunities, while star-shaped networks around a single supplier or small cliques may indicate limited alternatives for buyers.

```{r fig.height=12}
if (!is.null(filtered_analysis$network_plots) && length(filtered_analysis$network_plots) > 0) {
  for (plot in filtered_analysis$network_plots) {
    if (!is.null(plot)) {
      print(plot)
    }
  }
}
```

\\newpage

# Relative Price Analysis

## Are there price savings or price overruns prevailing?

### Density of Relative Prices

This plot shows how contract prices compare to their corresponding estimates. Values below 1 indicate savings relative to the estimate, while values above 1 indicate overruns. The shape of the distribution reveals whether most contracts come in on budget, below budget, or above budget.

```{r fig.height=7}
if (!is.null(filtered_analysis$rel_tot)) {
  print(filtered_analysis$rel_tot)
}
```

### Relative Prices Over Time

This figure shows how the contract-to-estimate ratio changes over time. An upward trend may signal inflationary pressures or worsening estimation accuracy. A downward trend may reflect improved cost control or more conservative initial estimates.

```{r fig.height=7}
if (!is.null(filtered_analysis$rel_year)) {
  print(filtered_analysis$rel_year)
}
```

### Top Markets by Relative Prices

This chart highlights the markets where contracted prices tend to be highest relative to estimates. Consistent overruns in certain markets may point to chronic underestimation, unique market conditions, or pricing challenges.

```{r fig.height=7}
if (!is.null(filtered_analysis$rel_10)) {
  print(filtered_analysis$rel_10)
}
```

### Top Buyers by Relative Prices

This chart identifies specific buyers whose contracted prices tend to be highest relative to estimates. Buyers with persistent overruns may face capacity constraints, market power issues, or estimation difficulties that warrant further investigation.

```{r fig.height=7}
if (!is.null(filtered_analysis$rel_buy)) {
  print(filtered_analysis$rel_buy)
}
```

\\newpage

# Competition Analysis

## Is there high competition?

### Single-Bid Share by Procedure

This chart breaks down single-bid incidence by procedure type. Direct awards and negotiated procedures without publication naturally have higher single-bid rates, but high single-bid rates in open or restricted procedures may signal barriers to competition.

```{r fig.height=7}
if (!is.null(filtered_analysis$single_bid_by_procedure)) {
  print(filtered_analysis$single_bid_by_procedure)
}
```

### Single-Bid by Market × Procedure × Value

This advanced visualization combines market, procedure, and value dimensions to identify specific contexts where single bidding is most prevalent. Look for patterns where certain procedure-market-value combinations consistently attract only one bidder.

```{r fig.height=8}
if (!is.null(filtered_analysis$single_bid_market_procedure_price_top)) {
  print(filtered_analysis$single_bid_market_procedure_price_top)
}
```

### Single-Bid Share by Contract Value

This plot shows how single-bid incidence varies by contract size. Higher single-bid rates in large-value contracts may reflect supplier capacity limitations, while high rates in small contracts may suggest lack of interest or excessive administrative burden.

```{r fig.height=7}
if (!is.null(filtered_analysis$single_bid_by_price)) {
  print(filtered_analysis$single_bid_by_price)
}
```

### Single-Bid by Value × Procedure

This chart combines contract value and procedure type to show how single-bid incidence varies across these two dimensions. It helps identify whether single bidding is driven more by procedure choice or contract characteristics.

```{r fig.height=7}
if (!is.null(filtered_analysis$single_bid_by_price_and_procedure)) {
  print(filtered_analysis$single_bid_by_price_and_procedure)
}
```

### Single-Bid by Buyer Group

This chart shows single-bid rates across different buyer types. Variations by buyer group may reflect differences in capacity, market power, or procurement practices across organizational types.

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
            title = "Contract Value Distribution by Year",
            width = 12,
            solidHeader = TRUE,
            status = "info",
            plotOutput("value_by_year_plot", height = "400px")
          )
        )
      ),
      
      # Market Sizing Tab
      tabItem(
        tabName = "market_sizing",
        h2("Market Sizing Analysis"),
        
        # FILTERS BOX
        fluidRow(
          box(
            title = "Filters",
            width = 12,
            collapsible = TRUE,
            collapsed = FALSE,
            status = "info",
            
            fluidRow(
              column(2, uiOutput("year_filter_market")),
              column(2, uiOutput("market_filter_market")),
              column(2, uiOutput("value_filter_market")),
              column(3, uiOutput("buyer_type_filter_market")),
              column(3, uiOutput("procedure_type_filter_market"))
            ),
            
            fluidRow(
              column(12,
                     actionButton("apply_filters_market", "Apply Filters", icon = icon("filter"), class = "btn-primary"),
                     actionButton("reset_filters_market", "Reset Filters", icon = icon("undo"), class = "btn-warning"),
                     textOutput("filter_status_market", inline = TRUE)
              )
            )
          )
        ),
        
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
                A small number of very large markets suggests concentration of budget."
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
                "This plot combines volume and typical contract size to show how markets differ in structure."
            ),
            plotOutput("market_size_av_plot", height = "600px"),
            downloadButton("download_market_size_av", "Download Figure", class = "download-btn btn-sm")
          )
        )
      ),
      
      # Supplier Dynamics Tab
      tabItem(
        tabName = "supplier_dynamics",
        h2("Supplier Dynamics Analysis"),
        
        # FILTERS BOX
        fluidRow(
          box(
            title = "Filters",
            width = 12,
            collapsible = TRUE,
            collapsed = FALSE,
            status = "info",
            
            fluidRow(
              column(2, uiOutput("year_filter_supplier")),
              column(2, uiOutput("market_filter_supplier")),
              column(2, uiOutput("value_filter_supplier")),
              column(3, uiOutput("buyer_type_filter_supplier")),
              column(3, uiOutput("procedure_type_filter_supplier"))
            ),
            
            fluidRow(
              column(12,
                     actionButton("apply_filters_supplier", "Apply Filters", icon = icon("filter"), class = "btn-primary"),
                     actionButton("reset_filters_supplier", "Reset Filters", icon = icon("undo"), class = "btn-warning"),
                     textOutput("filter_status_supplier", inline = TRUE)
              )
            )
          )
        ),
        
        div(class = "question-header",
            "How volatile are the markets in terms of new vs old suppliers?"
        ),
        
        fluidRow(
          box(
            title = "New vs Repeat Suppliers",
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            div(class = "description-box",
                "This figure shows how supplier participation evolves over time within markets."
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
            status = "primary",
            div(class = "description-box",
                "This chart tracks how many distinct suppliers participate in each market-year combination."
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
        
        # FILTERS BOX
        fluidRow(
          box(
            title = "Filters",
            width = 12,
            collapsible = TRUE,
            collapsed = FALSE,
            status = "info",
            
            fluidRow(
              column(2, uiOutput("year_filter_network")),
              column(2, uiOutput("market_filter_network")),
              column(2, uiOutput("value_filter_network")),
              column(3, uiOutput("buyer_type_filter_network")),
              column(3, uiOutput("procedure_type_filter_network"))
            ),
            
            fluidRow(
              column(12,
                     actionButton("apply_filters_network", "Apply Filters", icon = icon("filter"), class = "btn-primary"),
                     actionButton("reset_filters_network", "Reset Filters", icon = icon("undo"), class = "btn-warning"),
                     textOutput("filter_status_network", inline = TRUE)
              )
            )
          )
        ),
        
        div(class = "question-header",
            "Are buyers able to choose from a variety of market offerings?"
        ),
        
        fluidRow(
          box(
            title = "Network Visualizations",
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            div(class = "description-box",
                "These network diagrams visualize buyer-supplier relationships in selected CPV markets."
            ),
            uiOutput("network_plots_ui")
          )
        )
      ),
      
      # Relative Prices Tab
      tabItem(
        tabName = "relative_prices",
        h2("Relative Price Analysis"),
        
        # FILTERS BOX
        fluidRow(
          box(
            title = "Filters",
            width = 12,
            collapsible = TRUE,
            collapsed = FALSE,
            status = "info",
            
            fluidRow(
              column(2, uiOutput("year_filter_price")),
              column(2, uiOutput("market_filter_price")),
              column(2, uiOutput("value_filter_price")),
              column(3, uiOutput("buyer_type_filter_price")),
              column(3, uiOutput("procedure_type_filter_price"))
            ),
            
            fluidRow(
              column(12,
                     actionButton("apply_filters_price", "Apply Filters", icon = icon("filter"), class = "btn-primary"),
                     actionButton("reset_filters_price", "Reset Filters", icon = icon("undo"), class = "btn-warning"),
                     textOutput("filter_status_price", inline = TRUE)
              )
            )
          )
        ),
        
        div(class = "question-header",
            "Are there price savings or price overruns prevailing?"
        ),
        
        fluidRow(
          box(
            title = "Density of Relative Prices",
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            div(class = "description-box",
                "This plot shows how contract prices compare to their corresponding estimates."
            ),
            plotOutput("rel_tot_plot", height = "600px"),
            downloadButton("download_rel_tot", "Download Figure", class = "download-btn btn-sm")
          )
        ),
        
        fluidRow(
          box(
            title = "Relative Prices Over Time",
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            div(class = "description-box",
                "This figure shows how the contract-to-estimate ratio changes over time."
            ),
            plotOutput("rel_year_plot", height = "600px"),
            downloadButton("download_rel_year", "Download Figure", class = "download-btn btn-sm")
          )
        ),
        
        fluidRow(
          box(
            title = "Top Markets by Relative Prices",
            width = 6,
            solidHeader = TRUE,
            status = "primary",
            plotOutput("rel_10_plot", height = "500px"),
            downloadButton("download_rel_10", "Download Figure", class = "download-btn btn-sm")
          ),
          box(
            title = "Top Buyers by Relative Prices",
            width = 6,
            solidHeader = TRUE,
            status = "primary",
            plotOutput("rel_buy_plot", height = "500px"),
            downloadButton("download_rel_buy", "Download Figure", class = "download-btn btn-sm")
          )
        )
      ),
      
      # Competition Tab
      tabItem(
        tabName = "competition",
        h2("Competition Analysis"),
        
        # FILTERS BOX
        fluidRow(
          box(
            title = "Filters",
            width = 12,
            collapsible = TRUE,
            collapsed = FALSE,
            status = "info",
            
            fluidRow(
              column(2, uiOutput("year_filter_competition")),
              column(2, uiOutput("market_filter_competition")),
              column(2, uiOutput("value_filter_competition")),
              column(3, uiOutput("buyer_type_filter_competition")),
              column(3, uiOutput("procedure_type_filter_competition"))
            ),
            
            fluidRow(
              column(12,
                     actionButton("apply_filters_competition", "Apply Filters", icon = icon("filter"), class = "btn-primary"),
                     actionButton("reset_filters_competition", "Reset Filters", icon = icon("undo"), class = "btn-warning"),
                     textOutput("filter_status_competition", inline = TRUE)
              )
            )
          )
        ),
        
        div(class = "question-header",
            "Is there high competition?"
        ),
        
        fluidRow(
          box(
            title = "Single-Bid Share by Procedure",
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            div(class = "description-box",
                "This chart breaks down single-bid incidence by procedure type."
            ),
            plotOutput("single_bid_procedure_plot", height = "600px"),
            downloadButton("download_single_bid_procedure", "Download Figure", class = "download-btn btn-sm")
          )
        ),
        
        fluidRow(
          box(
            title = "Single-Bid by Market × Procedure × Value",
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            div(class = "description-box",
                "This visualization combines market, procedure, and value dimensions."
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
            status = "primary",
            plotOutput("single_bid_price_plot", height = "500px"),
            downloadButton("download_single_bid_price", "Download Figure", class = "download-btn btn-sm")
          ),
          box(
            title = "Single-Bid by Value × Procedure",
            width = 6,
            solidHeader = TRUE,
            status = "primary",
            plotOutput("single_bid_price_proc_plot", height = "500px"),
            downloadButton("download_single_bid_price_proc", "Download Figure", class = "download-btn btn-sm")
          )
        ),
        
        fluidRow(
          box(
            title = "Single-Bid by Buyer Group",
            width = 6,
            solidHeader = TRUE,
            status = "primary",
            plotOutput("single_bid_buyer_group_plot", height = "500px"),
            downloadButton("download_single_bid_buyer_group", "Download Figure", class = "download-btn btn-sm")
          ),
          box(
            title = "Top Buyers by Single-Bid Incidence",
            width = 6,
            solidHeader = TRUE,
            status = "primary",
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
            title = "Complete Reports",
            width = 6,
            solidHeader = TRUE,
            status = "primary",
            p("Generate comprehensive analysis reports."),
            downloadButton("download_pdf", "Download PDF Report", class = "btn-primary btn-lg download-btn"),
            br(), br(),
            downloadButton("download_word", "Download Word Report", class = "btn-info btn-lg download-btn")
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
            title = "Export Status",
            width = 12,
            solidHeader = TRUE,
            status = "info",
            textOutput("export_status")
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
  
  # Reactive values
  results <- reactiveValues(
    data = NULL,
    analysis = NULL,
    filtered_data = NULL,
    filtered_analysis = NULL,
    country_code = NULL,
    cpv_lookup = NULL,
    value_divisor = 1
  )
  
  filters <- reactiveValues(
    overview = list(year = NULL, market = NULL, value = NULL, buyer_type = NULL, procedure_type = NULL),
    market = list(year = NULL, market = NULL, value = NULL, buyer_type = NULL, procedure_type = NULL),
    supplier = list(year = NULL, market = NULL, value = NULL, buyer_type = NULL, procedure_type = NULL),
    network = list(year = NULL, market = NULL, value = NULL, buyer_type = NULL, procedure_type = NULL),
    price = list(year = NULL, market = NULL, value = NULL, buyer_type = NULL, procedure_type = NULL),
    competition = list(year = NULL, market = NULL, value = NULL, buyer_type = NULL, procedure_type = NULL)
  )
  
  # System status
  output$system_status_ui <- renderUI({
    status_text <- paste0(
      "Utils loaded: ", ifelse(utils_loaded, "✓ Yes", "✗ No"), "\n",
      if (!utils_loaded) {
        "⚠ Please update the source() path in the app file to load econ_out_utils.R"
      } else {
        "✓ Ready to analyze data"
      }
    )
    
    tags$pre(status_text)
  })
  
  # ========================================================================
  # DATA LOADING
  # ========================================================================
  
  observeEvent(input$run_analysis, {
    req(input$datafile, input$country_code)
    
    if (!utils_loaded) {
      showNotification("Cannot run analysis: econ_out_utils.R not loaded", 
                       type = "error", duration = NULL)
      return()
    }
    
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
        
        df <- as.data.frame(df)
        
        incProgress(0.3, detail = "Building CPV lookup...")
        
        cpv_lookup <- NULL
        if (!is.null(input$cpv_file)) {
          tryCatch({
            # Read the CPV file as a dataframe
            cpv_table <- fread(input$cpv_file$datapath, encoding = "UTF-8", stringsAsFactors = FALSE)
            
            cat("CPV file loaded. Columns:", paste(names(cpv_table), collapse = ", "), "\n")
            cat("First few rows:\n")
            print(head(cpv_table, 3))
            
            # Try to detect column names
            code_col <- NULL
            label_col <- NULL
            
            # Look for CODE column
            if ("CODE" %in% names(cpv_table)) {
              code_col <- "CODE"
            } else if ("code" %in% names(cpv_table)) {
              code_col <- "code"
            } else if ("cpv_code" %in% names(cpv_table)) {
              code_col <- "cpv_code"
            } else if ("CPV" %in% names(cpv_table)) {
              code_col <- "CPV"
            } else {
              code_col <- names(cpv_table)[1]  # Use first column
            }
            
            # Look for label column
            if ("EN" %in% names(cpv_table)) {
              label_col <- "EN"
            } else if ("en" %in% names(cpv_table)) {
              label_col <- "en"
            } else if ("description" %in% names(cpv_table)) {
              label_col <- "description"
            } else if ("name" %in% names(cpv_table)) {
              label_col <- "name"
            } else if ("label" %in% names(cpv_table)) {
              label_col <- "label"
            } else {
              label_col <- names(cpv_table)[2]  # Use second column
            }
            
            cat("Using columns:", code_col, "(code) and", label_col, "(label)\n")
            
            # Call the build_cpv_lookup function
            cpv_lookup <- build_cpv_lookup(cpv_table, code_col = code_col, label_col = label_col)
            
            cat("CPV lookup created with", nrow(cpv_lookup$cpv_2d), "2-digit entries and",
                nrow(cpv_lookup$cpv_3d), "3-digit entries\n")
            
            showNotification(paste("CPV codes loaded:", nrow(cpv_lookup$cpv_2d), "2-digit codes"), 
                             type = "message", duration = 3)
            
          }, error = function(e) {
            cat("Error loading CPV file:", e$message, "\n")
            showNotification(paste("Could not load CPV file:", e$message), 
                             type = "warning", duration = 5)
            cpv_lookup <- NULL
          })
        }
        
        # If no CPV lookup provided or loading failed, leave as NULL
        # The pipeline will handle creating a default one
        if (is.null(cpv_lookup)) {
          cat("No CPV lookup provided, pipeline will use defaults\n")
          showNotification("No CPV codes file - using defaults from data", 
                           type = "message", duration = 3)
        }
        
        incProgress(0.4, detail = "Running analysis...")
        
        country_code <- toupper(input$country_code)
        
        network_cpv_list <- if (!input$skip_networks && nchar(input$network_cpv) > 0) {
          as.character(unlist(strsplit(input$network_cpv, ",")))
        } else {
          character(0)
        }
        
        analysis_results <- run_economic_efficiency_pipeline(
          df = df,
          country_code = country_code,
          output_dir = tempdir(),
          cpv_lookup = cpv_lookup,
          network_cpv_clusters = network_cpv_list
        )
        
        incProgress(0.9, detail = "Finalizing...")
        
        # The pipeline returns results but doesn't modify df
        # We need to add tender_year and cpv_cluster to our stored data
        df_processed <- df
        
        # Add tender_year if not present
        if (!"tender_year" %in% names(df_processed)) {
          # Use the add_tender_year function from utils
          if (exists("add_tender_year")) {
            df_processed <- add_tender_year(df_processed)
          } else {
            # Fallback: extract from date columns
            date_cols <- c("tender_publications_firstcallfortenderdate",
                           "tender_awarddecisiondate", 
                           "tender_biddeadline")
            cols_present <- intersect(date_cols, names(df_processed))
            if (length(cols_present) > 0) {
              get_year <- function(x) substr(x, 1, 4)
              year_vec <- NA
              for (col in cols_present) {
                if (is.na(year_vec[1])) {
                  year_vec <- get_year(df_processed[[col]])
                } else {
                  year_vec <- ifelse(is.na(year_vec), get_year(df_processed[[col]]), year_vec)
                }
              }
              df_processed$tender_year <- as.integer(year_vec)
            }
          }
        }
        
        # Add cpv_cluster if not present
        if (!"cpv_cluster" %in% names(df_processed) && "lot_productcode" %in% names(df_processed)) {
          df_processed <- df_processed %>%
            mutate(cpv_cluster = substr(as.character(lot_productcode), 1, 2))
        }
        
        results$data <- df_processed
        results$analysis <- analysis_results
        results$filtered_data <- df_processed
        results$filtered_analysis <- analysis_results
        results$country_code <- country_code
        results$cpv_lookup <- cpv_lookup
        
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
  output$year_filter_overview <- output$year_filter_market <- 
    output$year_filter_supplier <- output$year_filter_network <- 
    output$year_filter_price <- output$year_filter_competition <- renderUI({
      # Use filtered_data which has tender_year added by pipeline
      req(results$filtered_data)
      
      if ("tender_year" %in% names(results$filtered_data)) {
        years <- results$filtered_data$tender_year
        years <- years[!is.na(years)]
        
        if (length(years) > 0) {
          sliderInput("year_range", "Year Range:",
                      min = min(years),
                      max = max(years),
                      value = c(min(years), max(years)),
                      step = 1,
                      sep = "")
        }
      } else {
        p("Year data will be available after analysis runs")
      }
    })
  
  # Market filters (CPV clusters generated by pipeline)
  output$market_filter_overview <- output$market_filter_market <- 
    output$market_filter_supplier <- output$market_filter_network <- 
    output$market_filter_price <- output$market_filter_competition <- renderUI({
      # Use filtered_data which has cpv_cluster added by pipeline
      req(results$filtered_data)
      
      if ("cpv_cluster" %in% names(results$filtered_data)) {
        cpv_codes <- unique(results$filtered_data$cpv_cluster)
        cpv_codes <- cpv_codes[!is.na(cpv_codes) & cpv_codes != ""]
        cpv_codes <- sort(cpv_codes)
        
        if (length(cpv_codes) > 0) {
          pickerInput("market_filter", "Market (CPV):",
                      choices = c("All", cpv_codes),
                      selected = "All",
                      multiple = TRUE,
                      options = list(`actions-box` = TRUE, `live-search` = TRUE))
        }
      } else {
        p("Market data will be available after analysis runs")
      }
    })
  
  # Value filters
  output$value_filter_overview <- output$value_filter_market <- 
    output$value_filter_supplier <- output$value_filter_network <- 
    output$value_filter_price <- output$value_filter_competition <- renderUI({
      req(results$filtered_data)
      
      if ("lot_estimatedprice" %in% names(results$filtered_data)) {
        prices <- results$filtered_data$lot_estimatedprice
        prices <- prices[!is.na(prices) & prices > 0]
        
        if (length(prices) > 0) {
          min_price <- min(prices)
          max_price <- max(prices)
          
          # Determine scale
          if (max_price > 1000000) {
            results$value_divisor <- 1000000
            label_text <- "Value Range (Millions):"
          } else if (max_price > 1000) {
            results$value_divisor <- 1000
            label_text <- "Value Range (Thousands):"
          } else {
            results$value_divisor <- 1
            label_text <- "Value Range:"
          }
          
          sliderInput("value_range", label_text,
                      min = round(min_price / results$value_divisor, 2),
                      max = round(max_price / results$value_divisor, 2),
                      value = c(round(min_price / results$value_divisor, 2),
                                round(max_price / results$value_divisor, 2)),
                      step = round((max_price - min_price) / results$value_divisor / 100, 2))
        }
      }
    })
  
  # Buyer type filters
  output$buyer_type_filter_overview <- output$buyer_type_filter_market <- 
    output$buyer_type_filter_supplier <- output$buyer_type_filter_network <- 
    output$buyer_type_filter_price <- output$buyer_type_filter_competition <- renderUI({
      req(results$filtered_data)
      
      if ("buyer_buyertype" %in% names(results$filtered_data)) {
        buyer_types <- unique(results$filtered_data$buyer_buyertype)
        buyer_types <- buyer_types[!is.na(buyer_types)]
        buyer_types <- sort(buyer_types)
        
        if (length(buyer_types) > 0) {
          pickerInput("buyer_type_filter", "Buyer Type:",
                      choices = c("All", buyer_types),
                      selected = "All",
                      multiple = TRUE,
                      options = list(`actions-box` = TRUE))
        }
      }
    })
  
  # Procedure type filters
  output$procedure_type_filter_overview <- output$procedure_type_filter_market <- 
    output$procedure_type_filter_supplier <- output$procedure_type_filter_network <- 
    output$procedure_type_filter_price <- output$procedure_type_filter_competition <- renderUI({
      req(results$filtered_data)
      
      if ("tender_proceduretype" %in% names(results$filtered_data)) {
        proc_types <- unique(results$filtered_data$tender_proceduretype)
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
    req(results$filtered_data, results$filtered_analysis)
    
    current_filters <- list(
      year = input$year_range,
      market = input$market_filter,
      value = input$value_range,
      buyer_type = input$buyer_type_filter,
      procedure_type = input$procedure_type_filter
    )
    
    # Store filters for this tab
    if (tab_name == "overview") filters$overview <- current_filters
    else if (tab_name == "market") filters$market <- current_filters
    else if (tab_name == "supplier") filters$supplier <- current_filters
    else if (tab_name == "network") filters$network <- current_filters
    else if (tab_name == "price") filters$price <- current_filters
    else if (tab_name == "competition") filters$competition <- current_filters
    
    # Apply filters to the already-processed data (which has tender_year and cpv_cluster)
    # Use results$data which has been processed by the initial pipeline run
    base_data <- if (!is.null(results$data) && 
                     "tender_year" %in% names(results$data) && 
                     "cpv_cluster" %in% names(results$data)) {
      results$data
    } else {
      # If base data doesn't have these columns, use filtered_data
      results$filtered_data
    }
    
    filtered <- filter_data(
      df = base_data,
      year_range = current_filters$year,
      market = current_filters$market,
      value_range = current_filters$value,
      buyer_type = current_filters$buyer_type,
      procedure_type = current_filters$procedure_type,
      value_divisor = results$value_divisor
    )
    
    # Re-run analysis on filtered data
    withProgress(message = 'Applying filters...', value = 0, {
      incProgress(0.5, detail = "Re-analyzing data...")
      
      tryCatch({
        filtered_analysis <- run_economic_efficiency_pipeline(
          df = filtered,
          country_code = results$country_code,
          output_dir = tempdir(),
          cpv_lookup = results$cpv_lookup,
          network_cpv_clusters = character(0)  # Skip networks on filtered data for speed
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
  observeEvent(input$apply_filters_market, { apply_tab_filters("market") })
  observeEvent(input$apply_filters_supplier, { apply_tab_filters("supplier") })
  observeEvent(input$apply_filters_network, { apply_tab_filters("network") })
  observeEvent(input$apply_filters_price, { apply_tab_filters("price") })
  observeEvent(input$apply_filters_competition, { apply_tab_filters("competition") })
  
  # Reset filter observers
  observeEvent(input$reset_filters_overview, {
    filters$overview <- list(year = NULL, market = NULL, value = NULL, buyer_type = NULL, procedure_type = NULL)
    results$filtered_data <- results$data
    results$filtered_analysis <- results$analysis
    showNotification("Filters reset", type = "message", duration = 2)
  })
  
  observeEvent(input$reset_filters_market, {
    filters$market <- list(year = NULL, market = NULL, value = NULL, buyer_type = NULL, procedure_type = NULL)
    results$filtered_data <- results$data
    results$filtered_analysis <- results$analysis
    showNotification("Filters reset", type = "message", duration = 2)
  })
  
  observeEvent(input$reset_filters_supplier, {
    filters$supplier <- list(year = NULL, market = NULL, value = NULL, buyer_type = NULL, procedure_type = NULL)
    results$filtered_data <- results$data
    results$filtered_analysis <- results$analysis
    showNotification("Filters reset", type = "message", duration = 2)
  })
  
  observeEvent(input$reset_filters_network, {
    filters$network <- list(year = NULL, market = NULL, value = NULL, buyer_type = NULL, procedure_type = NULL)
    results$filtered_data <- results$data
    results$filtered_analysis <- results$analysis
    showNotification("Filters reset", type = "message", duration = 2)
  })
  
  observeEvent(input$reset_filters_price, {
    filters$price <- list(year = NULL, market = NULL, value = NULL, buyer_type = NULL, procedure_type = NULL)
    results$filtered_data <- results$data
    results$filtered_analysis <- results$analysis
    showNotification("Filters reset", type = "message", duration = 2)
  })
  
  observeEvent(input$reset_filters_competition, {
    filters$competition <- list(year = NULL, market = NULL, value = NULL, buyer_type = NULL, procedure_type = NULL)
    results$filtered_data <- results$data
    results$filtered_analysis <- results$analysis
    showNotification("Filters reset", type = "message", duration = 2)
  })
  
  # Filter status outputs
  output$filter_status_overview <- renderText({
    paste("  ", get_filter_description(filters$overview))
  })
  
  output$filter_status_market <- renderText({
    paste("  ", get_filter_description(filters$market))
  })
  
  output$filter_status_supplier <- renderText({
    paste("  ", get_filter_description(filters$supplier))
  })
  
  output$filter_status_network <- renderText({
    paste("  ", get_filter_description(filters$network))
  })
  
  output$filter_status_price <- renderText({
    paste("  ", get_filter_description(filters$price))
  })
  
  output$filter_status_competition <- renderText({
    paste("  ", get_filter_description(filters$competition))
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
    
    years <- if ("tender_year" %in% names(df)) {
      unique(df$tender_year[!is.na(df$tender_year)])
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
  
  output$contracts_year_plot <- renderPlot({
    req(results$filtered_data)
    df <- results$filtered_data
    
    if ("tender_year" %in% names(df)) {
      year_counts <- df %>%
        group_by(tender_year) %>%
        summarise(n = n()) %>%
        ungroup()
      
      ggplot(year_counts, aes(x = tender_year, y = n)) +
        geom_col(fill = "#3c8dbc") +
        geom_text(aes(label = formatC(n, format = "d", big.mark = ",")),
                  vjust = -0.5, size = 4) +
        labs(x = "Year", y = "Number of Contracts", title = "Contracts per Year") +
        theme_minimal(base_size = 14) +
        scale_y_continuous(labels = scales::comma)
    }
  })
  
  output$value_by_year_plot <- renderPlot({
    req(results$filtered_data)
    df <- results$filtered_data
    
    if ("tender_year" %in% names(df) && "lot_estimatedprice" %in% names(df)) {
      year_values <- df %>%
        filter(!is.na(lot_estimatedprice) & lot_estimatedprice > 0) %>%
        group_by(tender_year) %>%
        summarise(
          total_value = sum(lot_estimatedprice, na.rm = TRUE),
          avg_value = mean(lot_estimatedprice, na.rm = TRUE),
          .groups = "drop"
        )
      
      # Determine scale for display
      max_val <- max(year_values$total_value, na.rm = TRUE)
      if (max_val > 1e9) {
        year_values <- year_values %>%
          mutate(total_value_display = total_value / 1e9)
        y_label <- "Total Contract Value (Billions)"
      } else if (max_val > 1e6) {
        year_values <- year_values %>%
          mutate(total_value_display = total_value / 1e6)
        y_label <- "Total Contract Value (Millions)"
      } else if (max_val > 1e3) {
        year_values <- year_values %>%
          mutate(total_value_display = total_value / 1e3)
        y_label <- "Total Contract Value (Thousands)"
      } else {
        year_values <- year_values %>%
          mutate(total_value_display = total_value)
        y_label <- "Total Contract Value"
      }
      
      ggplot(year_values, aes(x = tender_year, y = total_value_display)) +
        geom_col(fill = "#00a65a") +
        geom_text(aes(label = format(round(total_value_display, 1), big.mark = ",")),
                  vjust = -0.5, size = 4) +
        labs(x = "Year", y = y_label, 
             title = "Total Contract Value by Year") +
        theme_minimal(base_size = 14) +
        scale_y_continuous(labels = scales::comma)
    }
  })
  
  output$cpv_legend_table <- DT::renderDataTable({
    # cpv_lookup is a list with $cpv_2d and $cpv_3d dataframes
    if (!is.null(results$cpv_lookup) && is.list(results$cpv_lookup) && !is.null(results$cpv_lookup$cpv_2d)) {
      cpv_2d_table <- results$cpv_lookup$cpv_2d
      
      # Rename columns for display
      if ("cpv_cluster" %in% names(cpv_2d_table) && "cpv_category" %in% names(cpv_2d_table)) {
        display_table <- cpv_2d_table %>%
          rename(`CPV Code` = cpv_cluster, `Category` = cpv_category)
      } else {
        display_table <- cpv_2d_table
      }
      
      datatable(display_table, 
                options = list(pageLength = 10, scrollY = "350px", scrollCollapse = TRUE),
                rownames = FALSE,
                caption = "CPV 2-digit Market Definitions")
      
    } else if (!is.null(results$data) && "lot_productcode" %in% names(results$data)) {
      # Create on-the-fly CPV legend from data
      cpv_temp <- results$data %>%
        mutate(cpv_2dig = substr(as.character(lot_productcode), 1, 2)) %>%
        filter(!is.na(cpv_2dig) & cpv_2dig != "" & nchar(cpv_2dig) == 2) %>%
        group_by(cpv_2dig) %>%
        summarise(cpv_name = paste0("CPV ", cpv_2dig), 
                  n_contracts = n(), .groups = "drop") %>%
        arrange(cpv_2dig) %>%
        rename(`CPV Code` = cpv_2dig, `Category` = cpv_name, `Contracts` = n_contracts)
      
      datatable(cpv_temp, 
                options = list(pageLength = 10, scrollY = "350px", scrollCollapse = TRUE),
                rownames = FALSE,
                caption = "CPV codes extracted from data (no CPV file provided)")
    } else {
      datatable(data.frame(Message = "No CPV data available"), 
                options = list(dom = 't'), 
                rownames = FALSE)
    }
  })
  
  
  # ========================================================================
  # MARKET SIZING OUTPUTS
  # ========================================================================
  
  output$market_size_n_plot <- renderPlot({
    req(results$filtered_analysis$market_size_n)
    results$filtered_analysis$market_size_n
  })
  
  output$market_size_v_plot <- renderPlot({
    req(results$filtered_analysis$market_size_v)
    results$filtered_analysis$market_size_v
  })
  
  output$market_size_av_plot <- renderPlot({
    req(results$filtered_analysis$market_size_av)
    results$filtered_analysis$market_size_av
  })
  
  # ========================================================================
  # SUPPLIER DYNAMICS OUTPUTS
  # ========================================================================
  
  output$suppliers_entrance_plot <- renderPlot({
    req(results$filtered_analysis$suppliers_entrance)
    results$filtered_analysis$suppliers_entrance
  })
  
  output$unique_supp_plot <- renderPlot({
    req(results$filtered_analysis$unique_supp)
    results$filtered_analysis$unique_supp
  })
  
  # ========================================================================
  # NETWORK OUTPUTS
  # ========================================================================
  
  output$network_plots_ui <- renderUI({
    req(results$filtered_analysis$network_plots)
    
    if (length(results$filtered_analysis$network_plots) == 0) {
      return(p("No network plots available. Networks may have been skipped or no data matches criteria."))
    }
    
    plot_outputs <- lapply(seq_along(results$filtered_analysis$network_plots), function(i) {
      plot_name <- paste0("network_plot_", i)
      output[[plot_name]] <- renderPlot({
        results$filtered_analysis$network_plots[[i]]
      }, height = 800)
      
      box(
        title = names(results$filtered_analysis$network_plots)[i],
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        plotOutput(plot_name, height = "800px")
      )
    })
    
    do.call(tagList, plot_outputs)
  })
  
  # ========================================================================
  # RELATIVE PRICE OUTPUTS
  # ========================================================================
  
  output$rel_tot_plot <- renderPlot({
    req(results$filtered_analysis$rel_tot)
    results$filtered_analysis$rel_tot
  })
  
  output$rel_year_plot <- renderPlot({
    req(results$filtered_analysis$rel_year)
    results$filtered_analysis$rel_year
  })
  
  output$rel_10_plot <- renderPlot({
    req(results$filtered_analysis$rel_10)
    results$filtered_analysis$rel_10
  })
  
  output$rel_buy_plot <- renderPlot({
    req(results$filtered_analysis$rel_buy)
    results$filtered_analysis$rel_buy
  })
  
  # ========================================================================
  # COMPETITION OUTPUTS
  # ========================================================================
  
  output$single_bid_procedure_plot <- renderPlot({
    req(results$filtered_analysis$single_bid_by_procedure)
    results$filtered_analysis$single_bid_by_procedure
  })
  
  output$single_bid_market_proc_price_plot <- renderPlot({
    req(results$filtered_analysis$single_bid_market_procedure_price_top)
    results$filtered_analysis$single_bid_market_procedure_price_top
  })
  
  output$single_bid_price_plot <- renderPlot({
    req(results$filtered_analysis$single_bid_by_price)
    results$filtered_analysis$single_bid_by_price
  })
  
  output$single_bid_price_proc_plot <- renderPlot({
    req(results$filtered_analysis$single_bid_by_price_and_procedure)
    results$filtered_analysis$single_bid_by_price_and_procedure
  })
  
  output$single_bid_buyer_group_plot <- renderPlot({
    req(results$filtered_analysis$single_bid_by_buyer_group)
    results$filtered_analysis$single_bid_by_buyer_group
  })
  
  output$top_buyers_single_bid_plot <- renderPlot({
    req(results$filtered_analysis$top_buyers_single_bid)
    results$filtered_analysis$top_buyers_single_bid
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
  
  output$download_all_figures <- downloadHandler(
    filename = function() {
      paste0("all_figures_", results$country_code, "_", format(Sys.Date(), "%Y%m%d"), ".zip")
    },
    content = function(file) {
      req(results$filtered_analysis)
      
      withProgress(message = 'Creating ZIP file...', value = 0, {
        temp_dir <- tempfile()
        dir.create(temp_dir)
        
        # List all plots with their names
        plots <- list(
          list(plot = results$filtered_analysis$market_size_n, name = "market_size_n", width = 10, height = 7),
          list(plot = results$filtered_analysis$market_size_v, name = "market_size_v", width = 10, height = 7),
          list(plot = results$filtered_analysis$market_size_av, name = "market_size_av", width = 10, height = 7),
          list(plot = results$filtered_analysis$suppliers_entrance, name = "suppliers_entrance", width = 12, height = 10),
          list(plot = results$filtered_analysis$unique_supp, name = "unique_supp", width = 12, height = 10),
          list(plot = results$filtered_analysis$rel_tot, name = "rel_tot", width = 10, height = 7),
          list(plot = results$filtered_analysis$rel_year, name = "rel_year", width = 10, height = 7),
          list(plot = results$filtered_analysis$rel_10, name = "rel_10", width = 10, height = 7),
          list(plot = results$filtered_analysis$rel_buy, name = "rel_buy", width = 10, height = 7),
          list(plot = results$filtered_analysis$single_bid_by_procedure, name = "single_bid_procedure", width = 10, height = 7),
          list(plot = results$filtered_analysis$single_bid_market_procedure_price_top, name = "single_bid_market_proc_price", width = 12, height = 8),
          list(plot = results$filtered_analysis$single_bid_by_price, name = "single_bid_price", width = 10, height = 7),
          list(plot = results$filtered_analysis$single_bid_by_price_and_procedure, name = "single_bid_price_proc", width = 10, height = 7),
          list(plot = results$filtered_analysis$single_bid_by_buyer_group, name = "single_bid_buyer_group", width = 10, height = 7),
          list(plot = results$filtered_analysis$top_buyers_single_bid, name = "top_buyers_single_bid", width = 10, height = 7)
        )
        
        n_plots <- length(plots)
        saved_count <- 0
        
        for (i in seq_along(plots)) {
          incProgress(i/n_plots, detail = paste("Saving figure", i, "of", n_plots))
          
          if (!is.null(plots[[i]]$plot)) {
            file_path <- file.path(temp_dir, paste0(plots[[i]]$name, "_", results$country_code, ".png"))
            tryCatch({
              ggsave(file_path, plots[[i]]$plot, 
                     width = plots[[i]]$width, 
                     height = plots[[i]]$height, 
                     dpi = 300)
              saved_count <- saved_count + 1
            }, error = function(e) {
              message("Could not save ", plots[[i]]$name, ": ", e$message)
            })
          }
        }
        
        # Also save network plots if available
        if (!is.null(results$filtered_analysis$network_plots) && 
            length(results$filtered_analysis$network_plots) > 0) {
          for (j in seq_along(results$filtered_analysis$network_plots)) {
            if (!is.null(results$filtered_analysis$network_plots[[j]])) {
              network_name <- names(results$filtered_analysis$network_plots)[j]
              if (is.null(network_name) || network_name == "") {
                network_name <- paste0("network_", j)
              }
              file_path <- file.path(temp_dir, paste0(network_name, "_", results$country_code, ".png"))
              tryCatch({
                ggsave(file_path, results$filtered_analysis$network_plots[[j]], 
                       width = 12, height = 12, dpi = 300)
                saved_count <- saved_count + 1
              }, error = function(e) {
                message("Could not save network plot ", j, ": ", e$message)
              })
            }
          }
        }
        
        # Create ZIP file
        if (saved_count > 0) {
          zip::zip(zipfile = file, 
                   files = list.files(temp_dir, full.names = TRUE), 
                   mode = "cherry-pick")
          
          output$export_status <- renderText(
            paste0("Successfully saved ", saved_count, " figures to ZIP file!")
          )
        } else {
          output$export_status <- renderText("No figures available to save.")
        }
        
        unlink(temp_dir, recursive = TRUE)
      })
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
