# ========================================================================
# PROCUREMENT INTEGRITY ANALYSIS PIPELINE
# Version: 2.0
# Refactored for consistency, efficiency, and best practices
# ========================================================================

# ========================================================================
# PART 1: CONFIGURATION MANAGEMENT
# ========================================================================

#' Create pipeline configuration
#' 
#' @param country_code Two-letter country code
#' @return List of configuration parameters
create_pipeline_config <- function(country_code) {
  list(
    country = toupper(country_code),
    
    # Analysis thresholds
    thresholds = list(
      min_buyer_contracts = 100,
      min_suppliers_for_buyer_conc = 3,
      min_buyer_years = 3,
      cpv_digits = 3,
      min_bidders_for_edge = 4,
      top_n_buyers = 30,
      top_n_suppliers = 30,
      top_n_markets = 30,
      top_n_vars = 10,
      marginal_share_threshold = 0.05,
      max_wins_atypical = 3,
      min_history_threshold = 4,
      max_relative_price = 5,
      min_relative_price = 0
    ),
    
    # Year filters by component
    years = get_year_range(country_code, "default"),
    years_singleb = get_year_range(country_code, "singleb"),
    years_relprice = get_year_range(country_code, "rel_price"),
    
    # Model settings
    models = list(
      p_max = 0.10,
      fe_set = c("buyer", "year", "buyer+year", "buyer#year"),
      cluster_set = c("none", "buyer", "year", "buyer_year", "buyer_buyertype"),
      controls_set = c("x_only", "base", "base_extra"),
      model_types_relprice = c("ols_level", "ols_log", "gamma_log")
    ),
    
    # Plot settings
    plots = list(
      width = 10,
      height = 6,
      width_large = 12,
      height_large = 12,
      dpi = 300,
      base_size = 14
    )
  )
}

#' Year filter configuration for each country
year_filter_config <- tibble::tribble(
  ~component,  ~country_code, ~min_year, ~max_year,
  # default catch-all
  "default",   "BG",          NA,        NA,
  "default",   "UY",          NA,        NA,
  "default",   "UG",          NA,        NA,
  # component-specific overrides
  "singleb",   "BG",          2011,      2018,
  "singleb",   "UY",          2014,      NA,
  "singleb",   "UG",          NA,        NA,
  "rel_price", "BG",          2011,      2018,
  "rel_price", "UY",          2014,      NA,
  "rel_price", "UG",          NA,        NA
)

#' Get year range for specific component and country
#' 
#' @param country_code Two-letter country code
#' @param component One of "singleb", "rel_price", "default"
#' @return List with min_year and max_year
get_year_range <- function(country_code,
                           component = c("singleb", "rel_price", "default")) {
  component <- match.arg(component)
  cc <- toupper(country_code)
  
  # Try component-specific rule
  row_spec <- year_filter_config %>%
    dplyr::filter(component == !!component, country_code == !!cc) %>%
    dplyr::slice_head(n = 1)
  
  # Fall back to default for that country
  if (nrow(row_spec) == 0) {
    row_spec <- year_filter_config %>%
      dplyr::filter(component == "default", country_code == !!cc) %>%
      dplyr::slice_head(n = 1)
  }
  
  # If still nothing, no filtering
  if (nrow(row_spec) == 0) {
    return(list(min_year = -Inf, max_year = Inf))
  }
  
  min_y <- if (is.na(row_spec$min_year)) -Inf else row_spec$min_year
  max_y <- if (is.na(row_spec$max_year)) Inf else row_spec$max_year
  
  list(min_year = min_y, max_year = max_y)
}

# ========================================================================
# PART 2: LABEL DEFINITIONS AND LOOKUPS
# ========================================================================

#' Variable label lookup for display
label_lookup <- c(
  tender_id_missing_share = "Tender ID",
  tender_year_missing_share = "Tender Year",
  lot_number_missing_share = "Lot Number",
  bid_number_missing_share = "Number of Bids",
  bid_iswinning_missing_share = "Winning Bid",
  tender_country_missing_share = "Tender Country",
  tender_awarddecisiondate_missing_share = "Award Decision Date",
  tender_contractsignaturedate_missing_share = "Contract Signature Date",
  tender_biddeadline_missing_share = "Bid Deadline",
  tender_proceduretype_missing_share = "Procedure Type",
  tender_nationalproceduretype_missing_share = "National Procedure Type",
  tender_supplytype_missing_share = "Supply Type",
  tender_publications_firstcallfortenderdate_missing_share = "First Call for Tender Date",
  notice_url_missing_share = "Notice URL",
  source_missing_share = "Source",
  tender_publications_firstdcontractawarddate_missing_share = "First Contract Award Date",
  tender_publications_lastcontractawardurl_missing_share = "Last Contract Award URL",
  buyer_masterid_missing_share = "Buyer Master ID",
  buyer_id_missing_share = "Buyer ID",
  buyer_city_missing_share = "Buyer City",
  buyer_postcode_missing_share = "Buyer Postcode",
  buyer_country_missing_share = "Buyer Country",
  buyer_nuts_missing_share = "Buyer NUTS",
  buyer_name_missing_share = "Buyer Name",
  buyer_buyertype_missing_share = "Buyer Type",
  buyer_mainactivities_missing_share = "Buyer Main Activities",
  tender_addressofimplementation_country_missing_share = "Implementation Country",
  tender_addressofimplementation_nuts_missing_share = "Implementation NUTS",
  bidder_masterid_missing_share = "Bidder Master ID",
  bidder_id_missing_share = "Bidder ID",
  bidder_country_missing_share = "Bidder Country",
  bidder_nuts_missing_share = "Bidder NUTS",
  bidder_name_missing_share = "Bidder Name",
  bid_price_missing_share = "Bid Price",
  bid_priceusd_missing_share = "Bid Price (USD)",
  bid_pricecurrency_missing_share = "Bid Price Currency",
  lot_productcode_missing_share = "Product Code",
  lot_localproductcode_type_missing_share = "Local Product Code Type",
  lot_localproductcode_missing_share = "Local Product Code",
  submp_missing_share = "Submission Period",
  decp_missing_share = "Decision Period",
  is_capital_missing_share = "Capital City Indicator",
  lot_title_missing_share = "Lot Title",
  lot_estimatedpriceusd_missing_share = "Estimated Price (USD)",
  lot_estimatedprice_missing_share = "Estimated Price",
  lot_estimatedpricecurrency_missing_share = "Estimated Price Currency"
)

#' Apply label lookup to variable names
#' 
#' @param vec Character vector of variable names
#' @param lookup Named character vector of labels
#' @return Character vector with labels applied
label_with_lookup <- function(vec, lookup) {
  out <- lookup[vec]
  out[is.na(out)] <- vec[is.na(out)]
  unname(out)
}

#' Procedure type labels for display
procedure_type_labels <- c(
  "OPEN" = "Open (competitive bidding)",
  "RESTRICTED" = "Restricted (limited competition)",
  "NEGOTIATED_WITH_PUBLICATION" = "Negotiated with publication (limited competition)",
  "NEGOTIATED_WITHOUT_PUBLICATION" = "Negotiated without publication (no competition)",
  "NEGOTIATED" = "Negotiated (limited competition)",
  "COMPETITIVE_DIALOG" = "Competitive dialogue (limited competition)",
  "OUTRIGHT_AWARD" = "Outright award (direct purchase)",
  "OTHER" = "Other (special or exceptional procedures)",
  "Missing procedure type" = "Missing procedure type"
)

# ========================================================================
# PART 2A: STANDARD PLOT THEME SETTINGS
# ========================================================================

#' Standard text sizes for all plots
#' These are calibrated for fig.width=14, fig.height=12
PLOT_SIZES <- list(
  base_size = 16,
  title_size = 20,
  subtitle_size = 14,
  axis_title_size = 16,
  axis_text_size = 14,
  legend_title_size = 14,
  legend_text_size = 13,
  geom_text_size = 4,      # For geom_text (percentages in tiles, bar labels)
  geom_text_large = 5,     # For larger labels
  line_size = 1.5,
  point_size = 3
)

#' Apply standard theme to a ggplot object
#' 
#' @param base_size Base font size
#' @return ggplot2 theme
standard_plot_theme <- function(base_size = PLOT_SIZES$base_size) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = PLOT_SIZES$title_size, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = PLOT_SIZES$subtitle_size),
      axis.title = ggplot2::element_text(size = PLOT_SIZES$axis_title_size),
      axis.text = ggplot2::element_text(size = PLOT_SIZES$axis_text_size),
      legend.title = ggplot2::element_text(size = PLOT_SIZES$legend_title_size),
      legend.text = ggplot2::element_text(size = PLOT_SIZES$legend_text_size)
    )
}

# ========================================================================
# PART 3: DATA LOADING AND VALIDATION
# ========================================================================

#' Load data with consistent settings
#' 
#' @param input_path Path to input file
#' @return data.table
load_data <- function(input_path) {
  data <- data.table::fread(
    input = input_path,
    keepLeadingZeros = TRUE,
    encoding = "UTF-8",
    stringsAsFactors = FALSE,
    showProgress = TRUE,
    na.strings = c("", "-", "NA")
  )
  
  # Drop duplicated column names
  dup_cols <- duplicated(names(data))
  if (any(dup_cols)) {
    warning("Dropping ", sum(dup_cols), " duplicated columns")
    data <- data[, !dup_cols, with = FALSE]
  }
  
  return(data)
}

#' Validate required columns exist
#' 
#' @param df Data frame to check
#' @param required_cols Character vector of required column names
#' @param action_name Name of action requiring columns (for messages)
#' @return Logical indicating whether all columns exist
validate_required_columns <- function(df, required_cols, action_name = "this action") {
  missing <- setdiff(required_cols, names(df))
  
  if (length(missing) > 0) {
    message(sprintf(
      "Skipping %s: missing columns [%s]",
      action_name,
      paste(missing, collapse = ", ")
    ))
    return(FALSE)
  }
  TRUE
}

#' Check overall data quality
#' 
#' @param df Data frame to check
#' @param config Configuration list
#' @return List of quality metrics
check_data_quality <- function(df, config) {
  list(
    n_rows = nrow(df),
    n_cols = ncol(df),
    has_buyer_id = "buyer_masterid" %in% names(df),
    has_bidder_id = "bidder_masterid" %in% names(df),
    has_tender_year = "tender_year" %in% names(df),
    has_price = "bid_price" %in% names(df) | "bid_priceusd" %in% names(df),
    year_range = if ("tender_year" %in% names(df)) {
      range(df$tender_year, na.rm = TRUE)
    } else {
      NULL
    },
    n_unique_buyers = if ("buyer_masterid" %in% names(df)) {
      dplyr::n_distinct(df$buyer_masterid, na.rm = TRUE)
    } else {
      NA_integer_
    },
    n_unique_bidders = if ("bidder_masterid" %in% names(df)) {
      dplyr::n_distinct(df$bidder_masterid, na.rm = TRUE)
    } else {
      NA_integer_
    }
  )
}

# ========================================================================
# PART 4: DATA PREPARATION HELPERS
# ========================================================================

#' Add tender year from available date fields
#' 
#' @param df Data frame
#' @return Data frame with tender_year column
add_tender_year <- function(df) {
  df %>%
    dplyr::mutate(
      tender_year = dplyr::coalesce(
        stringr::str_extract(tender_publications_firstcallfortenderdate, "^\\d{4}"),
        stringr::str_extract(tender_awarddecisiondate, "^\\d{4}"),
        stringr::str_extract(tender_biddeadline, "^\\d{4}")
      ),
      tender_year = as.integer(tender_year)
    )
}

#' Add buyer group classification
#' 
#' @param buyer_buyertype Character vector of buyer types
#' @return Factor with buyer groups
add_buyer_group <- function(buyer_buyertype) {
  group <- dplyr::case_when(
    grepl("(?i)national", buyer_buyertype) ~ "National Buyer",
    grepl("(?i)regional", buyer_buyertype) ~ "Regional Buyer",
    grepl("(?i)utilities", buyer_buyertype) ~ "Utilities",
    grepl("(?i)European", buyer_buyertype) ~ "EU agency",
    TRUE ~ "Other Public Bodies"
  )
  
  factor(
    group,
    levels = c(
      "National Buyer",
      "Regional Buyer",
      "Utilities",
      "EU agency",
      "Other Public Bodies"
    )
  )
}

#' Standardize missing values
#' 
#' @param df Data frame
#' @return Data frame with standardized NAs
standardize_missing_values <- function(df) {
  df %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::where(is.character),
        ~ dplyr::na_if(., "")
      )
    )
}

#' Add commonly used derived variables
#' 
#' @param df Data frame
#' @return Data frame with derived variables
add_derived_variables <- function(df) {
  df <- df %>%
    dplyr::mutate(
      has_buyer_id = !is.na(buyer_masterid),
      has_bidder_id = !is.na(bidder_masterid),
      has_price = !is.na(bid_price) | !is.na(bid_priceusd)
    )
  
  # Add buyer group if buyertype exists
  if ("buyer_buyertype" %in% names(df)) {
    df <- df %>%
      dplyr::mutate(buyer_group = add_buyer_group(buyer_buyertype))
  }
  
  df
}

#' Clean NUTS3 codes
#' 
#' @param df Data frame
#' @param nuts_col Column name containing NUTS codes
#' @return Data frame with cleaned nuts3 column
clean_nuts3 <- function(df, nuts_col = buyer_nuts) {
  nuts_quo <- rlang::enquo(nuts_col)
  
  df %>%
    dplyr::mutate(
      buyer_nuts = as.character(!!nuts_quo),
      nuts_clean = buyer_nuts %>%
        stringr::str_replace_all("\\[|\\]", "") %>%
        stringr::str_replace_all('"', "") %>%
        stringr::str_squish(),
      nuts3 = dplyr::if_else(
        stringr::str_detect(nuts_clean, "^[A-Z]{2}[0-9]{3}$"),
        nuts_clean,
        NA_character_
      )
    )
}

#' Prepare data for analysis
#' 
#' @param df Data frame
#' @return Prepared data frame
prepare_data <- function(df) {
  df %>%
    add_tender_year() %>%
    standardize_missing_values() %>%
    add_derived_variables()
}

# ========================================================================
# PART 5: MISSING VALUE ANALYSIS
# ========================================================================

#' Compute missing shares across columns
#' 
#' @param df Data frame
#' @param cols Columns to analyze (tidy-select)
#' @return Data frame with missing share columns
#' Compute missing shares across columns
summarise_missing_shares <- function(df, cols = !dplyr::starts_with("ind_")) {
  df %>%
    dplyr::summarise(
      dplyr::across(
        .cols = {{ cols }} & !ends_with("_missing_share"),  # Remove dplyr:: prefix from ends_with
        .fns = ~ mean(is.na(.)),
        .names = "{.col}_missing_share"
      ),
      .groups = "drop"
    )
}

#' Pivot missing shares to long format
#' 
#' @param df Data frame with missing share columns
#' @param id_vars ID columns to keep
#' @return Long format data frame
pivot_missing_long <- function(df, id_vars = NULL) {
  if (is.null(id_vars)) {
    df %>%
      tidyr::pivot_longer(
        cols = dplyr::everything(),
        names_to = "variable",
        values_to = "missing_share"
      )
  } else {
    df %>%
      tidyr::pivot_longer(
        cols = -dplyr::all_of(id_vars),
        names_to = "variable",
        values_to = "missing_share"
      )
  }
}

#' Compute organization-level missing shares for interoperability
#' 
#' @param df Data frame
#' @return Data frame with organization type and missing shares
compute_org_missing <- function(df) {
  miss_or_na <- function(col) {
    if (col %in% names(df)) {
      mean(is.na(df[[col]]))
    } else {
      NA_real_
    }
  }
  
  tibble::tribble(
    ~organization_type, ~id_type, ~missing_share,
    "Supplier", "Source ID", miss_or_na("bidder_id"),
    "Supplier", "Generated ID", miss_or_na("bidder_masterid"),
    "Supplier", "Name", miss_or_na("bidder_name"),
    "Supplier", "Address (postcode)", miss_or_na("bidder_nuts"),
    "Buyer", "Source ID", miss_or_na("buyer_id"),
    "Buyer", "Generated ID", miss_or_na("buyer_masterid"),
    "Buyer", "Name", miss_or_na("buyer_name"),
    "Buyer", "Address (postcode)", miss_or_na("buyer_postcode")
  )
}

#' Compute missing correlations
#' 
#' @param df Data frame
#' @return Correlation matrix
compute_missing_correlations <- function(df) {
  miss_matrix <- df %>%
    dplyr::select(!dplyr::starts_with("ind_")) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ as.numeric(is.na(.))))
  
  corrr::correlate(miss_matrix)
}

# ========================================================================
# PART 6: PLOTTING FUNCTIONS
# ========================================================================

#' Create plot saver function
#' 
#' @param output_dir Output directory
#' @param config Configuration list
#' @return Function to save plots
create_plot_saver <- function(output_dir, config) {
  function(plot, filename, width = NULL, height = NULL) {
    if (is.null(plot)) return(invisible(NULL))
    
    ggplot2::ggsave(
      filename = file.path(output_dir, paste0(filename, "_", config$country, ".png")),
      plot = plot,
      width = width %||% config$plots$width,
      height = height %||% config$plots$height,
      dpi = config$plots$dpi
    )
    
    invisible(TRUE)
  }
}

#' Plot missing share bar chart
#' 
#' @param data_long Long format data with variable and missing_share
#' @param label_lookup Named vector for labels
#' @param title Plot title
#' @return ggplot object
plot_missing_bar <- function(data_long, label_lookup, title = "Missing Value Share per Variable") {
  ggplot2::ggplot(
    data_long,
    ggplot2::aes(
      x = reorder(variable, missing_share),
      y = missing_share
    )
  ) +
    ggplot2::geom_col(fill = "lightblue") +
    ggplot2::coord_flip() +
    ggplot2::scale_x_discrete(
      labels = function(x) label_with_lookup(x, label_lookup)
    ) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    ggplot2::labs(
      title = title,
      x = "Variable",
      y = "Missing Share"
    ) +
    standard_plot_theme()
}

#' Generic missing share heatmap with dynamic sizing
#'
#' @param data Data frame
#' @param x_var Name of x variable
#' @param y_var Name of y variable
#' @param fill_var Name of fill variable
#' @param title Plot title
#' @param x_lab X-axis label
#' @param y_lab Y-axis label
#' @param height_per_row Height per y-axis item (default 0.4)
#' @param text_size Size of percentage text in cells (default 3.5)
#' @return ggplot object with suggested_height attribute
plot_missing_heatmap <- function(data, x_var, y_var, fill_var,
                                 title, x_lab, y_lab,
                                 height_per_row = 0.3,
                                 text_size = NULL) {
  x_sym <- rlang::sym(x_var)
  y_sym <- rlang::sym(y_var)
  fill_sym <- rlang::sym(fill_var)
  
  # Use standard size or adjust based on data
  if (is.null(text_size)) {
    n_y_values <- length(unique(data[[y_var]]))
    if (n_y_values > 40) {
      text_size <- PLOT_SIZES$geom_text_size - 0.5
    } else if (n_y_values > 30) {
      text_size <- PLOT_SIZES$geom_text_size - 0.2
    } else {
      text_size <- PLOT_SIZES$geom_text_size
    }
  }
  
  p <- ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = !!x_sym,
      y = !!y_sym,
      fill = !!fill_sym
    )
  ) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::geom_text(
      ggplot2::aes(label = scales::percent(!!fill_sym, accuracy = 1)),
      size = text_size,
      color = "black"
    ) +
    ggplot2::scale_fill_gradient(
      name = "Missing share",
      labels = scales::percent_format(accuracy = 1),
      limits = c(0, 1),
      low = "skyblue1",
      high = "indianred1"
    ) +
    ggplot2::labs(
      title = title,
      x = x_lab,
      y = y_lab
    ) +
    standard_plot_theme() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
  
  p
}
#' Plot top N bar chart
#' 
#' @param df Data frame
#' @param x_var Name of x variable
#' @param y_var Name of y variable
#' @param label_var Name of label variable
#' @param title Plot title
#' @param x_lab X-axis label
#' @param y_lab Y-axis label
#' @param fill_color Bar fill color
#' @param y_limit Y-axis limits
#' @param percent Whether to format labels as percent
#' @return ggplot object
plot_top_bar <- function(df, x_var, y_var, label_var,
                         title, x_lab, y_lab,
                         fill_color = "skyblue1",
                         y_limit = c(0, 1.05),
                         percent = TRUE) {
  x_sym <- rlang::sym(x_var)
  y_sym <- rlang::sym(y_var)
  lab_sym <- rlang::sym(label_var)
  
  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(
      x = reorder(!!x_sym, !!y_sym),
      y = !!y_sym
    )
  ) +
    ggplot2::geom_col(fill = fill_color) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = if (percent) {
          scales::percent(!!y_sym, accuracy = 0.1)
        } else {
          !!lab_sym
        }
      ),
      hjust = -0.1,
      size = PLOT_SIZES$geom_text_large,
      check_overlap = TRUE
    ) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = title,
      x = x_lab,
      y = y_lab
    ) +
    ggplot2::theme_bw(base_size = PLOT_SIZES$base_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = PLOT_SIZES$title_size, face = "bold"),
      axis.title = ggplot2::element_text(size = PLOT_SIZES$axis_title_size),
      axis.text = ggplot2::element_text(size = PLOT_SIZES$axis_text_size)
    )
  
  if (!is.null(y_limit)) {
    p <- p + ggplot2::ylim(y_limit)
  }
  
  p
}

#' Plot ggeffects line with confidence band
#' 
#' @param pred ggeffects prediction object
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @param x_lab X-axis label
#' @param y_lab Y-axis label
#' @param caption Plot caption
#' @param wrap_width Width for text wrapping
#' @return ggplot object
plot_ggeffects_line <- function(pred,
                                title,
                                subtitle,
                                x_lab,
                                y_lab,
                                caption = NULL,
                                wrap_width = 110) {
  
  subtitle_w <- if (!is.null(subtitle)) stringr::str_wrap(subtitle, width = wrap_width) else NULL
  caption_w <- if (!is.null(caption)) stringr::str_wrap(caption, width = wrap_width) else NULL
  
  ggplot2::ggplot(pred, ggplot2::aes(x, predicted)) +
    ggplot2::geom_line(size = PLOT_SIZES$line_size, color = "lightblue") +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle_w,
      x = x_lab,
      y = y_lab,
      caption = caption_w
    ) +
    standard_plot_theme() +
    ggplot2::theme(
      plot.subtitle = ggplot2::element_text(size = PLOT_SIZES$subtitle_size, lineheight = 1.05),
      plot.caption = ggplot2::element_text(size = PLOT_SIZES$subtitle_size - 2, lineheight = 1.05),
      plot.margin = ggplot2::margin(10, 18, 10, 10)
    )
}
# ========================================================================
# PART 7: MODEL SPECIFICATION HELPERS
# ========================================================================

#' Build fixed effects formula part
#' 
#' @param fe Fixed effects specification
#' @return Character string for formula
make_fe_part <- function(fe) {
  switch(
    fe,
    "0" = "0",
    "buyer" = "buyer_masterid",
    "year" = "tender_year",
    "buyer+year" = "buyer_masterid + tender_year",
    "buyer#year" = "buyer_masterid^tender_year",
    stop("Unknown FE spec: ", fe)
  )
}

#' Build cluster formula
#' 
#' @param cluster Cluster specification
#' @return Formula or NULL
make_cluster <- function(cluster) {
  switch(
    cluster,
    "none" = NULL,
    "buyer" = stats::as.formula("~ buyer_masterid"),
    "year" = stats::as.formula("~ tender_year"),
    "buyer_year" = stats::as.formula("~ buyer_masterid + tender_year"),
    "buyer_buyertype" = stats::as.formula("~ buyer_masterid + buyer_buyertype"),
    stop("Unknown cluster spec: ", cluster)
  )
}

#' Safe fixest model fitting
#' 
#' @param expr Expression to evaluate
#' @return Model or NULL
safe_fixest <- function(expr) {
  tryCatch(expr, error = function(e) NULL)
}

#' Extract effect from fixest model
#' 
#' @param model fixest model object
#' @param x_name Name of variable to extract
#' @param data_used Data used for estimation
#' @param y_name Name of outcome variable
#' @return List with estimate, pvalue, nobs, std_slope
extract_effect_fixest <- function(model, x_name, data_used, y_name = NULL) {
  s <- summary(model)
  ct <- s$coeftable
  
  if (!(x_name %in% rownames(ct))) {
    return(list(
      estimate = NA_real_,
      pvalue = NA_real_,
      nobs = s$nobs,
      std_slope = NA_real_
    ))
  }
  
  est <- as.numeric(ct[x_name, "Estimate"])
  pv <- as.numeric(ct[x_name, "Pr(>|t|)"])
  if (is.na(pv) && "Pr(>|z|)" %in% colnames(ct)) {
    pv <- as.numeric(ct[x_name, "Pr(>|z|)"])
  }
  
  sx <- stats::sd(data_used[[x_name]], na.rm = TRUE)
  std_slope <- est * sx
  
  list(
    estimate = est,
    pvalue = pv,
    nobs = s$nobs,
    std_slope = std_slope
  )
}

#' Extract effect with specific vcov
#' 
#' @param model fixest model
#' @param x_name Variable name
#' @param data_used Data frame
#' @param vcov VCOV specification
#' @return List with estimate, se, t, p
extract_effect_fixest_vcov <- function(model, x_name, data_used, vcov) {
  ct <- tryCatch(
    fixest::coeftable(model, vcov = vcov),
    error = function(e) NULL
  )
  
  if (is.null(ct) || !(x_name %in% rownames(ct))) {
    return(list(estimate = NA_real_, se = NA_real_, t = NA_real_, p = NA_real_))
  }
  
  est <- as.numeric(ct[x_name, "Estimate"])
  se <- as.numeric(ct[x_name, "Std. Error"])
  
  tv <- if ("t value" %in% colnames(ct)) as.numeric(ct[x_name, "t value"]) else NA_real_
  pv <- if ("Pr(>|t|)" %in% colnames(ct)) as.numeric(ct[x_name, "Pr(>|t|)"]) else
    if ("Pr(>|z|)" %in% colnames(ct)) as.numeric(ct[x_name, "Pr(>|z|)"]) else NA_real_
  
  list(estimate = est, se = se, t = tv, p = pv)
}

#' Compute effect at P10 vs P90
#' 
#' @param model Model object
#' @param data_used Data frame
#' @param x_name Variable name
#' @return Numeric effect size
effect_p10_p90 <- function(model, data_used, x_name) {
  qs <- stats::quantile(data_used[[x_name]], probs = c(.1, .9), na.rm = TRUE)
  x_lo <- unname(qs[1])
  x_hi <- unname(qs[2])
  
  typical <- data_used[1, , drop = FALSE]
  for (nm in names(typical)) {
    if (nm == x_name) next
    v <- data_used[[nm]]
    if (is.numeric(v)) {
      typical[[nm]] <- stats::median(v, na.rm = TRUE)
    } else if (is.factor(v) || is.character(v)) {
      tab <- sort(table(v), decreasing = TRUE)
      typical[[nm]] <- names(tab)[1]
      if (is.factor(v)) typical[[nm]] <- factor(typical[[nm]], levels = levels(v))
    }
  }
  
  d_lo <- typical
  d_hi <- typical
  d_lo[[x_name]] <- x_lo
  d_hi[[x_name]] <- x_hi
  
  p_lo <- suppressWarnings(stats::predict(model, newdata = d_lo, type = "response"))
  p_hi <- suppressWarnings(stats::predict(model, newdata = d_hi, type = "response"))
  
  as.numeric(p_hi - p_lo)
}

#' Get default VCOV menu for robustness checks
#' 
#' @param has_buyertype Whether buyer type variable exists
#' @return List of VCOV specifications
get_default_vcov_menu <- function(has_buyertype = TRUE) {
  out <- list(
    "hetero",
    stats::as.formula("~ buyer_masterid"),
    stats::as.formula("~ tender_year"),
    stats::as.formula("~ buyer_masterid + tender_year")
  )
  if (has_buyertype) {
    out <- c(out, list(stats::as.formula("~ buyer_masterid + buyer_buyertype")))
  }
  out
}

#' Compute robustness summary
#' 
#' @param model Model object
#' @param x_name Variable name
#' @param data_used Data frame
#' @param vcov_list List of VCOV specs
#' @param p_max P-value threshold
#' @return List with robustness metrics
robustness_summary <- function(model, x_name, data_used, vcov_list, p_max = 0.10) {
  res <- lapply(vcov_list, function(v) extract_effect_fixest_vcov(model, x_name, data_used, v))
  
  pvals <- vapply(res, function(z) z$p, numeric(1))
  ests <- vapply(res, function(z) z$estimate, numeric(1))
  
  worst_p <- suppressWarnings(max(pvals, na.rm = TRUE))
  pass_count <- sum(!is.na(pvals) & pvals <= p_max)
  tested_count <- sum(!is.na(pvals))
  pass_share <- if (tested_count > 0) pass_count / tested_count else NA_real_
  
  sign_consistent <- {
    s <- sign(ests[!is.na(ests)])
    length(unique(s)) <= 1
  }
  
  list(
    robust_p_worst = if (is.infinite(worst_p)) NA_real_ else worst_p,
    robust_pass_count = pass_count,
    robust_tested = tested_count,
    robust_pass_share = pass_share,
    robust_sign_stable = sign_consistent
  )
}

# ========================================================================
# PART 8: MODEL SELECTION FUNCTIONS
# ========================================================================

#' Pick best model from specifications
#' 
#' @param results_df Data frame of model results
#' @param require_positive Whether to require positive estimate
#' @param p_max Maximum p-value
#' @param strength_col Column name for effect strength
#' @return Single row data frame or NULL
pick_best_model <- function(results_df,
                            require_positive = TRUE,
                            p_max = 0.10,
                            strength_col = c("effect_strength", "std_slope")) {
  strength_col <- match.arg(strength_col)
  
  df <- results_df
  if (require_positive) df <- df[df$estimate > 0, , drop = FALSE]
  df <- df[!is.na(df$pvalue) & df$pvalue <= p_max, , drop = FALSE]
  df <- df[!is.na(df[[strength_col]]), , drop = FALSE]
  if (nrow(df) == 0) return(NULL)
  
  df <- df[order(df[[strength_col]], decreasing = TRUE), , drop = FALSE]
  df[["rank"]] <- seq_len(nrow(df))
  df[1, , drop = FALSE]
}

#' Pick most robust model
#' 
#' @param results_df Data frame with robustness metrics
#' @param require_positive Require positive estimate
#' @param p_max P-value threshold
#' @param strength_col Column for effect strength
#' @return Single row or NULL
pick_most_robust_model <- function(results_df,
                                   require_positive = TRUE,
                                   p_max = 0.10,
                                   strength_col = c("effect_strength", "std_slope")) {
  strength_col <- match.arg(strength_col)
  
  df <- results_df
  if (require_positive) df <- df[df$estimate > 0, , drop = FALSE]
  
  df <- df[!is.na(df$robust_p_worst), , drop = FALSE]
  df <- df[df$robust_p_worst <= p_max, , drop = FALSE]
  df <- df[!is.na(df[[strength_col]]), , drop = FALSE]
  
  if (nrow(df) == 0) return(NULL)
  
  df <- df[order(
    -df$robust_pass_share,
    df$robust_p_worst,
    -df[[strength_col]]
  ), , drop = FALSE]
  
  df[1, , drop = FALSE]
}

#' Model diagnostics
#' 
#' @param specs_df Specification results
#' @param require_positive Require positive estimate
#' @param p_max P-value threshold
#' @param strength_col Strength column name
#' @return List of diagnostic counts
model_diagnostics <- function(specs_df, require_positive = TRUE, p_max = 0.10, strength_col) {
  if (is.null(specs_df) || nrow(specs_df) == 0L) {
    return(list(total = 0L, after_sign = 0L, after_p = 0L, after_strength = 0L))
  }
  
  df <- specs_df
  total <- nrow(df)
  
  df1 <- if (require_positive) df[df$estimate > 0, , drop = FALSE] else df
  after_sign <- nrow(df1)
  
  df2 <- df1[!is.na(df1$pvalue) & df1$pvalue <= p_max, , drop = FALSE]
  after_p <- nrow(df2)
  
  ok_strength <- !is.na(df2[[strength_col]])
  df3 <- df2[ok_strength, , drop = FALSE]
  after_strength <- nrow(df3)
  
  list(total = total, after_sign = after_sign, after_p = after_p, after_strength = after_strength)
}

# ========================================================================
# PART 9: SENSITIVITY ANALYSIS FUNCTIONS
# ========================================================================

#' Add strength column to specifications
#' 
#' @param specs Specifications data frame
#' @return Data frame with strength column
add_strength_column <- function(specs) {
  if (is.null(specs) || nrow(specs) == 0L) return(specs)
  
  if ("effect_strength" %in% names(specs)) {
    specs$strength <- specs$effect_strength
  } else if ("std_slope" %in% names(specs)) {
    specs$strength <- specs$std_slope
  } else {
    specs$strength <- NA_real_
  }
  specs
}

#' Summarize sensitivity overall
#' 
#' @param specs Specifications data frame
#' @param p_levels P-value levels to check
#' @return Summary tibble
summarise_sensitivity_overall <- function(specs, p_levels = c(0.05, 0.10, 0.20)) {
  if (is.null(specs) || nrow(specs) == 0L) return(tibble::tibble())
  specs <- add_strength_column(specs)
  
  tibble::tibble(
    n_specs = nrow(specs),
    share_positive = mean(specs$estimate > 0, na.rm = TRUE),
    share_negative = mean(specs$estimate < 0, na.rm = TRUE),
    median_estimate = median(specs$estimate, na.rm = TRUE),
    median_pvalue = median(specs$pvalue, na.rm = TRUE),
    median_strength = median(specs$strength, na.rm = TRUE),
    !!!setNames(
      lapply(p_levels, function(p) mean(specs$pvalue <= p, na.rm = TRUE)),
      paste0("share_p_le_", p_levels)
    )
  )
}

#' Summarize sign instability
#' 
#' @param specs Specifications data frame
#' @return Summary tibble
summarise_sign_instability <- function(specs) {
  if (is.null(specs) || nrow(specs) == 0L) return(tibble::tibble())
  s <- sign(specs$estimate)
  s <- s[!is.na(s) & s != 0]
  tibble::tibble(
    share_sign_stable = if (length(s) == 0) NA_real_ else as.numeric(length(unique(s)) <= 1),
    n_nonzero = length(s)
  )
}

#' Summarize by fixed effects
#' 
#' @param specs Specifications data frame
#' @return Summary tibble
summarise_by_fe <- function(specs) {
  if (is.null(specs) || nrow(specs) == 0L) return(tibble::tibble())
  specs <- add_strength_column(specs)
  
  specs %>%
    dplyr::group_by(fe) %>%
    dplyr::summarise(
      n_specs = dplyr::n(),
      share_positive = mean(estimate > 0, na.rm = TRUE),
      share_p10 = mean(pvalue <= 0.10, na.rm = TRUE),
      median_p = median(pvalue, na.rm = TRUE),
      median_strength = median(strength, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(share_p10))
}

#' Summarize by cluster
#' 
#' @param specs Specifications data frame
#' @return Summary tibble
summarise_by_cluster <- function(specs) {
  if (is.null(specs) || nrow(specs) == 0L) return(tibble::tibble())
  specs <- add_strength_column(specs)
  
  specs %>%
    dplyr::group_by(cluster) %>%
    dplyr::summarise(
      n_specs = dplyr::n(),
      share_positive = mean(estimate > 0, na.rm = TRUE),
      share_p10 = mean(pvalue <= 0.10, na.rm = TRUE),
      median_p = median(pvalue, na.rm = TRUE),
      median_strength = median(strength, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(median_p)
}

#' Summarize by controls
#' 
#' @param specs Specifications data frame
#' @return Summary tibble
summarise_by_controls <- function(specs) {
  if (is.null(specs) || nrow(specs) == 0L) return(tibble::tibble())
  specs <- add_strength_column(specs)
  
  specs %>%
    dplyr::group_by(controls) %>%
    dplyr::summarise(
      n_specs = dplyr::n(),
      share_positive = mean(estimate > 0, na.rm = TRUE),
      share_p10 = mean(pvalue <= 0.10, na.rm = TRUE),
      median_p = median(pvalue, na.rm = TRUE),
      median_strength = median(strength, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(share_p10))
}

#' Classify specifications
#' 
#' @param specs Specifications data frame
#' @param p_cut P-value cutoff
#' @return Classification tibble
classify_specs <- function(specs, p_cut = 0.10) {
  if (is.null(specs) || nrow(specs) == 0L) return(tibble::tibble())
  specs %>%
    dplyr::mutate(
      class = dplyr::case_when(
        estimate > 0 & pvalue <= p_cut ~ "Positive & significant",
        estimate > 0 ~ "Positive but insignificant",
        estimate < 0 & pvalue <= p_cut ~ "Negative & significant",
        estimate < 0 ~ "Negative but insignificant",
        TRUE ~ "Missing/NA"
      )
    ) %>%
    dplyr::count(class) %>%
    dplyr::mutate(share = n / sum(n))
}

#' Find top specification cells
#' 
#' @param specs Specifications data frame
#' @param p_cut P-value cutoff
#' @param n_top Number of top cells
#' @return Top cells tibble
top_cells <- function(specs, p_cut = 0.10, n_top = 10) {
  if (is.null(specs) || nrow(specs) == 0L) return(tibble::tibble())
  specs %>%
    dplyr::mutate(p_ok = pvalue <= p_cut) %>%
    dplyr::group_by(fe, cluster, controls) %>%
    dplyr::summarise(
      n = dplyr::n(),
      share_pok = mean(p_ok, na.rm = TRUE),
      median_p = median(pvalue, na.rm = TRUE),
      median_est = median(estimate, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(share_pok), median_p) %>%
    dplyr::slice_head(n = n_top)
}

#' Build sensitivity bundle
#' 
#' @param specs Specifications data frame
#' @return List of sensitivity summaries
build_sensitivity_bundle <- function(specs) {
  if (is.null(specs) || nrow(specs) == 0L) return(list())
  specs <- add_strength_column(specs)
  
  list(
    overall = summarise_sensitivity_overall(specs),
    sign = summarise_sign_instability(specs),
    by_fe = summarise_by_fe(specs),
    by_cluster = summarise_by_cluster(specs),
    by_controls = summarise_by_controls(specs),
    classes = classify_specs(specs),
    top_cells = top_cells(specs)
  )
}

# ========================================================================
# PART 10: DISPLAY HELPERS
# ========================================================================

#' Pretty model name
#' 
#' @param model_type Model type string
#' @return Readable name
pretty_model_name <- function(model_type) {
  switch(
    model_type,
    "fractional_logit" = "Fractional Logit (quasi-binomial, logit link)",
    "ols_level" = "OLS (levels)",
    "ols_log" = "OLS (log-transformed outcome)",
    "gamma_log" = "Gamma GLM (log link)",
    model_type
  )
}

#' Pretty controls label
#' 
#' @param ctrl Controls specification
#' @return Readable label
pretty_controls_label <- function(ctrl) {
  switch(
    ctrl,
    "x_only" = "No controls",
    "base" = "Core controls",
    "base_extra" = "Core + extra controls",
    ctrl
  )
}

#' Pretty FE label
#' 
#' @param fe Fixed effects specification
#' @return Readable label
pretty_fe_label <- function(fe) {
  switch(
    fe,
    "0" = "No fixed effects",
    "buyer" = "Buyer FE",
    "year" = "Year FE",
    "buyer+year" = "Buyer + Year FE",
    "buyer#year" = "Buyer×Year FE",
    fe
  )
}

#' Controls note for captions
#' 
#' @param ctrl Controls specification
#' @return Note string
controls_note <- function(ctrl) {
  switch(
    ctrl,
    "x_only" = "Controls: none (missingness only).",
    "base" = "Controls: log(contract value), buyer type, procedure type.",
    "base_extra" = "Controls: log(contracts), log(avg contract value), log(total value), buyer type.",
    paste0("Controls: ", ctrl, ".")
  )
}

#' FE counts note
#' 
#' @param data Estimation data
#' @param fe Fixed effects specification
#' @return Note string
fe_counts_note <- function(data, fe) {
  if (is.null(data) || nrow(data) == 0) return("FE counts: N/A")
  
  if (fe == "buyer") {
    return(paste0("FE groups: buyers=", dplyr::n_distinct(data$buyer_masterid)))
  }
  if (fe == "year") {
    return(paste0("FE groups: years=", dplyr::n_distinct(data$tender_year)))
  }
  if (fe == "buyer+year") {
    return(paste0(
      "FE groups: buyers=", dplyr::n_distinct(data$buyer_masterid),
      ", years=", dplyr::n_distinct(data$tender_year)
    ))
  }
  if (fe == "buyer#year") {
    return(paste0(
      "FE groups: buyer×year=",
      dplyr::n_distinct(paste(data$buyer_masterid, data$tender_year, sep = "_"))
    ))
  }
  "FE counts: N/A"
}

# ========================================================================
# PART 11: MODULE - MISSING VALUE ANALYSIS (with dynamic heights)
# ========================================================================

#' Analyze missing values
#' 
#' @param df Data frame
#' @param config Configuration list
#' @param output_dir Output directory
#' @return List of results
analyze_missing_values <- function(df, config, output_dir) {
  results <- list()
  save_plot <- create_plot_saver(output_dir, config)
  
  # Overall missing shares
  missing_shares <- df %>%
    summarise_missing_shares(cols = !dplyr::starts_with("ind_"))
  
  results$overall <- missing_shares
  results$overall_long <- pivot_missing_long(missing_shares)
  
  results$overall_plot <- plot_missing_bar(
    data_long = results$overall_long,
    label_lookup = label_lookup,
    title = paste("Missing Value Share per Variable –", config$country)
  )
  save_plot(results$overall_plot, "missing_shares", width = 10, height = 8)  # FIXED HEIGHT
  
  # By buyer type
  if (validate_required_columns(df, "buyer_buyertype", "missing by buyer type")) {
    results$by_buyer <- df %>%
      dplyr::mutate(buyer_group = add_buyer_group(buyer_buyertype)) %>%
      dplyr::group_by(buyer_group) %>%
      summarise_missing_shares(cols = -dplyr::starts_with("ind_")) %>%
      pivot_missing_long(id_vars = "buyer_group") %>%
      dplyr::mutate(variable_label = label_with_lookup(variable, label_lookup))
    
    results$by_buyer_plot <- plot_missing_heatmap(
      data = results$by_buyer,
      x_var = "buyer_group",
      y_var = "variable_label",
      fill_var = "missing_share",
      title = paste("Missing value share per variable by buyer type –", config$country),
      x_lab = "Buyer type",
      y_lab = "Variable",
      height_per_row = 0.35,
      text_size = 3
    )
  }
  
  # Top buyers with highest missing share
  # Top buyers with highest missing share
  if (validate_required_columns(df, c("buyer_masterid", "buyer_buyertype"), "top missing buyers")) {
    results$top_buyers_missing <- df %>%
      dplyr::select(!dplyr::starts_with("ind_")) %>%
      dplyr::group_by(buyer_masterid, buyer_buyertype) %>%
      dplyr::filter(dplyr::n() >= config$thresholds$min_buyer_contracts) %>%
      dplyr::summarise(
        missing_share = mean(is.na(dplyr::cur_data_all()), na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::arrange(dplyr::desc(missing_share)) %>%
      dplyr::slice_head(n = config$thresholds$top_n_buyers) %>%
      dplyr::mutate(
        # ADDED: Apply buyer group transformation
        buyer_group = add_buyer_group(buyer_buyertype),
        buyer_group_label = as.character(buyer_group),
        # UPDATED: Use buyer_group_label instead of buyer_buyertype
        buyer_label = paste0(
          buyer_masterid, "\n",
          buyer_group_label  # CHANGED from buyer_buyertype
        ),
        buyer_label_short = dplyr::if_else(
          nchar(buyer_masterid) > 20,
          paste0(substr(buyer_masterid, 1, 20), "…\n", buyer_group_label),  # CHANGED
          buyer_label
        )
      )
    
    results$top_buyers_plot <- plot_top_bar(
      df = results$top_buyers_missing,
      x_var = "buyer_label_short",
      y_var = "missing_share",
      label_var = "missing_share",
      title = paste("Top buyers with highest overall missing share –", config$country),
      x_lab = "Buyer",
      y_lab = "Missing share",
      fill_color = "skyblue1",
      y_limit = c(0, 1.05),
      percent = TRUE
    )
  }
  
  # Combined buyers plot - USE FIXED LARGE HEIGHT
  if (!is.null(results$by_buyer_plot) && !is.null(results$top_buyers_plot)) {
    results$combined_buyers_plot <- results$by_buyer_plot + results$top_buyers_plot +
      patchwork::plot_layout(nrow = 2)
    
    save_plot(
      results$combined_buyers_plot,
      "buyers_missing",
      width = 12,
      height = 16  # FIXED LARGE HEIGHT (increased from 12)
    )
  }
  
  # By procedure type - USE FIXED HEIGHT
  if (validate_required_columns(df, "tender_proceduretype", "missing by procedure type")) {
    results$by_procedure <- df %>%
      dplyr::mutate(
        proc_group = ifelse(
          is.na(tender_proceduretype),
          "Missing procedure type",
          as.character(tender_proceduretype)
        )
      ) %>%
      dplyr::group_by(proc_group) %>%
      summarise_missing_shares(cols = -dplyr::starts_with("ind_")) %>%
      pivot_missing_long(id_vars = "proc_group") %>%
      dplyr::mutate(
        proc_group_label = ifelse(
          proc_group %in% names(procedure_type_labels),
          procedure_type_labels[proc_group],
          proc_group
        ),
        variable_label = label_with_lookup(variable, label_lookup),
        proc_group_label = factor(
          proc_group_label,
          levels = c(
            "Open (competitive bidding)",
            "Restricted (limited competition)",
            "Negotiated with publication (limited competition)",
            "Negotiated without publication (no competition)",
            "Negotiated (limited competition)",
            "Competitive dialogue (limited competition)",
            "Outright award (direct purchase)",
            "Other (special or exceptional procedures)",
            "Missing procedure type"
          )
        )
      )
    
    results$by_procedure_plot <- plot_missing_heatmap(
      data = results$by_procedure,
      x_var = "proc_group_label",
      y_var = "variable_label",
      fill_var = "missing_share",
      title = paste("Missing value share per variable by procedure type –", config$country),
      x_lab = "Procedure Type",
      y_lab = "Variable",
      height_per_row = 0.35,
      text_size = 3
    )
    
    save_plot(results$by_procedure_plot, "missing_by_proc", width = 12, height = 16)  # FIXED LARGE HEIGHT
  }
  
  # Missing correlations
  results$correlations <- compute_missing_correlations(df)
  
  miss_corr_long <- results$correlations %>%
    corrr::stretch(na.rm = TRUE)
  
  results$correlation_plot <- ggplot2::ggplot(
    miss_corr_long,
    ggplot2::aes(x = x, y = y, fill = r)
  ) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient2(
      low = "blue",
      mid = "white",
      high = "red",
      midpoint = 0,
      limits = c(-1, 1)
    ) +
    ggplot2::coord_equal() +
    ggplot2::labs(
      title = paste("Correlation of NAs across variables –", config$country),
      x = "Variable",
      y = "Variable",
      fill = "Correlation"
    ) +
    standard_plot_theme() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)
    )
  
  save_plot(results$correlation_plot, "missing_corr", width = 10, height = 8)  # FIXED
  
  results$correlation_top <- miss_corr_long %>%
    dplyr::filter(x != y) %>%
    dplyr::arrange(dplyr::desc(abs(r))) %>%
    dplyr::slice_head(n = 20)
  
  # Missing by year
  if (validate_required_columns(df, "tender_year", "missing by year")) {
    results$by_year <- df %>%
      dplyr::filter(!is.na(tender_year)) %>%
      dplyr::group_by(tender_year) %>%
      summarise_missing_shares(cols = !dplyr::starts_with("ind_")) %>%
      pivot_missing_long(id_vars = "tender_year") %>%
      dplyr::mutate(variable_label = label_with_lookup(variable, label_lookup))
    
    top_vars <- results$by_year %>%
      dplyr::group_by(variable_label) %>%
      dplyr::summarise(avg_missing = mean(missing_share, na.rm = TRUE), .groups = "drop") %>%
      dplyr::slice_max(avg_missing, n = config$thresholds$top_n_vars) %>%
      dplyr::pull(variable_label)
    
    results$by_year_plot <- results$by_year %>%
      dplyr::filter(variable_label %in% top_vars) %>%
      ggplot2::ggplot(ggplot2::aes(x = tender_year, y = missing_share, color = variable_label)) +
      ggplot2::geom_line(size = PLOT_SIZES$line_size) +
      ggplot2::geom_point(size = PLOT_SIZES$point_size) +
      ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      ggplot2::labs(
        title = paste("Trends in missing shares over time (top variables) –", config$country),
        x = "Year",
        y = "Missing share",
        color = "Variable"
      ) +
      standard_plot_theme() +
      ggplot2::theme(
        legend.position = "bottom",
        legend.key.height = ggplot2::unit(0.5, "cm")
      ) +
      ggplot2::guides(color = ggplot2::guide_legend(ncol = 2))
    
    save_plot(results$by_year_plot, "year_miss", width = 10, height = 6)  # FIXED
  }
  
  results
}
# ========================================================================
# PART 12: MODULE - INTEROPERABILITY ANALYSIS
# ========================================================================

#' Analyze interoperability (organization-level matching)
#' 
#' @param df Data frame
#' @param config Configuration list
#' @param output_dir Output directory
#' @return List of results
analyze_interoperability <- function(df, config, output_dir) {
  results <- list()
  
  results$org_missing <- compute_org_missing(df)
  
  results
}

# ========================================================================
# PART 13: MODULE - COMPETITION ANALYSIS
# ========================================================================

#' Build CPV cluster data frame
#' 
#' @param df Data frame
#' @param cpv_digits Number of CPV digits
#' @return Data frame with CPV clusters
build_cpv_df <- function(df, cpv_digits = 3) {
  df %>%
    dplyr::select(
      tender_id,
      lot_number,
      bidder_id,
      lot_productcode,
      bid_priceusd
    ) %>%
    dplyr::mutate(
      cpv_cluster = stringr::str_sub(lot_productcode, 1, cpv_digits)
    )
}

#' Analyze buyer-supplier concentration
#' 
#' @param df Data frame
#' @param config Configuration list
#' @return List of concentration data and plots
analyze_buyer_supplier_concentration <- function(df, config) {
  results <- list()
  
  if (!validate_required_columns(
    df,
    c("buyer_masterid", "bidder_masterid", "bid_priceusd"),
    "buyer-supplier concentration"
  )) {
    return(results)
  }
  
  results$data <- df %>%
    dplyr::group_by(tender_year, buyer_masterid, bidder_masterid) %>%
    dplyr::summarise(
      n_contracts = dplyr::n(),
      total_spend = sum(bid_priceusd, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::group_by(tender_year, buyer_masterid) %>%
    dplyr::mutate(
      buyer_total_spend = sum(total_spend, na.rm = TRUE),
      buyer_total_contract = dplyr::n(),
      buyer_concentration = ifelse(
        buyer_total_spend > 0,
        total_spend / buyer_total_spend,
        NA_real_
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(tender_year, buyer_masterid) %>%
    dplyr::mutate(
      n_suppliers = dplyr::n(),
      buyer_concentration_display = ifelse(
        n_suppliers < config$thresholds$min_suppliers_for_buyer_conc,
        NA_real_,
        buyer_concentration
      )
    ) %>%
    dplyr::ungroup() %>%
    tidyr::drop_na()
  
  # Overall concentration
  top_buyers <- results$data %>%
    dplyr::group_by(buyer_masterid) %>%
    dplyr::summarise(
      max_conc = max(buyer_concentration_display, na.rm = TRUE),
      total_contracts = sum(n_contracts, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::slice_max(max_conc, n = config$thresholds$top_n_buyers)
  
  results$overall_plot <- plot_top_bar(
    df = top_buyers,
    x_var = "buyer_masterid",
    y_var = "max_conc",
    label_var = "total_contracts",
    title = paste("Top Buyers by Maximum Supplier Concentration –", config$country),
    x_lab = "Buyer ID",
    y_lab = "Max share of spending to one supplier",
    fill_color = "skyblue1",
    y_limit = c(0, 1.05),
    percent = FALSE
  )
  
  # Yearly concentration
  top_buyers_yearly <- results$data %>%
    dplyr::filter(tender_year > 2014) %>%
    dplyr::group_by(tender_year, buyer_masterid) %>%
    dplyr::summarise(
      max_conc = max(buyer_concentration_display, na.rm = TRUE),
      total_contracts = sum(n_contracts, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::group_by(tender_year) %>%
    dplyr::slice_max(max_conc, n = 30) %>%
    dplyr::ungroup()
  
  repeated_buyers <- top_buyers_yearly %>%
    dplyr::count(buyer_masterid) %>%
    dplyr::filter(n > 1) %>%
    dplyr::transmute(buyer_masterid, repeated = TRUE)
  
  top_buyers_yearly <- top_buyers_yearly %>%
    dplyr::left_join(repeated_buyers, by = "buyer_masterid") %>%
    dplyr::mutate(repeated = dplyr::if_else(is.na(repeated), FALSE, repeated))
  
  results$yearly_plot <- ggplot2::ggplot(
    top_buyers_yearly,
    ggplot2::aes(
      x = tidytext::reorder_within(buyer_masterid, max_conc, tender_year),
      y = max_conc,
      fill = repeated
    )
  ) +
    ggplot2::geom_col() +
    ggplot2::geom_text(
      ggplot2::aes(label = total_contracts, color = repeated),
      hjust = -0.1,
      size = PLOT_SIZES$geom_text_size - 0.5,
      show.legend = FALSE
    ) +
    ggplot2::coord_flip() +
    ggplot2::facet_wrap(~ tender_year, scales = "free_y") +
    tidytext::scale_x_reordered(labels = function(x) substr(x, 1, 8)) +
    ggplot2::scale_fill_manual(
      values = c(`FALSE` = "skyblue1", `TRUE` = "red"),
      name = "Buyer appears\nin multiple years",
      labels = c("No", "Yes")
    ) +
    ggplot2::scale_color_manual(values = c(`FALSE` = "black", `TRUE` = "red4")) +
    ggplot2::labs(
      title = paste("Top 30 Buyers by Maximum Spending Concentration per Year –", config$country),
      subtitle = "Red bars = buyers that appear in the top list in multiple years",
      x = "Buyer ID",
      y = "Max share of spending to one supplier"
    ) +
    ggplot2::theme_bw(base_size = PLOT_SIZES$base_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = PLOT_SIZES$title_size, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = PLOT_SIZES$subtitle_size),
      axis.text = ggplot2::element_text(size = PLOT_SIZES$axis_text_size - 2),
      axis.title = ggplot2::element_text(size = PLOT_SIZES$axis_title_size),
      strip.text = ggplot2::element_text(size = PLOT_SIZES$subtitle_size),
      legend.text = ggplot2::element_text(size = PLOT_SIZES$legend_text_size),
      legend.title = ggplot2::element_text(size = PLOT_SIZES$legend_title_size)
    ) +
    ggplot2::ylim(0, 1.05)
  
  results
}

#' Run single-bidding specifications
#' 
#' @param buyer_analysis_fe Buyer analysis data with FE-eligible buyers
#' @param config Configuration list
#' @return Data frame of specification results
run_singleb_specs <- function(buyer_analysis_fe, config) {
  out <- list()
  k <- 0L
  
  for (fe in config$models$fe_set) {
    fe_part <- make_fe_part(fe)
    
    for (cl in config$models$cluster_set) {
      cl_fml <- make_cluster(cl)
      
      for (ctrl in config$models$controls_set) {
        rhs_terms <- switch(
          ctrl,
          "x_only" = c("cumulative_missing_share"),
          "base" = c("cumulative_missing_share", "log1p(n_contracts)", "log1p(avg_contract_value)"),
          "base_extra" = c("cumulative_missing_share", "log1p(n_contracts)", "log1p(avg_contract_value)",
                           "log1p(total_contract_value)", "buyer_buyertype")
        )
        
        rhs_terms <- rhs_terms[rhs_terms %in% names(buyer_analysis_fe)]
        rhs <- paste(rhs_terms, collapse = " + ")
        fml <- stats::as.formula(paste0("cumulative_singleb_rate ~ ", rhs, " | ", fe_part))
        
        m <- safe_fixest(
          fixest::feglm(
            fml,
            family = quasibinomial(link = "logit"),
            data = buyer_analysis_fe,
            cluster = cl_fml
          )
        )
        
        if (is.null(m)) next
        
        eff <- extract_effect_fixest(
          model = m,
          x_name = "cumulative_missing_share",
          data_used = buyer_analysis_fe
        )
        
        eff_strength <- safe_fixest(effect_p10_p90(m, buyer_analysis_fe, "cumulative_missing_share"))
        if (is.null(eff_strength)) eff_strength <- NA_real_
        
        k <- k + 1L
        out[[k]] <- data.frame(
          outcome = "singleb",
          model_type = "fractional_logit",
          fe = fe,
          cluster = cl,
          controls = ctrl,
          estimate = eff$estimate,
          pvalue = eff$pvalue,
          nobs = eff$nobs,
          std_slope = eff$std_slope,
          effect_strength = eff_strength,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  
  if (length(out) == 0) return(data.frame())
  do.call(rbind, out)
}

#' Analyze single bidding
#' 
#' @param df Data frame
#' @param config Configuration list
#' @return List of results
analyze_singleb <- function(df, config) {
  results <- list()
  
  if (!validate_required_columns(df, "ind_corr_singleb", "single-bidding analysis")) {
    return(results)
  }
  
  # Scale to 0-1
  df <- df %>%
    dplyr::mutate(
      ind_corr_singleb = dplyr::if_else(
        !is.na(ind_corr_singleb),
        ind_corr_singleb / 100,
        NA_real_
      )
    )
  
  # Filter by year
  yr <- config$years_singleb
  df_filtered <- df %>%
    dplyr::filter(
      !is.na(tender_year),
      tender_year >= yr$min_year,
      tender_year <= yr$max_year
    )
  
  if (nrow(df_filtered) == 0L) {
    message("No observations after year filter for single-bidding analysis")
    return(results)
  }
  
  # Compute buyer-level aggregates
  buyer_missing_share <- df_filtered %>%
    dplyr::group_by(buyer_masterid, tender_year) %>%
    summarise_missing_shares(cols = !dplyr::starts_with("ind_")) %>%
    dplyr::mutate(
      cumulative_missing_share = rowMeans(
        dplyr::across(dplyr::ends_with("_missing_share")),
        na.rm = TRUE
      )
    )
  
  buyer_singleb_rate <- df_filtered %>%
    dplyr::group_by(buyer_masterid, tender_year) %>%
    dplyr::summarise(
      cumulative_singleb_rate = mean(ind_corr_singleb, na.rm = TRUE),
      .groups = "drop"
    )
  
  buyer_controls <- df_filtered %>%
    dplyr::group_by(buyer_masterid, tender_year, buyer_buyertype) %>%
    dplyr::summarise(
      n_contracts = dplyr::n(),
      avg_contract_value = mean(bid_priceusd, na.rm = TRUE),
      total_contract_value = sum(bid_priceusd, na.rm = TRUE),
      .groups = "drop"
    )
  
  buyer_analysis <- buyer_missing_share %>%
    dplyr::select(buyer_masterid, tender_year, cumulative_missing_share) %>%
    dplyr::inner_join(buyer_singleb_rate, by = c("buyer_masterid", "tender_year")) %>%
    dplyr::inner_join(buyer_controls, by = c("buyer_masterid", "tender_year")) %>%
    dplyr::mutate(
      buyer_buyertype = forcats::fct_explicit_na(as.factor(buyer_buyertype), "Unknown")
    )
  
  # Filter to buyers with sufficient history
  buyer_year_span <- buyer_analysis %>%
    dplyr::group_by(buyer_masterid) %>%
    dplyr::summarise(n_years = dplyr::n_distinct(tender_year), .groups = "drop")
  
  eligible_buyers <- buyer_year_span %>%
    dplyr::filter(n_years >= config$thresholds$min_buyer_years) %>%
    dplyr::pull(buyer_masterid)
  
  buyer_analysis_fe <- buyer_analysis %>%
    dplyr::filter(buyer_masterid %in% eligible_buyers)
  
  if (nrow(buyer_analysis_fe) == 0L) {
    message("No buyers with sufficient years of data for single-bidding analysis")
    return(results)
  }
  
  results$data <- buyer_analysis_fe
  results$specs <- run_singleb_specs(buyer_analysis_fe, config)
  results$singleb_sensitivity <- build_sensitivity_bundle(results$specs)
  
  results
}

#' Analyze competition
#' 
#' @param df Data frame
#' @param config Configuration list
#' @param output_dir Output directory
#' @return List of results
analyze_competition <- function(df, config, output_dir) {
  results <- list()
  save_plot <- create_plot_saver(output_dir, config)
  
  # Buyer-supplier concentration
  conc <- analyze_buyer_supplier_concentration(df, config)
  if (length(conc) > 0) {
    results$concentration <- conc$data
    results$concentration_overall_plot <- conc$overall_plot
    results$concentration_yearly_plot <- conc$yearly_plot
    
    save_plot(conc$overall_plot, "bc_overall")
    save_plot(conc$yearly_plot, "bc_yearly")
  }
  
  # Single bidding
  singleb <- analyze_singleb(df, config)
  if (length(singleb) > 0) {
    results$singleb_data <- singleb$data
    results$singleb_specs <- singleb$specs
    results$singleb_sensitivity <- singleb$singleb_sensitivity  # FIXED!
    
    # Print sensitivity
    if (!is.null(singleb$singleb_sensitivity)) {
      message("\n--- SINGLEB sensitivity (", config$country, ") ---")
      print(singleb$singleb_sensitivity$overall)
      print(singleb$singleb_sensitivity$sign)
      print(singleb$singleb_sensitivity$by_fe)
      print(singleb$singleb_sensitivity$by_cluster)
      print(singleb$singleb_sensitivity$by_controls)
      print(singleb$singleb_sensitivity$classes)
      print(singleb$singleb_sensitivity$top_cells)
    }
    
    # Select and plot best model
    if (!is.null(singleb$specs) && nrow(singleb$specs) > 0) {
      best_row <- pick_best_model(
        singleb$specs,
        require_positive = TRUE,
        p_max = config$models$p_max,
        strength_col = "effect_strength"
      )
      
      if (!is.null(best_row)) {
        # Refit model
        fe_part <- make_fe_part(best_row$fe)
        cl_fml <- make_cluster(best_row$cluster)
        
        rhs_terms <- switch(
          best_row$controls,
          "x_only" = c("cumulative_missing_share"),
          "base" = c("cumulative_missing_share", "log1p(n_contracts)", "log1p(avg_contract_value)"),
          "base_extra" = c("cumulative_missing_share", "log1p(n_contracts)", "log1p(avg_contract_value)",
                           "log1p(total_contract_value)", "buyer_buyertype")
        )
        rhs <- paste(rhs_terms, collapse = " + ")
        fml <- stats::as.formula(paste0("cumulative_singleb_rate ~ ", rhs, " | ", fe_part))
        
        model <- fixest::feglm(
          fml,
          family = quasibinomial(link = "logit"),
          data = singleb$data,
          cluster = cl_fml
        )
        
        pred <- tryCatch(
          ggeffects::ggpredict(model, terms = "cumulative_missing_share"),
          error = function(e) NULL
        )
        
        if (!is.null(pred)) {
          years_used <- range(singleb$data$tender_year, na.rm = TRUE)
          
          results$singleb_plot <- plot_ggeffects_line(
            pred = pred,
            title = paste("Predicted Single-Bidding by Missing Share –", config$country),
            subtitle = paste0(
              "(BEST) Model: ", pretty_model_name(best_row$model_type),
              " | Years: ", years_used[1], "–", years_used[2],
              " | N=", best_row$nobs,
              " | FE: ", pretty_fe_label(best_row$fe),
              " | Cluster: ", best_row$cluster,
              " | ", fe_counts_note(singleb$data, best_row$fe)
            ),
            x_lab = "Buyer Missing Share",
            y_lab = "Predicted Single-Bidding Share",
            caption = paste0(
              "Filters: tender_year in [", config$years_singleb$min_year, ", ",
              config$years_singleb$max_year,
              "]; buyers with ≥", config$thresholds$min_buyer_years, " years of data. ",
              controls_note(best_row$controls)
            )
          )
          
          save_plot(results$singleb_plot, "reg_singleb_missing_share")
        }
      }
    }
  }
  
  results
}

# ========================================================================
# PART 14: MODULE - MARKET ANALYSIS
# ========================================================================

#' Detect unusual market entries
#' 
#' @param cpv_data CPV cluster data
#' @param config Configuration list
#' @return Data frame of unusual entries
detect_unusual_entries <- function(cpv_data, config) {
  # Bidder-CPV statistics
  bidder_cpv_stats <- cpv_data %>%
    dplyr::group_by(bidder_id, cpv_cluster) %>%
    dplyr::summarise(
      n_awards = dplyr::n(),
      total_value = sum(bid_priceusd, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::group_by(bidder_id) %>%
    dplyr::mutate(
      bidder_total_awards = sum(n_awards),
      bidder_total_value = sum(total_value),
      share_awards = n_awards / bidder_total_awards,
      share_value = ifelse(
        bidder_total_value > 0,
        total_value / bidder_total_value,
        NA_real_
      )
    ) %>%
    dplyr::ungroup()
  
  # Flag atypical clusters
  df_with_flags <- cpv_data %>%
    dplyr::left_join(
      bidder_cpv_stats %>%
        dplyr::select(bidder_id, cpv_cluster, n_awards, bidder_total_awards, share_awards, share_value),
      by = c("bidder_id", "cpv_cluster")
    ) %>%
    dplyr::mutate(
      enough_history = bidder_total_awards >= config$thresholds$min_history_threshold,
      cpv_cluster_atypical = enough_history &
        share_awards < config$thresholds$marginal_share_threshold &
        n_awards <= config$thresholds$max_wins_atypical
    )
  
  # Cluster-supplier counts
  cluster_supplier_counts <- cpv_data %>%
    dplyr::group_by(cpv_cluster, bidder_id) %>%
    dplyr::summarise(n_ic = dplyr::n(), .groups = "drop")
  
  cluster_totals <- cpv_data %>%
    dplyr::group_by(cpv_cluster) %>%
    dplyr::summarise(
      n_c = dplyr::n(),
      n_suppliers_c = dplyr::n_distinct(bidder_id),
      .groups = "drop"
    )
  
  # Surprise scores
  cluster_surprise <- cluster_supplier_counts %>%
    dplyr::left_join(cluster_totals, by = "cpv_cluster") %>%
    dplyr::mutate(
      p_i_given_c = (n_ic + 1) / (n_c + n_suppliers_c),
      surprise_score = -log(p_i_given_c)
    )
  
  df_with_surprise <- df_with_flags %>%
    dplyr::left_join(
      cluster_surprise %>% dplyr::select(cpv_cluster, bidder_id, surprise_score),
      by = c("cpv_cluster", "bidder_id")
    ) %>%
    dplyr::mutate(
      surprise_z = ifelse(
        !is.na(surprise_score),
        (surprise_score - mean(surprise_score, na.rm = TRUE)) / sd(surprise_score, na.rm = TRUE),
        NA_real_
      )
    )
  
  # Home market
  home_market <- bidder_cpv_stats %>%
    dplyr::group_by(bidder_id) %>%
    dplyr::slice_max(n_awards, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(bidder_id, home_cpv_cluster = cpv_cluster)
  
  # Unusual entries
  df_unusual <- df_with_surprise %>%
    dplyr::left_join(home_market, by = "bidder_id") %>%
    dplyr::mutate(target_cpv_cluster = cpv_cluster) %>%
    dplyr::filter(enough_history, cpv_cluster_atypical, !is.na(surprise_z))
  
  df_unusual
}

#' Build unusual entry matrix
#' 
#' @param df_unusual Unusual entries data
#' @param config Configuration list
#' @return Data frame of unusual entry flows
build_unusual_matrix <- function(df_unusual, config) {
  df_unusual %>%
    dplyr::group_by(home_cpv_cluster, target_cpv_cluster) %>%
    dplyr::summarise(
      n_bidders = dplyr::n_distinct(bidder_id),
      n_awards = dplyr::n(),
      mean_surprise = mean(surprise_z, na.rm = TRUE),
      .groups = "drop"
    )
}

#' Analyze markets
#' 
#' @param df Data frame
#' @param config Configuration list
#' @param output_dir Output directory
#' @return List of results
analyze_markets <- function(df, config, output_dir) {
  results <- list()
  save_plot <- create_plot_saver(output_dir, config)
  
  required_cols <- c("tender_id", "lot_number", "bidder_id", "lot_productcode", "bid_priceusd")
  
  if (!validate_required_columns(df, required_cols, "market analysis")) {
    return(results)
  }
  
  # Build CPV data
  results$cpv_data <- build_cpv_df(df, config$thresholds$cpv_digits)
  
  # Detect unusual entries
  df_unusual <- detect_unusual_entries(results$cpv_data, config)
  
  if (nrow(df_unusual) == 0) {
    message("No unusual market entries detected")
    return(results)
  }
  
  results$unusual_entries <- df_unusual
  results$unusual_matrix <- build_unusual_matrix(df_unusual, config)
  
  # Network plot
  edges_markets <- results$unusual_matrix %>%
    dplyr::filter(n_bidders >= config$thresholds$min_bidders_for_edge) %>%
    dplyr::rename(from = home_cpv_cluster, to = target_cpv_cluster)
  
  if (nrow(edges_markets) > 0) {
    g_markets <- igraph::graph_from_data_frame(edges_markets, directed = TRUE)
    
    # In analyze_markets function, update network plot:
    results$network_plot <- ggraph::ggraph(g_markets, layout = "fr") +
      ggraph::geom_edge_link(
        ggplot2::aes(width = n_bidders, colour = mean_surprise),
        arrow = grid::arrow(length = grid::unit(0.15, "cm")),
        end_cap = ggraph::circle(2, "mm"),
        alpha = 0.6
      ) +
      ggraph::geom_node_point(size = PLOT_SIZES$point_size + 1, colour = "darkorange") +
      ggraph::geom_node_text(ggplot2::aes(label = name), repel = TRUE, size = PLOT_SIZES$geom_text_size) +
      ggraph::scale_edge_width(range = c(0.2, 2)) +
      ggraph::scale_edge_colour_continuous(
        low = "green",
        high = "red",
        name = "Mean surprise score (standardised)"
      ) +
      ggplot2::labs(
        title = paste("Network of unusual market entries –", config$country),
        subtitle = "Nodes represent CPV clusters; edges show atypical flow of bidders"
      ) +
      ggplot2::theme_void(base_size = PLOT_SIZES$base_size) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = PLOT_SIZES$title_size, face = "bold"),
        plot.subtitle = ggplot2::element_text(size = PLOT_SIZES$subtitle_size),
        legend.text = ggplot2::element_text(size = PLOT_SIZES$legend_text_size),
        legend.title = ggplot2::element_text(size = PLOT_SIZES$legend_title_size)
      )
  }
  
  # Supplier-level unusual behavior
  results$supplier_unusual <- df_unusual %>%
    dplyr::group_by(bidder_id, home_cpv_cluster) %>%
    dplyr::summarise(
      max_surprise_z = max(surprise_z, na.rm = TRUE),
      mean_surprise_z = mean(surprise_z, na.rm = TRUE),
      n_atypical_markets = dplyr::n_distinct(target_cpv_cluster),
      n_atypical_awards = dplyr::n(),
      .groups = "drop"
    )
  
  top_suppliers <- results$supplier_unusual %>%
    dplyr::filter(n_atypical_awards >= 3) %>%
    dplyr::arrange(dplyr::desc(max_surprise_z)) %>%
    dplyr::slice_head(n = config$thresholds$top_n_suppliers)
  
  results$supplier_unusual_plot <- ggplot2::ggplot(
    top_suppliers,
    ggplot2::aes(
      x = reorder(bidder_id, max_surprise_z),
      y = max_surprise_z,
      fill = n_atypical_markets
    )
  ) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_viridis_c(option = "C", name = "No. atypical\nmarkets") +
    ggplot2::labs(
      title = paste("Top suppliers by unusual market behaviour –", config$country),
      subtitle = "Max standardised surprise score across atypical CPV entries",
      x = "Supplier ID",
      y = "Max surprise z-score"
    ) +
    standard_plot_theme()
  
  save_plot(results$supplier_unusual_plot, "supp_unusual_suppliers")
  
  # Market-level unusual entries
  results$market_unusual <- df_unusual %>%
    dplyr::group_by(target_cpv_cluster) %>%
    dplyr::summarise(
      mean_surprise_z = mean(surprise_z, na.rm = TRUE),
      max_surprise_z = max(surprise_z, na.rm = TRUE),
      n_unusual_suppliers = dplyr::n_distinct(bidder_id),
      n_unusual_awards = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::mutate(market_risk_index = mean_surprise_z * log1p(n_unusual_suppliers))
  
  top_markets <- results$market_unusual %>%
    dplyr::slice_max(market_risk_index, n = config$thresholds$top_n_markets)
  
  results$market_unusual_plot <- ggplot2::ggplot(
    top_markets,
    ggplot2::aes(
      x = n_unusual_suppliers,
      y = mean_surprise_z,
      size = n_unusual_awards,
      colour = mean_surprise_z,
      label = target_cpv_cluster
    )
  ) +
    ggplot2::geom_point(alpha = 0.85) +
    ggrepel::geom_text_repel(size = PLOT_SIZES$geom_text_size, colour = "gray20") +
    ggplot2::scale_size_continuous(name = "No. atypical awards", range = c(3, 12)) +
    ggplot2::scale_colour_gradientn(
      colours = c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c"),
      name = "Mean surprise (z-score)"
    ) +
    ggplot2::labs(
      title = paste("Markets attracting unusual supplier entries –", config$country),
      subtitle = "Each point is a CPV cluster; size = awards; color = surprise",
      x = "Number of suppliers entering atypically",
      y = "Mean surprise z-score"
    ) +
    standard_plot_theme()
  
  save_plot(results$market_unusual_plot, "supp_unusual_markets")
  
  results
}

# ========================================================================
# PART 15: MODULE - PRICE ANALYSIS
# ========================================================================

#' Prepare price data
#' 
#' @param df Data frame
#' @param config Configuration list
#' @return Prepared data frame
prepare_price_data <- function(df, config) {
  if (!validate_required_columns(df, c("bid_price", "lot_estimatedprice"), "price analysis")) {
    return(NULL)
  }
  
  # Determine value variable
  val_var <- dplyr::case_when(
    "bid_priceusd" %in% names(df) ~ "bid_priceusd",
    "bid_price" %in% names(df) ~ "bid_price",
    TRUE ~ NA_character_
  )
  
  # Compute total missing share
  key_vars <- names(label_lookup) %>% gsub("_missing_share", "", .)
  df <- df %>%
    dplyr::mutate(
      total_missing_share = rowMeans(
        dplyr::across(all_of(key_vars), ~ is.na(.)),
        na.rm = TRUE
      )
    )
  
  # Add log contract value
  if (!is.na(val_var)) {
    df <- df %>%
      dplyr::mutate(log_contract_value = log1p(.data[[val_var]]))
  }
  
  # Filter by year
  yrp <- config$years_relprice
  rel_price_data <- df %>%
    dplyr::mutate(
      relative_price = bid_price / lot_estimatedprice,
      relative_price = ifelse(
        relative_price > config$thresholds$max_relative_price |
          relative_price <= config$thresholds$min_relative_price,
        NA,
        relative_price
      )
    ) %>%
    dplyr::filter(
      !is.na(tender_year),
      tender_year >= yrp$min_year,
      tender_year <= yrp$max_year
    )
  
  # Relevel factors
  if ("tender_proceduretype" %in% names(rel_price_data)) {
    rel_price_data <- rel_price_data %>%
      dplyr::mutate(
        tender_proceduretype = stats::relevel(as.factor(tender_proceduretype), ref = "OPEN")
      )
  }
  if ("buyer_buyertype" %in% names(rel_price_data)) {
    rel_price_data <- rel_price_data %>%
      dplyr::mutate(
        buyer_buyertype = stats::relevel(as.factor(buyer_buyertype), ref = "UTILITIES")
      )
  }
  
  rel_price_data
}


#' Run relative price specifications
#' 
#' @param rel_price_data Price data
#' @param config Configuration list
#' @return Data frame of specification results
run_relprice_specs <- function(rel_price_data, config) {
  out <- list()
  k <- 0L
  
  fe_set <- c("0", config$models$fe_set)
  
  for (mt in config$models$model_types_relprice) {
    for (fe in fe_set) {
      fe_part <- make_fe_part(fe)
      
      for (cl in config$models$cluster_set) {
        if (fe == "0" && cl == "none") next
        
        cl_fml <- make_cluster(cl)
        
        for (ctrl in config$models$controls_set) {
          rhs_terms <- switch(
            ctrl,
            "x_only" = c("total_missing_share"),
            "base" = c("total_missing_share", "log_contract_value",
                       "buyer_buyertype", "tender_proceduretype")
          )
          
          rhs_terms <- rhs_terms[rhs_terms %in% names(rel_price_data)]
          
          # ADDED: Skip if no valid terms
          if (length(rhs_terms) == 0) {
            message("Skipping spec: no valid RHS terms for controls=", ctrl)
            next
          }
          
          rhs <- paste(rhs_terms, collapse = " + ")
          
          m <- NULL
          d_used <- NULL
          x_name <- "total_missing_share"
          
          if (mt == "ols_level") {
            fml <- stats::as.formula(paste0("relative_price ~ ", rhs, " | ", fe_part))
            m <- safe_fixest(fixest::feols(fml, data = rel_price_data, cluster = cl_fml))
            d_used <- rel_price_data
            
          } else if (mt == "ols_log") {
            d_used <- rel_price_data %>%
              dplyr::mutate(log_relative_price = log(relative_price))
            d_used$log_relative_price[!is.finite(d_used$log_relative_price)] <- NA_real_
            d_used <- d_used[!is.na(d_used$log_relative_price), , drop = FALSE]
            if (nrow(d_used) == 0) next
            
            fml <- stats::as.formula(paste0("log_relative_price ~ ", rhs, " | ", fe_part))
            m <- safe_fixest(fixest::feols(fml, data = d_used, cluster = cl_fml))
            
          } else if (mt == "gamma_log") {
            fml <- stats::as.formula(paste0("relative_price ~ ", rhs, " | ", fe_part))
            m <- safe_fixest(fixest::feglm(
              fml, data = rel_price_data, family = Gamma(link = "log"), cluster = cl_fml
            ))
            d_used <- rel_price_data
          }
          
          if (is.null(m)) next
          
          # ADDED: Check if x_name is actually in the model
          if (!(x_name %in% names(coef(m)))) {
            message("Skipping spec: ", x_name, " not in model coefficients")
            next
          }
          
          eff <- extract_effect_fixest(model = m, x_name = x_name, data_used = d_used)
          
          k <- k + 1L
          out[[k]] <- data.frame(
            outcome = "rel_price",
            model_type = mt,
            fe = fe,
            cluster = cl,
            controls = ctrl,
            estimate = eff$estimate,
            pvalue = eff$pvalue,
            nobs = eff$nobs,
            std_slope = eff$std_slope,
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }
  
  if (length(out) == 0) return(data.frame())
  do.call(rbind, out)
}

#' Analyze prices
#' 
#' @param df Data frame
#' @param config Configuration list
#' @param output_dir Output directory
#' @return List of results
analyze_prices <- function(df, config, output_dir) {
  results <- list()
  
  # Create plot saver
  save_plot <- create_plot_saver(output_dir, config)
  
  results$data <- prepare_price_data(df, config)
  
  if (is.null(results$data) || nrow(results$data) == 0) {
    message("No data available for price analysis")
    return(results)
  }
  
  results$specs <- run_relprice_specs(results$data, config)
  
  # Check if specs were generated
  if (is.null(results$specs) || nrow(results$specs) == 0) {
    message("No relative price specifications generated for ", config$country)
    return(results)
  }
  
  results$relprice_sensitivity <- build_sensitivity_bundle(results$specs)
  
  # Print sensitivity
  if (!is.null(results$relprice_sensitivity)) {
    message("\n--- REL_PRICE sensitivity (", config$country, ") ---")
    print(results$relprice_sensitivity$overall)
    print(results$relprice_sensitivity$sign)
    print(results$relprice_sensitivity$by_fe)
    print(results$relprice_sensitivity$by_cluster)
    print(results$relprice_sensitivity$by_controls)
    print(results$relprice_sensitivity$classes)
    print(results$relprice_sensitivity$top_cells)
  }
  
  # Select and plot best model
  best_row <- pick_best_model(
    results$specs,
    require_positive = TRUE,
    p_max = config$models$p_max,
    strength_col = "std_slope"
  )
  
  if (is.null(best_row)) {
    message("No relative price model met selection criteria for ", config$country)
    return(results)
  }
  
  message("Relative price BEST spec for ", config$country, ": ",
          "model=", best_row$model_type,
          ", fe=", best_row$fe, 
          ", cluster=", best_row$cluster,
          ", controls=", best_row$controls,
          ", p=", signif(best_row$pvalue, 3))
  
  # Refit model
  fe_part <- make_fe_part(best_row$fe)
  cl_fml <- make_cluster(best_row$cluster)
  
  rhs_terms <- switch(
    best_row$controls,
    "x_only" = c("total_missing_share"),
    "base" = c("total_missing_share", "log_contract_value",
               "buyer_buyertype", "tender_proceduretype")
  )
  rhs_terms <- rhs_terms[rhs_terms %in% names(results$data)]
  rhs <- paste(rhs_terms, collapse = " + ")
  
  model <- NULL
  pred <- NULL
  y_lab <- NULL
  
  if (best_row$model_type == "ols_level") {
    fml <- stats::as.formula(paste0("relative_price ~ ", rhs, " | ", fe_part))
    model <- fixest::feols(fml, data = results$data, cluster = cl_fml)
    pred <- tryCatch(
      ggeffects::ggpredict(model, terms = "total_missing_share"),
      error = function(e) {
        message("Error in ggpredict: ", e$message)
        NULL
      }
    )
    y_lab <- "Predicted Relative Price"
    
  } else if (best_row$model_type == "ols_log") {
    d <- results$data %>% dplyr::mutate(log_relative_price = log(relative_price))
    d$log_relative_price[!is.finite(d$log_relative_price)] <- NA_real_
    d <- d[!is.na(d$log_relative_price), , drop = FALSE]
    
    if (nrow(d) > 0) {
      fml <- stats::as.formula(paste0("log_relative_price ~ ", rhs, " | ", fe_part))
      model <- fixest::feols(fml, data = d, cluster = cl_fml)
      pred <- tryCatch(
        ggeffects::ggpredict(model, terms = "total_missing_share"),
        error = function(e) {
          message("Error in ggpredict: ", e$message)
          NULL
        }
      )
      y_lab <- "Predicted log(Relative Price)"
    }
    
  } else if (best_row$model_type == "gamma_log") {
    fml <- stats::as.formula(paste0("relative_price ~ ", rhs, " | ", fe_part))
    model <- fixest::feglm(
      fml, data = results$data, family = Gamma(link = "log"), cluster = cl_fml
    )
    pred <- tryCatch(
      ggeffects::ggpredict(model, terms = "total_missing_share"),
      error = function(e) {
        message("Error in ggpredict: ", e$message)
        NULL
      }
    )
    y_lab <- "Predicted Relative Price (Gamma-log)"
  }
  
  if (!is.null(pred)) {
    years_used <- range(results$data$tender_year, na.rm = TRUE)
    
    results$rel_price_plot <- plot_ggeffects_line(
      pred = pred,
      title = paste("Predicted Relative Price by Missing Share –", config$country),
      subtitle = paste0(
        "(BEST) Model: ", pretty_model_name(best_row$model_type),
        " | Years: ", years_used[1], "–", years_used[2],
        " | N=", best_row$nobs,
        " | FE: ", pretty_fe_label(best_row$fe),
        " | Cluster: ", best_row$cluster,
        " | Controls: ", pretty_controls_label(best_row$controls)
      ),
      x_lab = "Total Missing Share",
      y_lab = y_lab,
      caption = paste0(
        "Filters: tender_year in [", config$years_relprice$min_year, ", ",
        config$years_relprice$max_year, "]. ",
        controls_note(best_row$controls)
      )
    )
    
    # Save the plot
    tryCatch({
      save_plot(results$rel_price_plot, "reg_relative_price_missing_share")
      message("✓ Relative price plot saved successfully for ", config$country)
    }, error = function(e) {
      message("✗ Error saving relative price plot: ", e$message)
      # Fallback: save directly
      ggplot2::ggsave(
        filename = file.path(output_dir, paste0("reg_relative_price_missing_share_", config$country, ".png")),
        plot = results$rel_price_plot,
        width = 10,
        height = 8,
        dpi = 300
      )
      message("✓ Saved plot using fallback method")
    })
    
  } else {
    message("Could not generate predictions for relative price plot")
  }
  
  results
}

# ========================================================================
# PART 16: MODULE - REGIONAL ANALYSIS
# ========================================================================

#' Analyze regional patterns (NUTS3)
#' 
#' @param df Data frame
#' @param config Configuration list
#' @param output_dir Output directory
#' @return List of results
analyze_regional <- function(df, config, output_dir) {
  results <- list()
  save_plot <- create_plot_saver(output_dir, config)
  
  if (!validate_required_columns(df, "buyer_nuts", "regional analysis")) {
    return(results)
  }
  
  df_nuts <- df %>% clean_nuts3(buyer_nuts)
  
  results$missing_by_region <- df_nuts %>%
    dplyr::filter(!is.na(nuts3)) %>%
    dplyr::select(!dplyr::starts_with("ind_")) %>%
    dplyr::group_by(nuts3) %>%
    dplyr::summarise(
      missing_share = mean(is.na(dplyr::cur_data_all()), na.rm = TRUE),
      .groups = "drop"
    )
  
  # Try to get map data
  region_sf <- tryCatch(
    {
      eurostat::get_eurostat_geospatial(
        output_class = "sf",
        nuts_level = 3,
        year = 2021
      ) %>%
        dplyr::filter(CNTR_CODE == config$country)
    },
    error = function(e) {
      message("Could not fetch NUTS3 map data: ", e$message)
      NULL
    }
  )
  
  if (!is.null(region_sf) && nrow(region_sf) > 0) {
    results$region_map_data <- region_sf %>%
      dplyr::left_join(results$missing_by_region, by = c("NUTS_ID" = "nuts3"))
    
    results$region_missing_map <- ggplot2::ggplot(results$region_map_data) +
      ggplot2::geom_sf(ggplot2::aes(fill = missing_share), color = "grey40", size = 0.2) +
      ggplot2::scale_fill_distiller(
        palette = "Blues",
        direction = 1,
        na.value = "grey90",
        labels = scales::percent_format(accuracy = 1),
        name = "Missing share"
      ) +
      ggplot2::labs(
        title = paste("Overall share of missing values by region (NUTS3) –", config$country)
      ) +
      ggplot2::theme_minimal()
    
    save_plot(results$region_missing_map, "region_missing_map")
  }
  
  results
}

# ========================================================================
# PART 17: SUMMARY STATISTICS MODULE
# ========================================================================

#' Log summary statistics
#' 
#' @param df Data frame
#' @param config Configuration list
#' @param output_dir Output directory
#' @return List of summary stats
log_summary_stats <- function(df, config, output_dir) {
  stats <- list()
  
  # Observations per year
  stats$n_obs_per_year <- df %>%
    dplyr::count(tender_year, name = "n_observations")
  
  # Unique entities
  stats$n_unique_buyers <- if ("buyer_masterid" %in% names(df)) {
    dplyr::n_distinct(df$buyer_masterid, na.rm = TRUE)
  } else {
    NA_integer_
  }
  
  stats$n_unique_bidders <- if ("bidder_masterid" %in% names(df)) {
    dplyr::n_distinct(df$bidder_masterid, na.rm = TRUE)
  } else {
    NA_integer_
  }
  
  # Tenders per year
  stats$tender_year_tenders <- if ("tender_id" %in% names(df)) {
    df %>%
      dplyr::group_by(tender_year) %>%
      dplyr::summarise(
        n_unique_tender_id = dplyr::n_distinct(tender_id),
        .groups = "drop"
      )
  } else {
    NULL
  }
  
  # Variables present
  stats$vars_present <- names(df)[!startsWith(names(df), "ind_")]
  
  # Print summary
  cat("\n", strrep("=", 70), "\n")
  cat("DATA SUMMARY FOR", config$country, "\n")
  cat(strrep("=", 70), "\n\n")
  
  cat("Contracts per year:\n")
  print(stats$n_obs_per_year)
  cat("\n")
  
  if (!is.na(stats$n_unique_buyers)) {
    cat("Number of unique buyers (buyer_masterid):", stats$n_unique_buyers, "\n\n")
  }
  
  if (!is.na(stats$n_unique_bidders)) {
    cat("Number of unique bidders (bidder_masterid):", stats$n_unique_bidders, "\n\n")
  }
  
  if (!is.null(stats$tender_year_tenders)) {
    cat("Number of unique tenders per year:\n")
    print(stats$tender_year_tenders)
    cat("\n")
  }
  
  cat("Variables present (excluding indicators):\n")
  cat(paste(stats$vars_present, collapse = ", "), "\n")
  cat("\n", strrep("=", 70), "\n\n")
  
  invisible(stats)
}

# ========================================================================
# PART 18: SAFE MODULE EXECUTION
# ========================================================================

#' Safely run analysis module
#' 
#' @param module_fn Module function
#' @param df Data frame
#' @param config Configuration list
#' @param output_dir Output directory
#' @return Module results or error info
safely_run_module <- function(module_fn, df, config, output_dir) {
  module_name <- deparse(substitute(module_fn))
  
  tryCatch(
    {
      message("\n", strrep("-", 60))
      message("Running module: ", module_name)
      message(strrep("-", 60))
      
      result <- module_fn(df, config, output_dir)
      
      message("✓ Module completed successfully: ", module_name)
      result
    },
    error = function(e) {
      message("✗ Module failed: ", module_name)
      message("Error: ", e$message)
      list(error = e$message, status = "failed")
    }
  )
}

# ========================================================================
# PART 19: ENSURE OUTPUT DIRECTORY
# ========================================================================

#' Ensure output directory exists
#' 
#' @param output_dir Output directory path
ensure_output_directory <- function(output_dir) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    message("Created output directory: ", output_dir)
  }
}

# ========================================================================
# PART 20: MAIN PIPELINE
# ========================================================================

#' Run complete integrity analysis pipeline
#' 
#' @param df Data frame with procurement data
#' @param country_code Two-letter country code
#' @param output_dir Directory for outputs
#' @return List of analysis results
#' @export
run_integrity_pipeline <- function(df, country_code = "GEN", output_dir) {
  
  # Initialize
  message("\n", strrep("=", 70))
  message("PROCUREMENT INTEGRITY ANALYSIS PIPELINE")
  message("Country: ", country_code)
  message(strrep("=", 70), "\n")
  
  config <- create_pipeline_config(country_code)
  ensure_output_directory(output_dir)
  
  # Prepare data
  df <- prepare_data(df)
  data_quality <- check_data_quality(df, config)
  
  # Log summary statistics
  summary_stats <- log_summary_stats(df, config, output_dir)
  
  # Run analysis modules
  results <- list(
    config = config,
    data_quality = data_quality,
    summary_stats = summary_stats,
    missing = safely_run_module(analyze_missing_values, df, config, output_dir),
    interoperability = safely_run_module(analyze_interoperability, df, config, output_dir),
    competition = safely_run_module(analyze_competition, df, config, output_dir),
    markets = safely_run_module(analyze_markets, df, config, output_dir),
    prices = safely_run_module(analyze_prices, df, config, output_dir),
    regional = safely_run_module(analyze_regional, df, config, output_dir)
  )
  
  # Final message
  message("\n", strrep("=", 70))
  message("PIPELINE COMPLETED")
  message("Results saved to: ", output_dir)
  message(strrep("=", 70), "\n")
  
  invisible(results)
}

# ========================================================================
# END OF SCRIPT
# ========================================================================
  
  


