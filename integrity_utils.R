# ========================================================================
# Helper functions (generic utilities used by all countries)
# ========================================================================
# 0) Data uploading helper------------------------------------------------
load_data <- function(input_path) {
  data <- data.table::fread(
    input            = input_path,
    keepLeadingZeros = TRUE,
    encoding         = "UTF-8",
    stringsAsFactors = FALSE,
    showProgress     = TRUE,
    na.strings       = c("", "-", "NA")
  )
  
  # Drop duplicated column names (keep the first occurrence)
  dup_cols <- duplicated(names(data))
  if (any(dup_cols)) {
    data <- data[, !dup_cols, with = FALSE]
  }
  
  return(data)
}
# 1) Label lookup helpers ------------------------------------------------
label_with_lookup <- function(vec, lookup) {
  out <- lookup[vec]
  out[is.na(out)] <- vec[is.na(out)]
  unname(out)
}

label_lookup <- c(
  tender_id_missing_share                                  = "Tender ID",
  tender_year_missing_share                                = "Tender Year",
  lot_number_missing_share                                 = "Lot Number",
  bid_number_missing_share                                 = "Number of Bids",
  bid_iswinning_missing_share                              = "Winning Bid Indicator",
  tender_country_missing_share                             = "Tender Country",
  tender_awarddecisiondate_missing_share                   = "Award Decision Date",
  tender_contractsignaturedate_missing_share               = "Contract Signature Date",
  tender_biddeadline_missing_share                         = "Bid Deadline",
  tender_proceduretype_missing_share                       = "Procedure Type",
  tender_nationalproceduretype_missing_share               = "National Procedure Type",
  tender_supplytype_missing_share                          = "Supply Type",
  tender_publications_firstcallfortenderdate_missing_share  = "First Call for Tender Date",
  notice_url_missing_share                                 = "Notice URL",
  source_missing_share                                     = "Source",
  tender_publications_firstdcontractawarddate_missing_share = "First Contract Award Date",
  tender_publications_lastcontractawardurl_missing_share    = "Last Contract Award URL",
  buyer_masterid_missing_share                             = "Buyer Master ID",
  buyer_id_missing_share                                   = "Buyer ID",
  buyer_city_missing_share                                 = "Buyer City",
  buyer_postcode_missing_share                             = "Buyer Postcode",
  buyer_country_missing_share                              = "Buyer Country",
  buyer_nuts_missing_share                                 = "Buyer NUTS",
  buyer_name_missing_share                                 = "Buyer Name",
  buyer_buyertype_missing_share                            = "Buyer Type",
  buyer_mainactivities_missing_share                       = "Buyer Main Activities",
  tender_addressofimplementation_country_missing_share      = "Implementation Country",
  tender_addressofimplementation_nuts_missing_share         = "Implementation NUTS",
  bidder_masterid_missing_share                            = "Bidder Master ID",
  bidder_id_missing_share                                  = "Bidder ID",
  bidder_country_missing_share                             = "Bidder Country",
  bidder_nuts_missing_share                                = "Bidder NUTS",
  bidder_name_missing_share                                = "Bidder Name",
  bid_price_missing_share                                  = "Bid Price",
  bid_priceusd_missing_share                               = "Bid Price (USD)",
  bid_pricecurrency_missing_share                          = "Bid Price Currency",
  lot_productcode_missing_share                            = "Product Code",
  lot_localproductcode_type_missing_share                  = "Local Product Code Type",
  lot_localproductcode_missing_share                       = "Local Product Code",
  submp_missing_share                                      = "Submission Period",
  decp_missing_share                                       = "Decision Period",
  is_capital_missing_share                                 = "Capital City Indicator",
  lot_title_missing_share                                  = "Lot Title",
  lot_estimatedpriceusd_missing_share                      = "Estimated Price (USD)",
  lot_estimatedprice_missing_share                         = "Estimated Price",
  lot_estimatedpricecurrency_missing_share                 = "Estimated Price Currency"
)

# 2) Buyer grouping helper -----------------------------------------------
add_buyer_group <- function(buyer_buyertype) {
  group <- dplyr::case_when(
    grepl("(?i)national",  buyer_buyertype) ~ "National Buyer",
    grepl("(?i)regional",  buyer_buyertype) ~ "Regional Buyer",
    grepl("(?i)utilities", buyer_buyertype) ~ "Utilities",
    grepl("(?i)European",  buyer_buyertype) ~ "EU agency",
    TRUE                                    ~ "Other Public Bodies"
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

# 3) Tender year helper --------------------------------------------------
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

# 4) Generic "missing shares" summariser ---------------------------------
summarise_missing_shares <- function(df, cols = !dplyr::starts_with("ind_")) {
  df %>%
    dplyr::summarise(
      dplyr::across(
        .cols  = {{ cols }},
        .fns   = ~ mean(is.na(.)),
        .names = "{.col}_missing_share"
      ),
      .groups = "drop"
    )
}

# 5) Pivot missing shares to long format ---------------------------------
pivot_missing_long <- function(df, id_vars = NULL) {
  if (is.null(id_vars)) {
    df %>%
      tidyr::pivot_longer(
        cols      = dplyr::everything(),
        names_to  = "variable",
        values_to = "missing_share"
      )
  } else {
    df %>%
      tidyr::pivot_longer(
        cols      = -dplyr::all_of(id_vars),
        names_to  = "variable",
        values_to = "missing_share"
      )
  }
}

# 6) Generic bar plot of missing share per variable ----------------------
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
    ggplot2::labs(
      title = title,
      x     = "Variable",
      y     = "Missing Share"
    ) +
    ggplot2::theme_minimal(base_size = 16) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 20, face = "bold"),
      axis.title = ggplot2::element_text(size = 16),
      axis.text  = ggplot2::element_text(size = 14)
    )
}

# 7) Generic missing share heatmap -----------------------------------------
plot_missing_heatmap <- function(data, x_var, y_var, fill_var,
                                 title, x_lab, y_lab) {
  x_sym    <- rlang::sym(x_var)
  y_sym    <- rlang::sym(y_var)
  fill_sym <- rlang::sym(fill_var)
  
  ggplot2::ggplot(
    data,
    ggplot2::aes(
      x    = !!x_sym,
      y    = !!y_sym,
      fill = !!fill_sym
    )
  ) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::geom_text(
      ggplot2::aes(label = scales::percent(!!fill_sym, accuracy = 1)),
      size  = 3,
      color = "black"
    ) +
    ggplot2::scale_fill_gradient(
      name   = "Missing share",
      labels = scales::percent_format(accuracy = 1),
      limits = c(0, 1),
      low    = "skyblue1",
      high   = "indianred1"
    ) +
    ggplot2::labs(
      title = title,
      x     = x_lab,
      y     = y_lab
    ) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      plot.title  = ggplot2::element_text(size = 18, face = "bold"),
      axis.title  = ggplot2::element_text(size = 14),
      axis.text.x = ggplot2::element_text(size = 10, angle = 45, hjust = 1),
      axis.text.y = ggplot2::element_text(size = 10)
    )
}

# 8) Generic “top N bar” plot --------------------------------------------
plot_top_bar <- function(df, x_var, y_var, label_var,
                         title, x_lab, y_lab,
                         fill_color = "skyblue1",
                         y_limit   = c(0, 1.05),
                         percent   = TRUE) {
  x_sym   <- rlang::sym(x_var)
  y_sym   <- rlang::sym(y_var)
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
      size  = 3.2,
      check_overlap = TRUE
    ) +
    ggplot2::coord_flip() +
    ggplot2::theme_bw() +
    ggplot2::labs(
      title = title,
      x     = x_lab,
      y     = y_lab
    )
  
  if (!is.null(y_limit)) {
    p <- p + ggplot2::ylim(y_limit)
  }
  
  p
}

# 9) Generic NUTS3 cleaning helper ---------------------------------------
#    Assumes NUTS codes like "BG123", "RO321", etc. in buyer_nuts
clean_nuts3 <- function(df, nuts_col = buyer_nuts) {
  nuts_quo <- rlang::enquo(nuts_col)
  
  df %>%
    dplyr::mutate(
      buyer_nuts = as.character(!!nuts_quo),
      nuts_clean = buyer_nuts %>%
        stringr::str_replace_all("\\[|\\]", "") %>%
        stringr::str_replace_all('"', "") %>%
        stringr::str_squish()
    ) %>%
    dplyr::mutate(
      # keep any valid NUTS3: two letters + three digits
      nuts3 = dplyr::if_else(
        stringr::str_detect(nuts_clean, "^[A-Z]{2}[0-9]{3}$"),
        nuts_clean,
        NA_character_
      )
    )
}

# 10) Generic ggpredict line+band plot -----------------------------------
plot_ggeffects_line <- function(pred,
                                title,
                                subtitle,
                                x_lab,
                                y_lab,
                                caption = NULL) {
  ggplot2::ggplot(pred, ggplot2::aes(x, predicted)) +
    ggplot2::geom_line(size = 1.5, color = "lightblue") +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
    ggplot2::labs(
      title    = title,
      subtitle = subtitle,
      x        = x_lab,
      y        = y_lab,
      caption  = caption
    ) +
    ggplot2::theme_minimal(base_size = 20)
}

# 11) CPV cluster df helper ----------------------------------------------
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

# 12) Organization-level missing share for identifiers ---------------------
compute_org_missing <- function(df) {
  # helper to compute missing share safely
  miss_or_na <- function(col) {
    if (col %in% names(df)) {
      mean(is.na(df[[col]]))
    } else {
      NA_real_  # marks variable as not present
    }
  }
  
  tibble::tribble(
    ~organization_type, ~id_type,                                   ~missing_share,
    "Supplier",         "Source ID",                         miss_or_na("bidder_id"),
    "Supplier",         "Generated ID",                      miss_or_na("bidder_masterid"),
    "Supplier",         "Name",                              miss_or_na("bidder_name"),
    "Supplier",         "Address (postcode)",                miss_or_na("bidder_nuts"),
    "Buyer",            "Source ID",                         miss_or_na("buyer_id"),
    "Buyer",            "Generated ID",                      miss_or_na("buyer_masterid"),
    "Buyer",            "Name",                              miss_or_na("buyer_name"),
    "Buyer",            "Address (postcode)",                miss_or_na("buyer_postcode")
  )
}


# 14) Year-filter configuration for each country and each component of the pipeline
year_filter_config <- tibble::tribble(
  ~component,  ~country_code, ~min_year, ~max_year,
  # default catch-all
  "default",   "BG",          NA,        NA,
  "default",   "UY",          NA,        NA,
  # component-specific overrides
  "singleb",   "BG",          2011,      2018,
  "singleb",   "UY",          2014,      NA,
  "rel_price", "BG",          2011,      2018,
  "rel_price", "UY",          2014,      NA
)


# convenient helper: returns c(min_year, max_year)
get_year_range <- function(country_code,
                           component = c("singleb", "rel_price", "default")) {
  component   <- match.arg(component)
  cc          <- toupper(country_code)
  
  # 1) try component-specific rule
  row_spec <- year_filter_config %>%
    dplyr::filter(component == !!component, country_code == !!cc) %>%
    dplyr::slice_head(n = 1)
  
  # 2) if nothing, fall back to default for that country
  if (nrow(row_spec) == 0) {
    row_spec <- year_filter_config %>%
      dplyr::filter(component == "default", country_code == !!cc) %>%
      dplyr::slice_head(n = 1)
  }
  
  # 3) if still nothing, fall back to "no filtering"
  if (nrow(row_spec) == 0) {
    return(list(min_year = -Inf, max_year = Inf))
  }
  
  min_y <- if (is.na(row_spec$min_year)) -Inf else row_spec$min_year
  max_y <- if (is.na(row_spec$max_year))  Inf else row_spec$max_year
  
  list(min_year = min_y, max_year = max_y)
}


# ========================================================================
# Unified country pipeline: works for any country where columns exist
# ========================================================================

run_integrity_pipeline <- function(df, country_code = "GEN", output_dir) {
  message("Running integrity pipeline for ", country_code, " ...")
  
  # Ensure tender_year exists for all later steps
  df <- df %>% add_tender_year()
  
  # ----------------------------------------------------------------------
  # SUMMARY STATS BLOCK (printed once at the beginning)
  # ----------------------------------------------------------------------
  # 1) Number of contracts per year
  n_obs_per_year <- df %>%
    dplyr::count(tender_year, name = "n_observations")
  
  # 2) Number of unique buyers(using buyer_masterid)
  n_unique_buyers <- if ("buyer_masterid" %in% names(df)) {
    dplyr::n_distinct(df$buyer_masterid, na.rm = TRUE)
  } else {
    NA_integer_
  }
  
  # 3) Number of unique tenders per year
  tender_year_tenders <- if ("tender_id" %in% names(df)) {
    df %>%
      dplyr::group_by(tender_year) %>%
      dplyr::summarise(
        n_unique_tender_id = dplyr::n_distinct(tender_id),
        .groups = "drop"
      )
  } else {
    NULL
  }
  
  # 4) Number of unique bidders (using bidder_masterid)
  n_unique_bidders <- if ("bidder_masterid" %in% names(df)) {
    dplyr::n_distinct(df$bidder_masterid, na.rm = TRUE)
  } else {
    NA_integer_
  }
  
  # 5) List of variables present (excluding ind_* variables)
  vars_present <- names(df)
  vars_present <- vars_present[!startsWith(vars_present, "ind_")]
  
  summary_stats <- list(
    n_obs_per_year      = n_obs_per_year,
    n_unique_buyers     = n_unique_buyers,
    tender_year_tenders = tender_year_tenders,
    n_unique_bidders    = n_unique_bidders,
    vars_present        = vars_present
  )
  
  cat("---------- DATA SUMMARY FOR", country_code, "----------\n\n")
  
  cat("Contracts per year:\n")
  print(n_obs_per_year)
  cat("\n")
  
  if (!is.na(n_unique_buyers)) {
    cat("Number of unique buyers (buyer_masterid): ", n_unique_buyers, "\n\n")
  } else {
    cat("Column 'buyer_masterid' not found: cannot compute number of unique buyers.\n\n")
  }
  
  if (!is.null(tender_year_tenders)) {
    cat("Number of unique tenders per tender_year:\n")
    print(tender_year_tenders)
    cat("\n")
  } else {
    cat("Column 'tender_id' not found: cannot compute unique tenders per year.\n\n")
  }
  
  if (!is.na(n_unique_bidders)) {
    cat("Number of unique bidders (bidder_masterid): ", n_unique_bidders, "\n\n")
  } else {
    cat("Column 'bidder_masterid' not found: cannot compute number of unique bidders.\n\n")
  }
  
  cat("Variables present (excluding indicators):\n")
  print(vars_present)
  cat("\n-----------------------------------------------\n\n")
  
  # ----------------------------------------------------------------------
  # Pre-initialize optional objects so they always exist (for results list)
  # ----------------------------------------------------------------------
  missing_by_buyer        <- NULL
  missing_by_buyer_plot   <- NULL
  top10_buyers_missing    <- NULL
  missing_buyers          <- NULL
  combined_buyers_plot    <- NULL
  missing_by_proc         <- NULL
  missing_by_proc_plot    <- NULL
  org_interop_missing     <- NULL
  
  df_cpv                  <- NULL
  unusual_matrix          <- NULL
  market_network_plot     <- NULL
  supplier_unusual        <- NULL
  supplier_unusual_plot   <- NULL
  market_unusual          <- NULL
  market_unusual_plot     <- NULL
  
  buyer_supplier_conc     <- NULL
  bc_overall              <- NULL
  bc_yearly               <- NULL
  
  buyer_missing_share     <- NULL
  buyer_singleb_rate      <- NULL
  buyer_controls          <- NULL
  buyer_analysis          <- NULL
  singleb_fe_model        <- NULL
  singleb_plot            <- NULL
  
  region_map_data         <- NULL
  region_missing_map      <- NULL
  missing_by_region       <- NULL
  
  rel_price_data          <- NULL
  rel_price_model         <- NULL
  rel_price_plot          <- NULL
  
  # ----------------------------------------------------------------------
  # Overall missing-value shares
  # ----------------------------------------------------------------------
  missing_shares <- df %>%
    summarise_missing_shares(cols = !dplyr::starts_with("ind_"))
  
  missing_shares_long <- missing_shares %>%
    pivot_missing_long()
  
  missing_plot <- plot_missing_bar(
    data_long    = missing_shares_long,
    label_lookup = label_lookup,
    title        = paste("Missing Value Share per Variable –", country_code)
  )
  
  ggplot2::ggsave(
    filename = file.path(output_dir, paste0("missing_shares_plot_", country_code, ".png")),
    plot     = missing_plot,
    width    = 10,
    height   = 6,
    dpi      = 300
  )
  
  # ----------------------------------------------------------------------
  # Organization-level missing share for matching to other registers
  # ----------------------------------------------------------------------
  org_interop_missing <- compute_org_missing(df)
  
  # ----------------------------------------------------------------------
  # Missing shares by buyer type (if buyer_buyertype exists)
  # ----------------------------------------------------------------------
  if ("buyer_buyertype" %in% names(df)) {
    missing_by_buyer <- df %>%
      dplyr::mutate(
        buyer_group = add_buyer_group(buyer_buyertype)
      ) %>%
      dplyr::group_by(buyer_group) %>%
      summarise_missing_shares(cols = -dplyr::starts_with("ind_")) %>%
      pivot_missing_long(id_vars = "buyer_group") %>%
      dplyr::mutate(
        variable_label = label_with_lookup(variable, label_lookup)
      )
    
    missing_by_buyer_plot <- plot_missing_heatmap(
      data     = missing_by_buyer,
      x_var    = "buyer_group",
      y_var    = "variable_label",
      fill_var = "missing_share",
      title    = paste("Missing value share per variable by buyer type –", country_code),
      x_lab    = "Buyer type",
      y_lab    = "Variable"
    )
  }
  
  # ----------------------------------------------------------------------
  # Top buyers with highest overall missing share
  # ----------------------------------------------------------------------
  if (all(c("buyer_masterid", "buyer_buyertype") %in% names(df))) {
    top10_buyers_missing <- df %>%
      dplyr::select(!dplyr::starts_with("ind_")) %>%
      dplyr::group_by(buyer_masterid, buyer_buyertype) %>%
      dplyr::filter(dplyr::n() >= 100) %>%  # ADJUST: threshold for minimum obs per buyer
      dplyr::summarise(
        missing_share = mean(is.na(dplyr::cur_data_all()), na.rm = TRUE),
        .groups       = "drop"
      ) %>%
      dplyr::arrange(dplyr::desc(missing_share)) %>%
      dplyr::slice_head(n = 10) %>%
      dplyr::mutate(
        buyer_label = paste0(
          buyer_masterid, "\n",
          ifelse(is.na(buyer_buyertype), "Unknown Type", buyer_buyertype)
        ),
        buyer_label_short = dplyr::if_else(
          nchar(buyer_masterid) > 20,
          paste0(substr(buyer_masterid, 1, 20), "…\n",
                 ifelse(is.na(buyer_buyertype), "Unknown Type", buyer_buyertype)),
          buyer_label
        )
      )
    
    missing_buyers <- plot_top_bar(
      df        = top10_buyers_missing,
      x_var     = "buyer_label_short",  # <- show name with type below it
      y_var     = "missing_share",
      label_var = "missing_share",
      title     = paste("Top 10 buyers with highest overall missing share –", country_code),
      x_lab     = "Buyer",
      y_lab     = "Missing share",
      fill_color = "skyblue1",
      y_limit   = c(0, 1.05),
      percent   = TRUE
    )
  }
  
  
  
  if (!is.null(missing_by_buyer_plot) && !is.null(missing_buyers)) {
    combined_buyers_plot <- missing_by_buyer_plot + missing_buyers +
      patchwork::plot_layout(nrow = 2)
    
    ggplot2::ggsave(
      filename = file.path(output_dir, paste0("buyers_missing_", country_code, ".png")),
      plot     = combined_buyers_plot,
      width    = 12,
      height   = 12,
      dpi      = 300
    )
  }
  
  # ----------------------------------------------------------------------
  # Missing shares by procedure type (if tender_proceduretype exists)
  # ----------------------------------------------------------------------
  if ("tender_proceduretype" %in% names(df)) {
    
    # Define readable labels for procurement procedure types
    procedure_type_labels <- c(
      "OPEN"                           = "Open (competitive bidding)",
      "RESTRICTED"                     = "Restricted (limited competition)",
      "NEGOTIATED_WITH_PUBLICATION"    = "Negotiated with publication (limited competition)",
      "NEGOTIATED_WITHOUT_PUBLICATION" = "Negotiated without publication (no competition)",
      "NEGOTIATED"                     = "Negotiated (limited competition)",
      "COMPETITIVE_DIALOG"             = "Competitive dialogue (limited competition)",
      "OUTRIGHT_AWARD"                 = "Outright award (direct purchase)",
      "OTHER"                          = "Other (special or exceptional procedures)",
      "Missing procedure type"         = "Missing procedure type"
    )
    
    missing_by_proc <- df %>%
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
        variable_label   = label_with_lookup(variable, label_lookup),
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
    
    missing_by_proc_plot <- plot_missing_heatmap(
      data     = missing_by_proc,
      x_var    = "proc_group_label",
      y_var    = "variable_label",
      fill_var = "missing_share",
      title    = paste("Missing value share per variable by procedure type –", country_code),
      x_lab    = "Procedure Type",
      y_lab    = "Variable"
    )
    
    ggplot2::ggsave(
      filename = file.path(output_dir, paste0("missing_by_proc_", country_code, ".png")),
      plot     = missing_by_proc_plot,
      width    = 10,
      height   = 6,
      dpi      = 300
    )
  }
  
  
  # ----------------------------------------------------------------------
  # Correlation of missing share across variables
  # ----------------------------------------------------------------------
  miss_matrix <- df %>%
    dplyr::select(!dplyr::starts_with("ind_")) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ as.numeric(is.na(.))))
  
  miss_corr <- corrr::correlate(miss_matrix)
  
  miss_corr_long <- miss_corr %>%
    corrr::stretch(na.rm = TRUE)
  
  corr_plot <- ggplot2::ggplot(
    miss_corr_long,
    ggplot2::aes(
      x    = x,
      y    = y,
      fill = r
    )
  ) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient2(
      low      = "blue",
      mid      = "white",
      high     = "red",
      midpoint = 0,
      limits   = c(-1, 1)
    ) +
    ggplot2::coord_equal() +
    ggplot2::labs(
      title = paste("Correlation of NAs across variables –", country_code),
      x     = "Variable",
      y     = "Variable",
      fill  = "Correlation"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)
    )
  
  ggplot2::ggsave(
    filename = file.path(output_dir, paste0("missing_corr_", country_code, ".png")),
    plot     = corr_plot,
    width    = 10,
    height   = 8,
    dpi      = 300
  )
  
  miss_corr_top <- miss_corr_long %>%
    dplyr::filter(x != y) %>%
    dplyr::arrange(dplyr::desc(abs(r))) %>%
    dplyr::slice_head(n = 20)
  # NOTE: no longer printed; just stored in results
  
  # ----------------------------------------------------------------------
  # Missing shares over time (by tender year)
  # ----------------------------------------------------------------------
  missing_by_year <- df %>%
    dplyr::filter(!is.na(tender_year)) %>%
    dplyr::group_by(tender_year) %>%
    summarise_missing_shares(cols = !dplyr::starts_with("ind_")) %>%
    pivot_missing_long(id_vars = "tender_year") %>%
    dplyr::mutate(
      variable_label = label_with_lookup(variable, label_lookup)
    )
  
  top_vars <- missing_by_year %>%
    dplyr::group_by(variable_label) %>%
    dplyr::summarise(
      avg_missing = mean(missing_share, na.rm = TRUE),
      .groups     = "drop"
    ) %>%
    dplyr::slice_max(avg_missing, n = 10) %>%
    dplyr::pull(variable_label)
  
  year_miss <- missing_by_year %>%
    dplyr::filter(variable_label %in% top_vars) %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x     = tender_year,
        y     = missing_share,
        color = variable_label
      )
    ) +
    ggplot2::geom_line(size = 1.2) +
    ggplot2::geom_point(size = 2) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    ggplot2::labs(
      title = paste("Trends in missing shares over time (top 10 variables) –", country_code),
      x     = "Year",
      y     = "Missing share",
      color = "Variable"
    ) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      plot.title      = ggplot2::element_text(size = 18, face = "bold"),
      legend.position = "bottom"
    )
  
  ggplot2::ggsave(
    filename = file.path(output_dir, paste0("year_miss_", country_code, ".png")),
    plot     = year_miss,
    width    = 10,
    height   = 6,
    dpi      = 300
  )
  
  # ----------------------------------------------------------------------
  # Unusual supplier market entries by CPV cluster (if columns exist)
  # ----------------------------------------------------------------------
  if (all(c("tender_id", "lot_number", "bidder_id", "lot_productcode", "bid_priceusd") %in% names(df))) {
    
    df_cpv <- build_cpv_df(df, cpv_digits = 3)  # ADJUST cpv_digits (2,3,4...)
    
    bidder_cpv_stats <- df_cpv %>%
      dplyr::group_by(bidder_id, cpv_cluster) %>%
      dplyr::summarise(
        n_awards    = dplyr::n(),
        total_value = sum(bid_priceusd, na.rm = TRUE),
        .groups     = "drop"
      ) %>%
      dplyr::group_by(bidder_id) %>%
      dplyr::mutate(
        bidder_total_awards = sum(n_awards),
        bidder_total_value  = sum(total_value),
        share_awards        = n_awards / bidder_total_awards,
        share_value         = ifelse(
          bidder_total_value > 0,
          total_value / bidder_total_value,
          NA_real_
        )
      ) %>%
      dplyr::ungroup()
    
    df_with_flags <- df_cpv %>%
      dplyr::left_join(
        bidder_cpv_stats %>%
          dplyr::select(
            bidder_id,
            cpv_cluster,
            n_awards,
            bidder_total_awards,
            share_awards,
            share_value
          ),
        by = c("bidder_id", "cpv_cluster")
      ) %>%
      dplyr::mutate(
        enough_history   = bidder_total_awards >= 4,  # ADJUST: history threshold
        cpv_cluster_atypical =
          enough_history &
          share_awards < 0.05 &                       # ADJUST: marginal share
          n_awards <= 3                               # ADJUST: max wins in atypical cluster
      )
    
    cluster_supplier_counts <- df_cpv %>%
      dplyr::group_by(cpv_cluster, bidder_id) %>%
      dplyr::summarise(
        n_ic   = dplyr::n(),
        .groups = "drop"
      )
    
    cluster_totals <- df_cpv %>%
      dplyr::group_by(cpv_cluster) %>%
      dplyr::summarise(
        n_c           = dplyr::n(),
        n_suppliers_c = dplyr::n_distinct(bidder_id),
        .groups       = "drop"
      )
    
    cluster_surprise <- cluster_supplier_counts %>%
      dplyr::left_join(cluster_totals, by = "cpv_cluster") %>%
      dplyr::mutate(
        p_i_given_c    = (n_ic + 1) / (n_c + n_suppliers_c),  # Laplace smoothing
        surprise_score = -log(p_i_given_c)
      )
    
    df_with_surprise <- df_with_flags %>%
      dplyr::left_join(
        cluster_surprise %>%
          dplyr::select(cpv_cluster, bidder_id, surprise_score),
        by = c("cpv_cluster", "bidder_id")
      ) %>%
      dplyr::mutate(
        surprise_z = ifelse(
          !is.na(surprise_score),
          (surprise_score - mean(surprise_score, na.rm = TRUE)) /
            sd(surprise_score, na.rm = TRUE),
          NA_real_
        )
      )
    
    home_market <- bidder_cpv_stats %>%
      dplyr::group_by(bidder_id) %>%
      dplyr::slice_max(n_awards, n = 1, with_ties = FALSE) %>%
      dplyr::ungroup() %>%
      dplyr::transmute(
        bidder_id,
        home_cpv_cluster = cpv_cluster
      )
    
    df_unusual <- df_with_surprise %>%
      dplyr::left_join(home_market, by = "bidder_id") %>%
      dplyr::mutate(target_cpv_cluster = cpv_cluster) %>%
      dplyr::filter(
        enough_history,
        cpv_cluster_atypical,
        !is.na(surprise_z)
      )
    
    unusual_matrix <- df_unusual %>%
      dplyr::group_by(home_cpv_cluster, target_cpv_cluster) %>%
      dplyr::summarise(
        n_bidders     = dplyr::n_distinct(bidder_id),
        n_awards      = dplyr::n(),
        mean_surprise = mean(surprise_z, na.rm = TRUE),
        .groups       = "drop"
      )
    
    edges_markets <- unusual_matrix %>%
      dplyr::filter(n_bidders >= 4) %>%  # ADJUST: min number of bidders for edge
      dplyr::rename(
        from = home_cpv_cluster,
        to   = target_cpv_cluster
      )
    
    if (nrow(edges_markets) > 0) {
      g_markets <- igraph::graph_from_data_frame(edges_markets, directed = TRUE)
      
      market_network_plot <- ggraph::ggraph(g_markets, layout = "fr") +
        ggraph::geom_edge_link(
          ggplot2::aes(width = n_bidders, colour = mean_surprise),
          arrow   = grid::arrow(length = grid::unit(0.15, "cm")),
          end_cap = ggraph::circle(2, "mm"),
          alpha   = 0.6
        ) +
        ggraph::geom_node_point(size = 4, colour = "darkorange") +
        ggraph::geom_node_text(ggplot2::aes(label = name), repel = TRUE, size = 3) +
        ggraph::scale_edge_width(range = c(0.2, 2)) +
        ggraph::scale_edge_colour_continuous(
          low  = "green",
          high = "red",
          name = "Mean surprise score (standardised)"
        ) +
        ggplot2::labs(
          title    = paste("Network of unusual market entries –", country_code),
          subtitle = "Nodes represent CPV clusters; edges show atypical flow of bidders between markets"
        ) +
        ggplot2::theme_void()
      
      ggplot2::ggsave(
        filename = file.path(output_dir, paste0("supp_net_", country_code, ".png")),
        plot     = market_network_plot,
        width    = 10,
        height   = 6,
        dpi      = 300
      )
    } else {
      message("No edges for unusual market entries network (", country_code, ").")
    }
  }
  
  # ------------------------------------------------------------------
  # Supplier / market unusualness (only if df_unusual exists)
  # ------------------------------------------------------------------
  if (exists("df_unusual") && !is.null(df_unusual) && nrow(df_unusual) > 0) {
    
    # Supplier-level "unusualness": who behaves strangely?
    supplier_unusual <- df_unusual %>%
      dplyr::group_by(bidder_id, home_cpv_cluster) %>%
      dplyr::summarise(
        max_surprise_z      = max(surprise_z, na.rm = TRUE),
        mean_surprise_z     = mean(surprise_z, na.rm = TRUE),
        n_atypical_markets  = dplyr::n_distinct(target_cpv_cluster),
        n_atypical_awards   = dplyr::n(),
        .groups             = "drop"
      )
    
    # focus on the most "interesting" suppliers (adjust thresholds as needed)
    top_suppliers <- supplier_unusual %>%
      dplyr::filter(n_atypical_awards >= 3) %>%         # at least 3 atypical awards
      dplyr::arrange(dplyr::desc(max_surprise_z)) %>%
      dplyr::slice_head(n = 30)                         # top 30 by max surprise
    
    supplier_unusual_plot <- ggplot2::ggplot(
      top_suppliers,
      ggplot2::aes(
        x = reorder(bidder_id, max_surprise_z),
        y = max_surprise_z,
        fill = n_atypical_markets
      )
    ) +
      ggplot2::geom_col() +
      ggplot2::coord_flip() +
      ggplot2::scale_fill_viridis_c(
        option = "C",
        name   = "No. atypical\nmarkets"
      ) +
      ggplot2::labs(
        title    = paste("Top suppliers by unusual market behaviour –", country_code),
        subtitle = "Max standardised surprise score across atypical CPV entries\nColour indicates how many distinct target markets they enter unusually",
        x        = "Supplier ID",
        y        = "Max surprise z-score"
      ) +
      ggplot2::theme_bw(base_size = 11)
    
    ggplot2::ggsave(
      filename = file.path(output_dir, paste0("supp_unusual_suppliers_", country_code, ".png")),
      plot     = supplier_unusual_plot,
      width    = 9,
      height   = 7,
      dpi      = 300
    )
    
    # Market-level "unusualness": which CPV clusters attract unusual entries?
    market_unusual <- df_unusual %>%
      dplyr::group_by(target_cpv_cluster) %>%
      dplyr::summarise(
        mean_surprise_z      = mean(surprise_z, na.rm = TRUE),
        max_surprise_z       = max(surprise_z, na.rm = TRUE),
        n_unusual_suppliers  = dplyr::n_distinct(bidder_id),
        n_unusual_awards     = dplyr::n(),
        .groups              = "drop"
      ) %>%
      dplyr::mutate(
        market_risk_index = mean_surprise_z * log1p(n_unusual_suppliers)
      )
    
    top_markets <- market_unusual %>%
      dplyr::slice_max(market_risk_index, n = 30)
    
    market_unusual_plot <- ggplot2::ggplot(
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
      ggrepel::geom_text_repel(size = 3, colour = "gray20") +
      ggplot2::scale_size_continuous(
        name = "No. atypical awards",
        range = c(3, 12)
      ) +
      ggplot2::scale_colour_gradientn(
        colours = c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c"),
        name = "Mean surprise (z-score)",
        limits = c(min(top_markets$mean_surprise_z, na.rm = TRUE),
                   max(top_markets$mean_surprise_z, na.rm = TRUE))
      ) +
      ggplot2::labs(
        title    = paste("Markets attracting unusual supplier entries –", country_code),
        subtitle = "Each point is a CPV 3-digit cluster\nx = number of unusual suppliers; y = mean surprise; color = mean surprise; size = number of atypical awards",
        x        = "Number of suppliers entering atypically",
        y        = "Mean surprise z-score"
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        plot.title      = ggplot2::element_text(face = "bold", size = 13, colour = "#222222"),
        plot.subtitle   = ggplot2::element_text(size = 11, colour = "#444444"),
        legend.position = "right",
        panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_line(colour = "grey85", linewidth = 0.3),
        axis.title      = ggplot2::element_text(face = "bold"),
        axis.text       = ggplot2::element_text(colour = "#333333")
      )
    
    ggplot2::ggsave(
      filename = file.path(output_dir, paste0("supp_unusual_markets_", country_code, ".png")),
      plot     = market_unusual_plot,
      width    = 9,
      height   = 7,
      dpi      = 300
    )
    
  } else {
    # keep these NULL so the results list is consistent
    supplier_unusual      <- NULL
    supplier_unusual_plot <- NULL
    market_unusual        <- NULL
    market_unusual_plot   <- NULL
  }
  
  # ----------------------------------------------------------------------
  # Buyer–supplier concentration (overall and by year)
  # ----------------------------------------------------------------------
  if (all(c("buyer_masterid", "bidder_masterid", "bid_priceusd") %in% names(df))) {
    buyer_supplier_conc <- df %>%
      dplyr::group_by(tender_year, buyer_masterid, bidder_masterid) %>%
      dplyr::summarise(
        n_contracts = dplyr::n(),
        total_spend = sum(bid_priceusd, na.rm = TRUE),
        .groups     = "drop"
      ) %>%
      dplyr::group_by(tender_year, buyer_masterid) %>%
      dplyr::mutate(
        buyer_total_spend    = sum(total_spend, na.rm = TRUE),
        buyer_total_contract = dplyr::n(),
        buyer_concentration  = ifelse(
          buyer_total_spend > 0,
          total_spend / buyer_total_spend,
          NA_real_
        )
      ) %>%
      dplyr::ungroup()
    
    buyer_supplier_conc <- buyer_supplier_conc %>%
      dplyr::group_by(tender_year, buyer_masterid) %>%
      dplyr::mutate(
        n_suppliers                 = dplyr::n(),
        buyer_concentration_display = ifelse(
          n_suppliers < 3, NA_real_, buyer_concentration  # ADJUST: min suppliers
        )
      ) %>%
      dplyr::ungroup() %>%
      tidyr::drop_na()
    
    top_buyers <- buyer_supplier_conc %>%
      dplyr::group_by(buyer_masterid) %>%
      dplyr::summarise(
        max_conc        = max(buyer_concentration_display, na.rm = TRUE),
        total_contracts = sum(n_contracts, na.rm = TRUE),
        .groups         = "drop"
      ) %>%
      dplyr::slice_max(max_conc, n = 50)
    
    bc_overall <- ggplot2::ggplot(
      top_buyers,
      ggplot2::aes(
        x = reorder(buyer_masterid, max_conc),
        y = max_conc
      )
    ) +
      ggplot2::geom_col(fill = "skyblue1") +
      ggplot2::geom_text(
        ggplot2::aes(label = total_contracts),
        hjust = -0.1,
        size  = 3.2
      ) +
      ggplot2::coord_flip() +
      ggplot2::scale_x_discrete(
        labels = function(x) substr(x, 1, 8)
      ) +
      ggplot2::theme_bw() +
      ggplot2::labs(
        title    = paste("Top 50 Buyers by Maximum Supplier Concentration –", country_code),
        subtitle = "Labels show total number of contracts procured by buyer",
        x        = "Buyer ID",
        y        = "Max share of spending to one supplier"
      ) +
      ggplot2::ylim(0, 1.05)
    
    ggplot2::ggsave(
      filename = file.path(output_dir, paste0("bc_overall_", country_code, ".png")),
      plot     = bc_overall,
      width    = 10,
      height   = 8,
      dpi      = 300
    )
    
    top_buyers_yearly <- buyer_supplier_conc %>%
      dplyr::filter(tender_year > 2014) %>%  # ADJUST: year cutoff
      dplyr::group_by(tender_year, buyer_masterid) %>%
      dplyr::summarise(
        max_conc        = max(buyer_concentration_display, na.rm = TRUE),
        total_contracts = sum(n_contracts, na.rm = TRUE),
        .groups         = "drop"
      ) %>%
      dplyr::group_by(tender_year) %>%
      dplyr::slice_max(max_conc, n = 30) %>%  # ADJUST: top N buyers per year
      dplyr::ungroup()
    
    repeated_buyers <- top_buyers_yearly %>%
      dplyr::count(buyer_masterid) %>%
      dplyr::filter(n > 1) %>%
      dplyr::transmute(buyer_masterid, repeated = TRUE)
    
    top_buyers_yearly <- top_buyers_yearly %>%
      dplyr::left_join(repeated_buyers, by = "buyer_masterid") %>%
      dplyr::mutate(repeated = dplyr::if_else(is.na(repeated), FALSE, repeated))
    
    bc_yearly <- ggplot2::ggplot(
      top_buyers_yearly,
      ggplot2::aes(
        x     = tidytext::reorder_within(buyer_masterid, max_conc, tender_year),
        y     = max_conc,
        fill  = repeated
      )
    ) +
      ggplot2::geom_col() +
      ggplot2::geom_text(
        ggplot2::aes(label = total_contracts, color = repeated),
        hjust       = -0.1,
        size        = 2.8,
        show.legend = FALSE
      ) +
      ggplot2::coord_flip() +
      ggplot2::facet_wrap(~ tender_year, scales = "free_y") +
      tidytext::scale_x_reordered(
        labels = function(x) substr(x, 1, 8)
      ) +
      ggplot2::scale_fill_manual(
        values = c(`FALSE` = "skyblue1", `TRUE` = "red"),
        name   = "Buyer appears\nin multiple years",
        labels = c("No", "Yes")
      ) +
      ggplot2::scale_color_manual(
        values = c(`FALSE` = "black", `TRUE` = "red4")
      ) +
      ggplot2::theme_bw(base_size = 11) +
      ggplot2::labs(
        title    = paste("Top 30 Buyers by Maximum Spending Concentration per Year –", country_code),
        subtitle = "Red bars = buyers that appear in the top list in multiple years\nLabels show total number of contracts per year-buyer",
        x        = "Buyer ID",
        y        = "Max share of spending to one supplier"
      ) +
      ggplot2::ylim(0, 1.05)
    
    ggplot2::ggsave(
      filename = file.path(output_dir, paste0("bc_yearly_", country_code, ".png")),
      plot     = bc_yearly,
      width    = 10,
      height   = 8,
      dpi      = 300
    )
  }
  
  # ----------------------------------------------------------------------
  # Effect of non-transparency on competition (FE regression)
  # ----------------------------------------------------------------------
  if ("ind_corr_singleb" %in% names(df)) {
    
    df <- df %>%
      dplyr::mutate(
        ind_corr_singleb = dplyr::if_else(
          !is.na(ind_corr_singleb),
          ind_corr_singleb / 100,
          NA_real_
        )
      )
    
    # ---- Country-specific year filter ------------------------------
    yr <- get_year_range(country_code, component = "singleb")
    
    df_filtered <- df %>%
      dplyr::filter(
        !is.na(tender_year),
        tender_year >= yr$min_year,
        tender_year <= yr$max_year
      )
    
    if (nrow(df_filtered) == 0L) {
      message("No observations after year filter for ", country_code, " (singleb).")
      singleb_fe_model <- NULL
      singleb_plot     <- NULL
      
    } else {
      
      # --- Compute missing-share indicator per buyer-year -------------
      buyer_missing_share <- df_filtered %>%
        dplyr::group_by(buyer_masterid, tender_year) %>%
        summarise_missing_shares(cols = !dplyr::starts_with("ind_")) %>%
        dplyr::mutate(
          cumulative_missing_share = rowMeans(
            dplyr::across(dplyr::ends_with("_missing_share")),
            na.rm = TRUE
          )
        )
      
      # --- Compute single-bidding share per buyer-year ----------------
      buyer_singleb_rate <- df_filtered %>%
        dplyr::group_by(buyer_masterid, tender_year) %>%
        dplyr::summarise(
          cumulative_singleb_rate = mean(ind_corr_singleb, na.rm = TRUE),
          .groups = "drop"
        )
      
      # --- Basic controls ---------------------------------------------
      buyer_controls <- df_filtered %>%
        dplyr::group_by(buyer_masterid, tender_year, buyer_buyertype) %>%
        dplyr::summarise(
          n_contracts          = dplyr::n(),
          avg_contract_value   = mean(bid_priceusd, na.rm = TRUE),
          total_contract_value = sum(bid_priceusd, na.rm = TRUE),
          .groups              = "drop"
        )
      
      # --- Merge datasets ---------------------------------------------
      buyer_analysis <- buyer_missing_share %>%
        dplyr::select(buyer_masterid, tender_year, cumulative_missing_share) %>%
        dplyr::inner_join(buyer_singleb_rate, by = c("buyer_masterid", "tender_year")) %>%
        dplyr::inner_join(buyer_controls,     by = c("buyer_masterid", "tender_year")) %>%
        dplyr::mutate(
          buyer_buyertype = forcats::fct_explicit_na(as.factor(buyer_buyertype), "Unknown")
        )
      
      # --- Keep only buyers with >=3 distinct years ----------------
      buyer_year_span <- buyer_analysis %>%
        dplyr::group_by(buyer_masterid) %>%
        dplyr::summarise(
          n_years = dplyr::n_distinct(tender_year),
          .groups = "drop"
        )
      
      eligible_buyers <- buyer_year_span %>%
        dplyr::filter(n_years >= 3L) %>%
        dplyr::pull(buyer_masterid)
      
      buyer_analysis_fe <- buyer_analysis %>%
        dplyr::filter(buyer_masterid %in% eligible_buyers)
      
      if (nrow(buyer_analysis_fe) == 0L) {
        message("No buyers with ≥3 years of data for ", country_code, " (singleb).")
        singleb_fe_model <- NULL
        singleb_plot     <- NULL
        
      } else {
        # --- FE model: buyer + year fixed effects -------------------
        singleb_fe_model <- fixest::feglm(
          cumulative_singleb_rate ~ cumulative_missing_share +
            log1p(n_contracts) +
            log1p(avg_contract_value) |
            buyer_masterid + tender_year,
          family = quasibinomial(link = "logit"),
          data   = buyer_analysis_fe
        )
        
        # --- Extract summary info SAFELY ----------------------------
        model_sum <- summary(singleb_fe_model)
        n_obs     <- model_sum$nobs
        
        coef_df <- as.data.frame(model_sum$coeftable)
        
        # default values if the coefficient is not in the table
        coef_est <- NA_real_
        coef_p   <- NA_real_
        stars    <- ""
        
        if ("cumulative_missing_share" %in% rownames(coef_df)) {
          coef_row <- coef_df["cumulative_missing_share", , drop = FALSE]
          
          # protect against numeric(0)
          if (nrow(coef_row) == 1L) {
            coef_est <- as.numeric(coef_row$Estimate)
            coef_p   <- as.numeric(coef_row$`Pr(>|z|)`)
            
            # significance stars – only if coef_p is a single non-NA number
            if (!is.na(coef_p) && length(coef_p) == 1L) {
              if (coef_p < 0.001) {
                stars <- "***"
              } else if (coef_p < 0.01) {
                stars <- "**"
              } else if (coef_p < 0.05) {
                stars <- "*"
              } else {
                stars <- ""
              }
            }
          }
        }
        
        year_min <- min(buyer_analysis_fe$tender_year, na.rm = TRUE)
        year_max <- max(buyer_analysis_fe$tender_year, na.rm = TRUE)
        
        # --- Model text for plot ------------------------------------
        model_subtitle <- sprintf(
          "Fractional logit FE (buyer + year) | N = %s | Years: %s–%s",
          formatC(n_obs, big.mark = ",", format = "f", digits = 0),
          year_min,
          year_max
        )
        
        if (isTRUE(!is.na(coef_est) && length(coef_est) == 1L && !is.na(coef_p))) {
          model_caption <- sprintf(
            "Coeff. on missing share: %.3f%s (p = %.3f)\nNote: Only buyers with ≥3 years of data; FE for buyer and year.",
            coef_est,
            stars,
            coef_p
          )
        } else {
          model_caption <- "Coeff. on missing share not available (dropped or unidentified).\nNote: Only buyers with ≥3 years of data; FE for buyer and year."
        }
        
        # --- Predicted relationship plot (with guard) ---------------
        pred_singleb <- tryCatch(
          ggeffects::ggpredict(
            singleb_fe_model,
            terms = "cumulative_missing_share"
          ),
          error = function(e) {
            message("ggpredict failed for ", country_code, " (singleb): ", e$message)
            NULL
          }
        )
        
        if (is.null(pred_singleb)) {
          singleb_plot <- NULL
        } else {
          singleb_plot <- plot_ggeffects_line(
            pred      = pred_singleb,
            title     = paste("Predicted Single-Bidding by Missing Share –", country_code),
            subtitle  = model_subtitle,
            x_lab     = "Buyer Missing Share",
            y_lab     = "Predicted Single-Bidding Share",
            caption   = model_caption
          )
          
          ggplot2::ggsave(
            filename = file.path(output_dir, paste0("reg_singleb_missing_share_", country_code, ".png")),
            plot     = singleb_plot,
            width    = 10,
            height   = 8,
            dpi      = 300
          )
        }
      }
    }
  }
  
  # ----------------------------------------------------------------------
  # Region-level missing share map (NUTS3-style; mainly EU)
  # ----------------------------------------------------------------------
  if ("buyer_nuts" %in% names(df)) {
    df_nuts <- df %>%
      clean_nuts3(buyer_nuts)
    
    # (no more print(table(...)) here)
    
    missing_by_region <- df_nuts %>%
      dplyr::filter(!is.na(nuts3)) %>%
      dplyr::select(!dplyr::starts_with("ind_")) %>%
      dplyr::group_by(nuts3) %>%
      dplyr::summarise(
        missing_share = mean(is.na(dplyr::cur_data_all()), na.rm = TRUE),
        .groups       = "drop"
      )
    
    region_sf <- eurostat::get_eurostat_geospatial(
      output_class = "sf",
      nuts_level   = 3,
      year         = 2021
    ) %>%
      dplyr::filter(CNTR_CODE == country_code)
    
    region_map_data <- region_sf %>%
      dplyr::left_join(missing_by_region, by = c("NUTS_ID" = "nuts3"))
    
    region_missing_map <- ggplot2::ggplot(region_map_data) +
      ggplot2::geom_sf(ggplot2::aes(fill = missing_share), color = "grey40", size = 0.2) +
      ggplot2::scale_fill_distiller(
        palette   = "Blues",
        direction = 1,
        na.value  = "grey90",
        labels    = scales::percent_format(accuracy = 1),
        name      = "Missing share"
      ) +
      ggplot2::labs(
        title = paste("Overall share of missing values by region (NUTS3) –", country_code)
      ) +
      ggplot2::theme_minimal()
    
    ggplot2::ggsave(
      filename = file.path(output_dir, paste0("region_missing_map_", country_code, ".png")),
      plot     = region_missing_map,
      width    = 10,
      height   = 6,
      dpi      = 300
    )
  }
  
  # ----------------------------------------------------------------------
  # Effect on relative price (generic), if price variables exist
  # ----------------------------------------------------------------------
  if (all(c("bid_price", "lot_estimatedprice") %in% names(df))) {
    key_vars <- names(label_lookup) %>%
      gsub("_missing_share", "", .)
    
    df <- df %>%
      dplyr::mutate(
        total_missing_share = rowMeans(
          dplyr::across(all_of(key_vars), ~ is.na(.)),
          na.rm = TRUE
        )
      )
    
    # --- NEW: country- and component-specific year filter -------------
    yr <- get_year_range(country_code, component = "rel_price")
    
    rel_price_data <- df %>%
      dplyr::mutate(
        relative_price = bid_price / lot_estimatedprice,
        relative_price = ifelse(
          relative_price > 5 | relative_price <= 0,
          NA,
          relative_price
        )
      ) %>%
      dplyr::filter(
        !is.na(tender_year),
        tender_year >= yr$min_year,
        tender_year <= yr$max_year
      )
    # ------------------------------------------------------------------
    
    if ("tender_proceduretype" %in% names(rel_price_data)) {
      rel_price_data <- rel_price_data %>%
        dplyr::mutate(
          tender_proceduretype = stats::relevel(
            as.factor(tender_proceduretype),
            ref = "OPEN"
          )
        )
    }
    if ("buyer_buyertype" %in% names(rel_price_data)) {
      rel_price_data <- rel_price_data %>%
        dplyr::mutate(
          buyer_buyertype = stats::relevel(
            as.factor(buyer_buyertype),
            ref = "UTILITIES"
          )
        )
    }
    
    rel_price_model <- fixest::feols(
      relative_price ~ total_missing_share +
        buyer_buyertype +
        tender_proceduretype |
        tender_year,
      data    = rel_price_data,
      cluster = ~ buyer_masterid
    )
    
    pred_price <- ggeffects::ggpredict(rel_price_model, terms = "total_missing_share")
    
    rel_price_plot <- plot_ggeffects_line(
      pred      = pred_price,
      title     = paste("Predicted Relative Price by Missing Share –", country_code),
      subtitle  = "Linear model with year FE (SEs clustered by buyer)\nControls: procedure type, tender year, buyer type",
      x_lab     = "Total Missing Share",
      y_lab     = "Predicted Relative Price",
      caption   = "Note: predicted relative price per contract.\nShaded area = 95% confidence interval (clustered by buyer)."
    )
    
    ggplot2::ggsave(
      filename = file.path(output_dir, paste0("reg_relative_price_missing_share_", country_code, ".png")),
      plot     = rel_price_plot,
      width    = 10,
      height   = 8,
      dpi      = 300
    )
  }
  
  # ----------------------------------------------------------------------
  # Collect outputs to return (data + plots)
  # ----------------------------------------------------------------------
  results <- list(
    country_code          = country_code,
    
    # Data / tables
    missing_shares_long   = missing_shares_long,
    missing_by_buyer      = missing_by_buyer,
    missing_by_proc       = missing_by_proc,
    miss_corr             = miss_corr,
    miss_corr_top         = miss_corr_top,
    missing_by_year       = missing_by_year,
    buyer_supplier_conc   = buyer_supplier_conc,
    unusual_matrix        = unusual_matrix,
    buyer_analysis        = buyer_analysis,
    buyer_missing_share   = buyer_missing_share,
    buyer_singleb_rate    = buyer_singleb_rate,
    buyer_controls        = buyer_controls,
    singleb_fe_model      = singleb_fe_model,
    
    # CPV-related
    cpv_data              = df_cpv,
    
    # Plots
    missing_plot          = missing_plot,
    missing_by_buyer_plot = missing_by_buyer_plot,
    missing_buyers        = missing_buyers,
    combined_buyers_plot  = combined_buyers_plot,
    missing_by_proc_plot  = missing_by_proc_plot,
    corr_plot             = corr_plot,
    year_miss             = year_miss,
    unusual_matrix        = unusual_matrix,
    market_network_plot   = market_network_plot,
    supplier_unusual      = supplier_unusual,
    supplier_unusual_plot = supplier_unusual_plot,
    market_unusual        = market_unusual,
    market_unusual_plot   = market_unusual_plot,
    bc_overall            = bc_overall,
    bc_yearly             = bc_yearly,
    singleb_plot          = singleb_plot,
    org_interop_missing   = org_interop_missing, 
    
    # Region-level outputs (NUTS3-style; NULL if no buyer_nuts)
    region_map_data       = region_map_data,
    missing_by_region     = missing_by_region,
    region_missing_map    = region_missing_map,
    
    # Relative price model (NULL if price data not available)
    rel_price_data        = rel_price_data,
    rel_price_model       = rel_price_model,
    rel_price_plot        = rel_price_plot,
    
    summary_stats         = summary_stats
  )
  
  invisible(results)
}
