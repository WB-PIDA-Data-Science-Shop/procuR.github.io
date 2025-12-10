# ========================================================================
# Administrative efficiency pipeline
# ========================================================================
# The pipeline produces:
#  - Descriptive figures on procedure mix, submission and decision periods
#  - Buyer-level breakdowns of short and long periods (by count and value)
#  - Regression-based estimates linking timing to single bidding
# ========================================================================

# ------------------------------------------------------------------------
# 0. Data loading helper
# ------------------------------------------------------------------------

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
  
  data
}

# ------------------------------------------------------------------------
# 1. Tender year helper
# ------------------------------------------------------------------------
# Extracts year information from the main tender dates and stores it in
# a numeric `tender_year` column.

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

# ------------------------------------------------------------------------
# 2. Procedure type recoding
# ------------------------------------------------------------------------
# Maps raw procedure codes to a small, interpretable set of labels.
# These labels are used consistently in descriptive plots.

recode_procedure_type <- function(x) {
  dplyr::recode(
    as.character(x),
    "OPEN"                           = "Open Procedure",
    "OUTRIGHT_AWARD"                 = "Direct Award",
    "RESTRICTED"                     = "Restricted Procedure",
    "OTHER"                          = "Other Procedures",
    "COMPETITIVE_DIALOG"             = "Competitive Dialog",
    "NEGOTIATED"                     = "Negotiated",
    "NEGOTIATED_WITHOUT_PUBLICATION" = "Negotiated without publications",
    "NEGOTIATED_WITH_PUBLICATION"    = "Negotiated with publications",
    .default                         = as.character(x)
  )
}

# ------------------------------------------------------------------------
# 3. Buyer grouping helper
# ------------------------------------------------------------------------
# Collapses granular buyer types into three broad groups used in the
# administrative efficiency figures.
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

# ------------------------------------------------------------------------
# 4. Generic "days between" helper
# ------------------------------------------------------------------------
# Computes the number of days between two date columns and filters
# out negative or implausibly long intervals (> 364 days).

compute_tender_days <- function(df, from_col, to_col, new_col) {
  from_quo   <- rlang::enquo(from_col)
  to_quo     <- rlang::enquo(to_col)
  new_col_nm <- rlang::as_name(rlang::enquo(new_col))
  
  df %>%
    dplyr::mutate(
      !!from_quo := as.Date(!!from_quo),
      !!to_quo   := as.Date(!!to_quo)
    ) %>%
    dplyr::filter(
      !is.na(!!from_quo),
      !is.na(!!to_quo)
    ) %>%
    dplyr::mutate(
      !!new_col_nm := as.numeric(!!to_quo - !!from_quo)
    ) %>%
    dplyr::filter(
      !!rlang::sym(new_col_nm) >= 0,
      !!rlang::sym(new_col_nm) < 365
    )
}

# ------------------------------------------------------------------------
# 5. Short / medium submission flags (country-specific thresholds,
#    with median fallback when thresholds are NA)
# ------------------------------------------------------------------------

add_short_deadline_flags <- function(df,
                                     days_col = tender_days_open,
                                     proc_col = tender_proceduretype,
                                     thr) {
  days_col <- rlang::enquo(days_col)
  proc_col <- rlang::enquo(proc_col)
  
  # Medians by procedure type, used as fallback if thresholds are missing
  med_open <- df %>%
    dplyr::filter(!!proc_col == "Open Procedure") %>%
    dplyr::summarise(m = stats::median(!!days_col, na.rm = TRUE)) %>%
    dplyr::pull(m)
  
  med_restricted <- df %>%
    dplyr::filter(!!proc_col == "Restricted Procedure") %>%
    dplyr::summarise(m = stats::median(!!days_col, na.rm = TRUE)) %>%
    dplyr::pull(m)
  
  med_negotiated <- df %>%
    dplyr::filter(!!proc_col == "Negotiated with publications") %>%
    dplyr::summarise(m = stats::median(!!days_col, na.rm = TRUE)) %>%
    dplyr::pull(m)
  
  # If the country thresholds are NA, use the medians instead
  short_open_cutoff <- if (is.na(thr$subm_short_open))         med_open       else thr$subm_short_open
  short_rest_cutoff <- if (is.na(thr$subm_short_restricted))   med_restricted else thr$subm_short_restricted
  short_neg_cutoff  <- if (is.null(thr$subm_short_negotiated) ||
                           is.na(thr$subm_short_negotiated))  med_negotiated else thr$subm_short_negotiated
  
  # For "medium" we only flag if both bounds are provided (still only for Open)
  medium_min <- thr$subm_medium_open_min
  medium_max <- thr$subm_medium_open_max
  use_medium <- !is.na(medium_min) & !is.na(medium_max)
  
  df %>%
    dplyr::mutate(
      short_deadline = dplyr::case_when(
        !!proc_col == "Open Procedure" &
          !!days_col < short_open_cutoff ~ TRUE,
        !!proc_col == "Restricted Procedure" &
          !!days_col < short_rest_cutoff ~ TRUE,
        !!proc_col == "Negotiated with publications" &
          !!days_col < short_neg_cutoff ~ TRUE,
        TRUE ~ FALSE
      ),
      medium_deadline = dplyr::case_when(
        use_medium &
          !!proc_col == "Open Procedure" &
          !!days_col >= medium_min &
          !!days_col <  medium_max ~ TRUE,
        TRUE ~ FALSE
      )
    )
}



# ------------------------------------------------------------------------
# 6. Long decision flag (country-specific thresholds)
# ------------------------------------------------------------------------
# Flags “long” decisions for selected procedures using a single threshold
# in days, typically representing a long administrative delay.

add_long_decision_flag <- function(df,
                                   days_col = tender_days_dec,
                                   proc_col = tender_proceduretype,
                                   thr) {
  days_col <- rlang::enquo(days_col)
  proc_col <- rlang::enquo(proc_col)
  
  df %>%
    dplyr::mutate(
      long_decision = dplyr::case_when(
        !!proc_col %in% c(
          "Open Procedure",
          "Restricted Procedure",
          "Negotiated with publications"
        ) & !!days_col >= thr$long_decision_days ~ TRUE,
        TRUE ~ FALSE
      )
    )
}

# ------------------------------------------------------------------------
# 7. Generic histogram with quartiles
# ------------------------------------------------------------------------
# Plots a histogram of time intervals (in days) and overlays quartiles.
# Can be used overall or faceted by a categorical variable (e.g. procedure).

plot_days_hist_with_quartiles <- function(data,
                                          days_var,
                                          facet_var = NULL,
                                          title,
                                          x_lab,
                                          y_lab   = "Number of tenders",
                                          caption = NULL,
                                          binwidth = 5,
                                          xlim     = c(0, 365)) {
  days_sym <- rlang::sym(days_var)
  
  base <- ggplot2::ggplot(data, ggplot2::aes(x = !!days_sym)) +
    ggplot2::geom_histogram(
      binwidth = binwidth,
      fill     = "lightblue",
      color    = "white",
      boundary = 0
    ) +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0, 0.25))
    ) +
    ggplot2::coord_cartesian(xlim = xlim, clip = "off") +
    ggplot2::labs(
      title   = title,
      x       = x_lab,
      y       = y_lab,
      caption = caption
    ) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      plot.title.position = "plot",
      plot.title          = ggplot2::element_text(margin = ggplot2::margin(b = 20)),
      plot.caption        = ggplot2::element_text(
        hjust  = 0,
        face   = "italic",
        size   = 10,
        margin = ggplot2::margin(t = 10)
      )
    )
  
  if (is.null(facet_var)) {
    # overall quartiles
    q <- stats::quantile(data[[days_var]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
    
    base +
      ggplot2::geom_vline(
        xintercept = q,
        color      = "blue",
        linetype   = c("dashed", "solid", "dashed"),
        size       = 1
      ) +
      ggplot2::annotate(
        "text",
        x     = q,
        y     = Inf,
        label = paste0(names(q), ": ", round(q, 1), " days"),
        color = "blue",
        size  = 4,
        angle = 45,
        vjust = -1,
        hjust = 0
      )
    
  } else {
    facet_sym <- rlang::sym(facet_var)
    
    q_by_facet <- data %>%
      dplyr::group_by(!!facet_sym) %>%
      dplyr::summarise(
        q25 = stats::quantile(!!days_sym, 0.25, na.rm = TRUE),
        q50 = stats::quantile(!!days_sym, 0.50, na.rm = TRUE),
        q75 = stats::quantile(!!days_sym, 0.75, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      tidyr::pivot_longer(
        cols      = dplyr::starts_with("q"),
        names_to  = "quartile",
        values_to = "xint"
      ) %>%
      dplyr::mutate(
        quartile_label = dplyr::case_when(
          quartile == "q25" ~ "25%",
          quartile == "q50" ~ "50% (median)",
          quartile == "q75" ~ "75%",
          TRUE              ~ quartile
        ),
        linetype = dplyr::case_when(
          quartile == "q50" ~ "solid",
          TRUE              ~ "dashed"
        )
      )
    
    base +
      ggplot2::geom_vline(
        data      = q_by_facet,
        ggplot2::aes(
          xintercept = xint,
          linetype   = quartile_label
        ),
        color = "blue",
        size  = 0.9
      ) +
      ggrepel::geom_text_repel(
        data = q_by_facet,
        ggplot2::aes(
          x     = xint,
          y     = Inf,
          label = paste0(quartile_label, ": ", round(xint, 1), " days")
        ),
        inherit.aes        = FALSE,
        color              = "blue",
        size               = 3.3,
        angle              = 90,
        vjust              = 1.2,
        min.segment.length = 0,
        segment.color      = "blue",
        box.padding        = 0.5,
        direction          = "x",
        max.overlaps       = Inf
      ) +
      ggplot2::facet_wrap(stats::as.formula(paste("~", facet_var)), scales = "free_y") +
      ggplot2::scale_linetype_manual(
        name   = NULL,
        values = c(
          "25%"          = "dashed",
          "50% (median)" = "solid",
          "75%"          = "dashed"
        )
      )
  }
}

# ------------------------------------------------------------------------
# 8. Procedure-type shares (value + count)
# ------------------------------------------------------------------------
# Aggregates contracts by procedure type and computes their share in total
# value and total number of contracts.

build_proc_share_data <- function(df) {
  df %>%
    dplyr::mutate(
      tender_proceduretype = recode_procedure_type(tender_proceduretype),
      tender_proceduretype = forcats::fct_explicit_na(
        as.factor(tender_proceduretype),
        na_level = "Missing value"
      )
    ) %>%
    dplyr::group_by(tender_proceduretype) %>%
    dplyr::summarise(
      total_value = sum(bid_priceusd, na.rm = TRUE),
      n_contracts = dplyr::n(),
      .groups     = "drop"
    ) %>%
    dplyr::mutate(
      share_value     = total_value / sum(total_value),
      share_contracts = n_contracts / sum(n_contracts)
    )
}

plot_proc_share_value <- function(plot_data) {
  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = stats::reorder(tender_proceduretype, share_value),
      y = share_value
    )
  ) +
    ggplot2::geom_col(
      ggplot2::aes(fill = tender_proceduretype),
      show.legend = FALSE,
      width       = 0.6
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = paste0(
          scales::percent(share_value, accuracy = 0.1),
          " (",
          scales::dollar(total_value, scale = 1e-6, suffix = "M"),
          ")"
        )
      ),
      hjust = -0.05,
      size  = 4
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::percent_format(accuracy = 1),
      expand = ggplot2::expansion(mult = c(0, 0.4))
    ) +
    ggplot2::scale_fill_brewer(palette = "Blues", direction = -1) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title   = "Share of contracts value",
      x       = NULL,
      y       = "Share of total value",
      caption = "Values in millions of USD"
    ) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      plot.margin = ggplot2::margin(10, 30, 10, 10),
      axis.text.y = ggplot2::element_text(size = 14)
    )
}

plot_proc_share_count <- function(plot_data) {
  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = stats::reorder(tender_proceduretype, share_value),  # keep same order
      y = share_contracts
    )
  ) +
    ggplot2::geom_col(
      ggplot2::aes(fill = tender_proceduretype),
      show.legend = FALSE,
      width       = 0.6
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = paste0(
          scales::percent(share_contracts, accuracy = 0.1),
          " (", n_contracts, " contracts)"
        )
      ),
      hjust = -0.05,
      size  = 4
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::percent_format(accuracy = 1),
      expand = ggplot2::expansion(mult = c(0, 0.4))
    ) +
    ggplot2::scale_fill_brewer(palette = "Blues", direction = -1) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Share of number of contracts",
      x     = NULL,
      y     = "Share of contracts"
    ) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      plot.margin = ggplot2::margin(10, 30, 10, 0),
      axis.text.y = ggplot2::element_text(size = 14)
    )
}

# ------------------------------------------------------------------------
# 9. Threshold configuration and accessor
# ------------------------------------------------------------------------
# Stores and retrieves country-specific thresholds for short/medium
# submission periods and long decisions.

admin_threshold_config <- tibble::tribble(
  ~country_code, ~subm_short_open, ~subm_short_restricted, ~subm_short_negotiated,
  ~subm_medium_open_min, ~subm_medium_open_max, ~long_decision_days,
  
  "DEFAULT", 30, 30, 30, 30, 30, 60,
  "UY",      21, 14, 14, NA, 28, 56, #Specify here country deadlines. Consider that in law there are business days, and we calculalte actual days, so business days should be transformed to actual days first.
  "BG",      30, 30, 30, 30, 30, NA
)


get_admin_thresholds <- function(country_code) {
  cc <- toupper(country_code)
  
  row <- admin_threshold_config %>%
    dplyr::filter(country_code %in% c(cc, "DEFAULT")) %>%
    dplyr::arrange(dplyr::desc(country_code == cc)) %>%
    dplyr::slice(1)
  
  row <- dplyr::select(row, -country_code)
  as.list(row)
}

# ------------------------------------------------------------------------
# 10. Year-filter configuration for regressions (by country and component)
# ------------------------------------------------------------------------
# Controls which tender years enter different parts of the pipeline.

year_filter_config <- tibble::tribble(
  ~component, ~country_code, ~min_year, ~max_year,
  
  # default catch-all (no explicit filtering)
  "default",  "BG",          NA,       NA,
  "default",  "UY",          NA,       NA,
  
  # component-specific overrides for single bidding
  "singleb",  "BG",          2011,     2018,
  "singleb",  "UY",          2014,     NA
)

# convenient helper: returns list(min_year, max_year)
get_year_range <- function(country_code,
                           component = c("singleb", "default")) {
  component <- match.arg(component)
  cc        <- toupper(country_code)
  
  # 1) component-specific rule
  row_spec <- year_filter_config %>%
    dplyr::filter(component == !!component, country_code == !!cc) %>%
    dplyr::slice_head(n = 1)
  
  # 2) fall back to default rule for that country
  if (nrow(row_spec) == 0) {
    row_spec <- year_filter_config %>%
      dplyr::filter(component == "default", country_code == !!cc) %>%
      dplyr::slice_head(n = 1)
  }
  
  # 3) if still nothing, no filtering
  if (nrow(row_spec) == 0) {
    return(list(min_year = -Inf, max_year = Inf))
  }
  
  min_y <- if (is.na(row_spec$min_year)) -Inf else row_spec$min_year
  max_y <- if (is.na(row_spec$max_year))  Inf else row_spec$max_year
  
  list(min_year = min_y, max_year = max_y)
}

# ========================================================================
# Unified administrative efficiency pipeline
# ========================================================================

run_admin_efficiency_pipeline <- function(df, country_code = "GEN", output_dir) {
  message("Running administrative efficiency pipeline for ", country_code, " ...")
  
  # country-specific thresholds
  thr <- get_admin_thresholds(country_code)
  
  # ensure tender_year is available
  df <- df %>% add_tender_year()
  
  # year range for single-bidding regressions (short/long)
  yr_singleb <- get_year_range(country_code, component = "singleb")
  min_year_singleb <- yr_singleb$min_year
  max_year_singleb <- yr_singleb$max_year
  
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
  # A) Share of procedure types by contract value and count
  # ----------------------------------------------------------------------
  proc_share_data <- build_proc_share_data(df)
  
  sh      <- plot_proc_share_value(proc_share_data)
  p_count <- plot_proc_share_count(proc_share_data)
  
  combined_proc <- sh + p_count + patchwork::plot_layout(ncol = 2)
  
  ggplot2::ggsave(
    filename = file.path(output_dir, "share_value_vs_contracts.png"),
    plot     = combined_proc,
    width    = 19,
    height   = 8,
    dpi      = 300
  )
  
  # ----------------------------------------------------------------------
  # B) Days between call for tender and bid deadline
  # ----------------------------------------------------------------------
  tender_periods_open <- compute_tender_days(
    df,
    tender_publications_firstcallfortenderdate,
    tender_biddeadline,
    tender_days_open
  )
  
  subm <- plot_days_hist_with_quartiles(
    data      = tender_periods_open,
    days_var  = "tender_days_open",
    facet_var = NULL,
    title     = "Days for bid submission",
    x_lab     = "Days between call opening and bid submission deadline",
    caption   = "Vertical lines indicate the 25th, 50th (median), and 75th percentiles (quartiles)"
  )
  
  ggplot2::ggsave(
    filename = file.path(output_dir, "subm.png"),
    plot     = subm,
    width    = 10,
    height   = 6,
    dpi      = 300
  )
  
  # ----------------------------------------------------------------------
  # C) Days between call and bid deadline by procedure type
  # ----------------------------------------------------------------------
  tender_periods_open_proc <- tender_periods_open %>%
    dplyr::mutate(
      tender_proceduretype = recode_procedure_type(tender_proceduretype)
    ) %>%
    tidyr::drop_na(tender_proceduretype)
  
  subm_proc_facet_q <- plot_days_hist_with_quartiles(
    data      = tender_periods_open_proc,
    days_var  = "tender_days_open",
    facet_var = "tender_proceduretype",
    title     = "Days for bid submission by procedure type",
    x_lab     = "Days between call opening and bid submission deadline",
    caption   = "Blue lines indicate the 25th, 50th (median), and 75th percentiles (quartiles) within each procedure type"
  )
  
  ggplot2::ggsave(
    filename = file.path(output_dir, "subm_proc_fac.png"),
    plot     = subm_proc_facet_q,
    width    = 10,
    height   = 6,
    dpi      = 300
  )
  
  # ----------------------------------------------------------------------
  # D) Too short submission periods (overall, by buyer, by value)
  # ----------------------------------------------------------------------
  tender_periods_short <- tender_periods_open_proc %>%
    dplyr::filter(
      tender_proceduretype %in% c(
        "Open Procedure",
        "Restricted Procedure",
        "Negotiated with publications"
      )
    ) %>%
    add_short_deadline_flags(
      days_col = tender_days_open,
      proc_col = tender_proceduretype,
      thr      = thr
    )
  
  
  # Distribution with coloured bars
  subm_r <- ggplot2::ggplot(
    tender_periods_short,
    ggplot2::aes(
      x = tender_days_open,
      fill = dplyr::case_when(
        short_deadline  ~ "red",
        medium_deadline ~ "yellow",
        TRUE            ~ "lightblue"
      )
    )
  ) +
    ggplot2::geom_histogram(
      binwidth = 1,
      boundary = 0,
      colour   = "white"
    ) +
    ggplot2::facet_wrap(~ tender_proceduretype, scales = "free_y") +
    ggplot2::scale_fill_identity() +
    ggplot2::xlim(0, 60) +
    ggplot2::labs(
      x        = "Days taken for the decision",
      y        = "Number of tenders",
      title    = "Distribution of tender open periods by procedure type",
      subtitle = "Bars highlighted: red = short deadline; yellow = medium deadline"
    ) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(legend.position = "none")
  
  share_labels_short <- tender_periods_short %>%
    dplyr::group_by(tender_proceduretype) %>%
    dplyr::summarise(
      share_short = mean(short_deadline, na.rm = TRUE) * 100,
      .groups     = "drop"
    )
  
  subm_r <- subm_r +
    ggplot2::geom_text(
      data = share_labels_short,
      ggplot2::aes(
        x = 50,
        y = Inf,
        label = paste0(
          "Share of contracts\nwith short deadlines: ",
          round(share_short, 1), "%"
        )
      ),
      vjust        = 2,
      hjust        = 1,
      size         = 4.5,
      fontface     = "bold",
      inherit.aes  = FALSE
    )
  
  ggplot2::ggsave(
    filename = file.path(output_dir, "subm_r.png"),
    plot     = subm_r,
    width    = 10,
    height   = 6,
    dpi      = 300
  )
  
  # --- Shares by buyers (number & value) --------------------------------
  tender_periods_buyer <- tender_periods_short %>%
    dplyr::mutate(
      buyer_group = add_buyer_group(buyer_buyertype)
    )
  
  # 1) by number of contracts
  short_share_buyer_proc <- tender_periods_buyer %>%
    dplyr::group_by(buyer_group, tender_proceduretype) %>%
    dplyr::summarise(
      share_short = mean(short_deadline, na.rm = TRUE),
      n_tenders   = dplyr::n(),
      .groups     = "drop"
    ) %>%
    dplyr::mutate(share_other = 1 - share_short) %>%
    tidyr::pivot_longer(
      cols      = c(share_short, share_other),
      names_to  = "deadline_type",
      values_to = "share"
    )
  
  buyer_short <- ggplot2::ggplot(
    short_share_buyer_proc,
    ggplot2::aes(
      x    = buyer_group,
      y    = share,
      fill = deadline_type
    )
  ) +
    ggplot2::geom_col(position = "fill") +
    ggplot2::geom_text(
      ggplot2::aes(label = scales::percent(share, accuracy = 1)),
      position = ggplot2::position_fill(vjust = 0.5),
      color    = "white",
      size     = 4,
      fontface = "bold"
    ) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    ggplot2::scale_fill_manual(
      values = c("share_short" = "tomato2", "share_other" = "steelblue2"),
      breaks = c("share_short", "share_other"),
      labels = c("Short submission period", "Other submission periods")
    ) +
    ggplot2::facet_wrap(~ tender_proceduretype) +
    ggplot2::labs(
      x     = "Buyer group",
      y     = "Share of tenders (100%)",
      fill  = NULL,
      title = "Short tender submission periods\n(calculated in number of contracts)"
    ) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      axis.text.x    = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = "top"
    )
  
  
  # 2) by value of contracts
  short_share_value_buyer_proc <- tender_periods_buyer %>%
    dplyr::group_by(buyer_group, tender_proceduretype) %>%
    dplyr::summarise(
      total_value = sum(bid_priceusd, na.rm = TRUE),
      short_value = sum(bid_priceusd[short_deadline %in% TRUE], na.rm = TRUE),
      share_short = dplyr::if_else(total_value > 0, short_value / total_value, NA_real_),
      n_contracts = dplyr::n(),
      .groups     = "drop"
    ) %>%
    dplyr::mutate(share_other = 1 - share_short) %>%
    tidyr::pivot_longer(
      cols      = c(share_short, share_other),
      names_to  = "deadline_type",
      values_to = "share"
    )
  
  buyer_short_v <- ggplot2::ggplot(
    short_share_value_buyer_proc,
    ggplot2::aes(
      x    = buyer_group,
      y    = share,
      fill = deadline_type
    )
  ) +
    ggplot2::geom_col(position = "fill") +
    ggplot2::geom_text(
      ggplot2::aes(label = scales::percent(share, accuracy = 1)),
      position = ggplot2::position_fill(vjust = 0.5),
      color    = "white",
      size     = 4,
      fontface = "bold"
    ) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_fill_manual(
      values = c("share_short" = "tomato2", "share_other" = "steelblue2"),
      breaks = c("share_short", "share_other"),
      labels = c("Short submission period", "Other submission periods")
    )+
    ggplot2::facet_wrap(~ tender_proceduretype) +
    ggplot2::labs(
      x     = "Buyer group",
      y     = "Share of contract value (100%)",
      fill  = NULL,
      title = "Short tender submission periods\n(calculated in value of contracts)"
    ) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      axis.text.x    = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = "top"
    )
  
  combined_short_buyer <- buyer_short + buyer_short_v +
    patchwork::plot_layout(nrow = 2)
  
  ggplot2::ggsave(
    filename = file.path(output_dir, "short_submission_buyer.png"),
    plot     = combined_short_buyer,
    width    = 12,
    height   = 12,
    dpi      = 300
  )
  
  # ----------------------------------------------------------------------
  # E) Days between submission deadline and award date
  # ----------------------------------------------------------------------
  tender_periods_dec <- compute_tender_days(
    df,
    tender_biddeadline,
    tender_awarddecisiondate,
    tender_days_dec
  )
  
  decp <- plot_days_hist_with_quartiles(
    data      = tender_periods_dec,
    days_var  = "tender_days_dec",
    facet_var = NULL,
    title     = "Days for award decision",
    x_lab     = "Days between bid submission deadline and contract award",
    caption   = "Vertical lines indicate the 25th, 50th (median), and 75th percentiles (quartiles)"
  )
  
  ggplot2::ggsave(
    filename = file.path(output_dir, "decp.png"),
    plot     = decp,
    width    = 10,
    height   = 6,
    dpi      = 300
  )
  
  # ----------------------------------------------------------------------
  # F) Days between submission and award date by procedure type
  # ----------------------------------------------------------------------
  tender_periods_dec_proc <- tender_periods_dec %>%
    dplyr::mutate(
      tender_proceduretype = recode_procedure_type(tender_proceduretype)
    ) %>%
    tidyr::drop_na(tender_proceduretype)
  
  decp_proc_facet_q <- plot_days_hist_with_quartiles(
    data      = tender_periods_dec_proc,
    days_var  = "tender_days_dec",
    facet_var = "tender_proceduretype",
    title     = "Days for award decision",
    x_lab     = "Days between bid submission deadline and contract award",
    caption   = "Blue lines indicate the 25th, 50th (median), and 75th percentiles (quartiles) within each procedure type"
  )
  
  ggplot2::ggsave(
    filename = file.path(output_dir, "decp_proc_fac.png"),
    plot     = decp_proc_facet_q,
    width    = 10,
    height   = 6,
    dpi      = 300
  )
  
  # ----------------------------------------------------------------------
  # G) “Too long” periods by procedure (long-decision threshold)
  # ----------------------------------------------------------------------
  # Mirrors the original logic: long periods are defined on the call-to-bid
  # interval, using the same 56-day style threshold.
  # ----- Effective long threshold for the "too long" descriptive block -----
  long_threshold_open <- if (is.na(thr$long_decision_days)) {
    tender_periods_open_proc %>%
      dplyr::filter(
        tender_proceduretype %in% c(
          "Open Procedure",
          "Restricted Procedure",
          "Negotiated with publications"
        )
      ) %>%
      dplyr::summarise(m = stats::median(tender_days_open, na.rm = TRUE)) %>%
      dplyr::pull(m)
  } else {
    thr$long_decision_days
  }
  
  
  thr_long_open <- thr
  thr_long_open$long_decision_days <- long_threshold_open
  
  
  tender_periods_long <- tender_periods_open_proc %>%
    dplyr::filter(
      tender_proceduretype %in% c(
        "Open Procedure",
        "Restricted Procedure",
        "Negotiated with publications"
      )
    ) %>%
    add_long_decision_flag(
      days_col = tender_days_open,
      proc_col = tender_proceduretype,
      thr      = thr_long_open
    )
  
  long_thr_label_ge <- paste0("≥ ", round(long_threshold_open), " days")
  long_thr_label_lt <- paste0("< ",  round(long_threshold_open), " days")
  
  
  decp_r <- ggplot2::ggplot(
    tender_periods_long,
    ggplot2::aes(
      x = tender_days_open,
      fill = dplyr::case_when(
        tender_proceduretype %in% c("Open Procedure", "Restricted Procedure") &
          tender_days_open >= long_threshold_open ~ "red",
        TRUE ~ "lightblue"
      )
    )
  ) +
    ggplot2::geom_histogram(
      binwidth = 4,
      boundary = 0,
      colour   = "white"
    ) +
    ggplot2::facet_wrap(~ tender_proceduretype, scales = "free_y") +
    ggplot2::scale_fill_identity() +
    ggplot2::xlim(0, 300) +
    ggplot2::labs(
      x        = "Days between bid submission deadline and contract award",
      y        = "Number of tenders",
      title    = "Distribution of tender decision periods by procedure type",
      subtitle = paste0(
        "Bars highlighted in red: periods ", long_thr_label_ge,
        " (country-specific long threshold)"
      )
    ) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(legend.position = "none")
  
  
  share_labels_long <- tender_periods_long %>%
    dplyr::group_by(tender_proceduretype) %>%
    dplyr::summarise(
      share_long = mean(tender_days_open >= long_threshold_open, na.rm = TRUE) * 100,
      .groups    = "drop"
    )
  
  decp_r <- decp_r +
    ggplot2::geom_text(
      data = share_labels_long,
      ggplot2::aes(
        x = 200,
        y = Inf,
        label = paste0(
          "Share of contracts\nwith delayed decision: ",
          round(share_long, 1), "%"
        )
      ),
      vjust       = 2,
      hjust       = 0.75,
      size        = 4.5,
      fontface    = "bold",
      inherit.aes = FALSE
    )
  
  
  ggplot2::ggsave(
    filename = file.path(output_dir, "decp_r.png"),
    plot     = decp_r,
    width    = 10,
    height   = 6,
    dpi      = 300
  )
  
  # ----------------------------------------------------------------------
  # H) Share of delayed decisions by buyer type (number & value)
  # ----------------------------------------------------------------------
  tender_periods_labeled_dec <- tender_periods_long %>%
    dplyr::mutate(
      buyer_group = add_buyer_group(buyer_buyertype)
    )
  
  # 1) by number of contracts
  long_share_buyer_proc <- tender_periods_labeled_dec %>%
    dplyr::group_by(buyer_group, tender_proceduretype) %>%
    dplyr::summarise(
      share_long = mean(long_decision, na.rm = TRUE),
      n_tenders  = dplyr::n(),
      .groups    = "drop"
    ) %>%
    dplyr::mutate(share_other = 1 - share_long) %>%
    tidyr::pivot_longer(
      cols      = c(share_long, share_other),
      names_to  = "decision_type",
      values_to = "share"
    )
  
  buyer_long <- ggplot2::ggplot(
    long_share_buyer_proc,
    ggplot2::aes(
      x    = buyer_group,
      y    = share,
      fill = decision_type
    )
  ) +
    ggplot2::geom_col(position = "fill") +
    ggplot2::geom_text(
      ggplot2::aes(label = scales::percent(share, accuracy = 1)),
      position = ggplot2::position_fill(vjust = 0.5),
      color    = "white",
      size     = 4,
      fontface = "bold"
    ) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    ggplot2::scale_fill_manual(
      values = c("share_long" = "tomato2", "share_other" = "steelblue2"),
      breaks = c("share_long", "share_other"),
      labels = c(long_thr_label_ge, long_thr_label_lt)
    ) +
    ggplot2::facet_wrap(~ tender_proceduretype) +
    ggplot2::labs(
      x     = "Buyer group",
      y     = "Share of tenders (100%)",
      fill  = NULL,
      title = paste0(
        "Long tender decision periods (", long_thr_label_ge, ")\n",
        "(calculated in number of contracts)"
      )
    ) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      axis.text.x    = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = "top"
    )
  

  
  # 2) by value of contracts
  long_share_value_buyer_proc <- tender_periods_labeled_dec %>%
    dplyr::group_by(buyer_group, tender_proceduretype) %>%
    dplyr::summarise(
      total_value = sum(bid_priceusd, na.rm = TRUE),
      long_value  = sum(bid_priceusd[long_decision %in% TRUE], na.rm = TRUE),
      share_long  = dplyr::if_else(total_value > 0, long_value / total_value, NA_real_),
      n_contracts = dplyr::n(),
      .groups     = "drop"
    ) %>%
    dplyr::mutate(share_other = 1 - share_long) %>%
    tidyr::pivot_longer(
      cols      = c(share_long, share_other),
      names_to  = "decision_type",
      values_to = "share"
    )
  
  buyer_long_v <- ggplot2::ggplot(
    long_share_value_buyer_proc,
    ggplot2::aes(
      x    = buyer_group,
      y    = share,
      fill = decision_type
    )
  ) +
    ggplot2::geom_col(position = "fill") +
    ggplot2::geom_text(
      ggplot2::aes(label = scales::percent(share, accuracy = 1)),
      position = ggplot2::position_fill(vjust = 0.5),
      color    = "white",
      size     = 4,
      fontface = "bold"
    ) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    ggplot2::scale_fill_manual(
      values = c("share_long" = "tomato2", "share_other" = "steelblue2"),
      breaks = c("share_long", "share_other"),
      labels = c(long_thr_label_ge, long_thr_label_lt)
    ) +
    ggplot2::facet_wrap(~ tender_proceduretype) +
    ggplot2::labs(
      x     = "Buyer group",
      y     = "Share of contract value (100%)",
      fill  = NULL,
      title = paste0(
        "Long tender decision periods (", long_thr_label_ge, ")\n",
        "(calculated in value of contracts)"
      )
    ) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      axis.text.x    = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = "top"
    )
  
  
  
  combined_dec_plot <- buyer_long + buyer_long_v +
    patchwork::plot_layout(nrow = 2)
  
  ggplot2::ggsave(
    filename = file.path(output_dir, "long_decision_buyer.png"),
    plot     = combined_dec_plot,
    width    = 12,
    height   = 12,
    dpi      = 300
  )
  
  # ----------------------------------------------------------------------
  # I) Effect of shortened period on single bidding (regression)
  # ----------------------------------------------------------------------
  reg_short_base <- df %>%
    dplyr::mutate(
      tender_publications_firstcallfortenderdate =
        as.Date(tender_publications_firstcallfortenderdate),
      tender_biddeadline = as.Date(tender_biddeadline),
      tender_days_open   = as.numeric(
        tender_biddeadline - tender_publications_firstcallfortenderdate
      )
    ) %>%
    add_tender_year() %>%
    dplyr::filter(
      tender_year >= min_year_singleb,
      tender_year <= max_year_singleb,
      !is.na(tender_days_open),
      tender_days_open >= 0,
      tender_days_open < 365,
      tender_proceduretype %in% c("OPEN", "RESTRICTED", "NEGOTIATED_WITH_PUBLICATION")
    )
  
  # Effective short thresholds for regression (median fallback)
  short_open_reg <- if (is.na(thr$subm_short_open)) {
    stats::median(
      reg_short_base$tender_days_open[reg_short_base$tender_proceduretype == "OPEN"],
      na.rm = TRUE
    )
  } else thr$subm_short_open
  
  short_rest_reg <- if (is.na(thr$subm_short_restricted)) {
    stats::median(
      reg_short_base$tender_days_open[reg_short_base$tender_proceduretype == "RESTRICTED"],
      na.rm = TRUE
    )
  } else thr$subm_short_restricted
  
  short_neg_reg <- if (is.null(thr$subm_short_negotiated) ||
                       is.na(thr$subm_short_negotiated)) {
    stats::median(
      reg_short_base$tender_days_open[reg_short_base$tender_proceduretype == "NEGOTIATED_WITH_PUBLICATION"],
      na.rm = TRUE
    )
  } else thr$subm_short_negotiated
  
  reg_short <- reg_short_base %>%
    dplyr::mutate(
      short_submission_period = dplyr::case_when(
        tender_proceduretype == "OPEN" &
          tender_days_open < short_open_reg ~ 1L,
        tender_proceduretype == "RESTRICTED" &
          tender_days_open < short_rest_reg ~ 1L,
        tender_proceduretype == "NEGOTIATED_WITH_PUBLICATION" &
          tender_days_open < short_neg_reg ~ 1L,
        tender_proceduretype %in% c("OPEN", "RESTRICTED", "NEGOTIATED_WITH_PUBLICATION") ~ 0L,
        TRUE ~ NA_integer_
      )
    ) %>%
    dplyr::filter(
      !is.na(short_submission_period),
      !is.na(ind_corr_singleb),
      !is.na(buyer_id)
    ) %>%
    dplyr::mutate(
      ind_corr_binary = ind_corr_singleb / 100
    )
  
  if (nrow(reg_short) > 0) {
    model_short_glm <- stats::glm(
      ind_corr_binary ~ short_submission_period +
        buyer_buyertype + tender_proceduretype +
        as.factor(tender_year),
      data   = reg_short,
      family = stats::binomial(link = "logit")
    )
    
    cov_cluster_short <- sandwich::vcovCL(
      model_short_glm,
      cluster = reg_short$buyer_id
    )
    lmtest::coeftest(model_short_glm, vcov = cov_cluster_short)
    
    pred_short <- ggeffects::ggpredict(
      model_short_glm,
      terms    = "short_submission_period",
      vcov_fun = "vcovCL",
      vcov_args = list(cluster = reg_short$buyer_id)
    )
    
    # caption: N and years covered
    n_short <- nrow(reg_short)
    min_y_s <- min(reg_short$tender_year, na.rm = TRUE)
    max_y_s <- max(reg_short$tender_year, na.rm = TRUE)
    caption_short <- paste0(
      "Sample: N = ", n_short,
      " tenders; years covered: ", min_y_s, "–", max_y_s
    )
    
    plot_short_reg <- ggplot2::ggplot(
      pred_short,
      ggplot2::aes(x = x, y = predicted)
    ) +
      ggplot2::geom_line(size = 1.5, color = "lightblue") +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = conf.low, ymax = conf.high),
        alpha = 0.2
      ) +
      ggplot2::labs(
        title    = "Predicted probability of single bidding\nby short submission period",
        subtitle = "Controls: buyer type, tender year; cluster-robust SEs by buyer",
        x        = "Short submission period (0 = normal, 1 = short)",
        y        = "Predicted probability",
        caption  = caption_short
      ) +
      ggplot2::scale_y_continuous(labels = scales::percent_format()) +
      ggplot2::theme_minimal(base_size = 20)
  } else {
    model_short_glm <- NULL
    plot_short_reg  <- NULL
  }
  
  
  # ----------------------------------------------------------------------
  # J) Effect of long period on single bidding (regression)
  # ----------------------------------------------------------------------
  reg_long_base <- df %>%
    dplyr::mutate(
      tender_publications_firstcallfortenderdate =
        as.Date(tender_publications_firstcallfortenderdate),
      tender_biddeadline = as.Date(tender_biddeadline),
      tender_days_dec    = as.numeric(
        tender_biddeadline - tender_publications_firstcallfortenderdate
      )
    ) %>%
    add_tender_year() %>%
    dplyr::filter(
      tender_year >= min_year_singleb,
      tender_year <= max_year_singleb,
      tender_days_dec >= 0,
      tender_days_dec < 365,
      tender_proceduretype %in% c("OPEN", "RESTRICTED", "NEGOTIATED_WITH_PUBLICATION")
    )
  
  # Effective long threshold for regression (median fallback)
  long_threshold_dec <- if (is.na(thr$long_decision_days)) {
    stats::median(reg_long_base$tender_days_dec, na.rm = TRUE)
  } else thr$long_decision_days
  
  reg_long <- reg_long_base %>%
    dplyr::mutate(
      long_decision_period = dplyr::case_when(
        !is.na(tender_days_dec) &
          tender_days_dec >= 0 & tender_days_dec < 365 &
          tender_days_dec >= long_threshold_dec ~ 1L,
        !is.na(tender_days_dec) &
          tender_days_dec >= 0 & tender_days_dec < 365 &
          tender_days_dec < long_threshold_dec ~ 0L,
        TRUE ~ NA_integer_
      )
    ) %>%
    dplyr::filter(
      !is.na(long_decision_period),
      !is.na(ind_corr_singleb),
      !is.na(buyer_id)
    ) %>%
    dplyr::mutate(
      ind_corr_binary = ind_corr_singleb / 100
    )
  
  if (nrow(reg_long) > 0) {
    model_long_glm <- stats::glm(
      ind_corr_binary ~ long_decision_period +
        buyer_buyertype + tender_proceduretype +
        as.factor(tender_year),
      data   = reg_long,
      family = stats::binomial(link = "logit")
    )
    
    cov_cluster_long <- sandwich::vcovCL(
      model_long_glm,
      cluster = reg_long$buyer_id
    )
    lmtest::coeftest(model_long_glm, vcov = cov_cluster_long)
    
    pred_long <- ggeffects::ggpredict(
      model_long_glm,
      terms    = "long_decision_period",
      vcov_fun = "vcovCL",
      vcov_args = list(cluster = reg_long$buyer_id)
    )
    
    # caption: N and years covered
    n_long  <- nrow(reg_long)
    min_y_l <- min(reg_long$tender_year, na.rm = TRUE)
    max_y_l <- max(reg_long$tender_year, na.rm = TRUE)
    caption_long <- paste0(
      "Sample: N = ", n_long,
      " tenders; years covered: ", min_y_l, "–", max_y_l
    )
    
    x_label_long <- "Long decision period (0 = normal, 1 = too long)"
    
    plot_long_reg <- ggplot2::ggplot(
      pred_long,
      ggplot2::aes(x = x, y = predicted)
    ) +
      ggplot2::geom_line(size = 1.5, color = "lightblue") +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = conf.low, ymax = conf.high),
        alpha = 0.2
      ) +
      ggplot2::labs(
        title    = "Predicted probability of single bidding\nby length of decision period",
        subtitle = "Controls: buyer type, tender year; cluster-robust SEs by buyer",
        x        = x_label_long,
        y        = "Predicted probability",
        caption  = caption_long
      ) +
      ggplot2::theme_minimal(base_size = 20) +
      ggplot2::scale_y_continuous(labels = scales::percent_format())
  } else {
    model_long_glm <- NULL
    plot_long_reg  <- NULL
  }
  
  # ----------------------------------------------------------------------
  # K) Collect outputs and return
  # ----------------------------------------------------------------------
  results <- list(
    country_code               = country_code,
    thresholds                 = thr,
    proc_share_data            = proc_share_data,
    tender_periods_open        = tender_periods_open,
    tender_periods_open_proc   = tender_periods_open_proc,
    tender_periods_short       = tender_periods_short,
    tender_periods_dec         = tender_periods_dec,
    tender_periods_dec_proc    = tender_periods_dec_proc,
    tender_periods_long        = tender_periods_long,
    tender_periods_labeled_dec = tender_periods_labeled_dec,
    
    # plots
    sh                   = sh,
    p_count              = p_count,
    combined_proc        = combined_proc,
    subm                 = subm,
    subm_proc_facet_q    = subm_proc_facet_q,
    subm_r               = subm_r,
    buyer_short          = buyer_short,
    buyer_short_v        = buyer_short_v,
    combined_short_buyer = combined_short_buyer,
    decp                 = decp,
    decp_proc_facet_q    = decp_proc_facet_q,
    decp_r               = decp_r,
    buyer_long           = buyer_long,
    buyer_long_v         = buyer_long_v,
    combined_dec_plot    = combined_dec_plot,
    plot_short_reg       = plot_short_reg,
    plot_long_reg        = plot_long_reg,
    
    # models
    model_short_glm      = model_short_glm,
    model_long_glm       = model_long_glm,
    
    summary_stats         = summary_stats
  )
  
  invisible(results)
}

