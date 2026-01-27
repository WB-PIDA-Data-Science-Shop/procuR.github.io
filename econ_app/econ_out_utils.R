# ========================================================================
# PROACT Economic Efficiency Utilities
# ========================================================================
# Purpose:
#   A reusable, modular toolkit for procurement exports (e.g., PROACT).
#   Focus areas ("economic efficiency"):
#     - CPV market sizing (counts, total value, typical contract size)
#     - Supplier entry dynamics (new vs repeat suppliers)
#     - Buyer–supplier network snapshots by year (top buyers)
#     - Relative prices (contract vs estimated) diagnostics (BG-style)
#     - Competition diagnostics (single-bid incidence by procedure/price/buyer)
#
# Design principles:
#   - Small pure helpers + a single orchestrator pipeline that returns a list
#     of objects for printing in an Rmd (similar to your admin pipeline).
#   - No hard-coded file paths; all I/O is parameterized.
#   - CPV lookup is built once and reused.
#   - Guardrails for missing columns and pathological values.
# ========================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(purrr)
  library(forcats)
  library(ggplot2)
  library(scales)
  library(ggrepel)
  library(tidygraph)
  library(ggraph)
  library(patchwork)
})

# ------------------------------------------------------------------------
# 0) I/O helpers
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

load_proact_csv <- function(input_path) load_data(input_path)

dir_ensure <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}

save_plot <- function(plot, out_dir, filename, width = 10, height = 7, dpi = 300) {
  dir_ensure(out_dir)
  ggplot2::ggsave(
    filename = file.path(out_dir, filename),
    plot     = plot,
    width    = width,
    height   = height,
    dpi      = dpi
  )
  invisible(file.path(out_dir, filename))
}

# ------------------------------------------------------------------------
# 1) Relabeling / recoding helpers (keep near the top)
# ------------------------------------------------------------------------

recode_procedure_type <- function(x) {
  dplyr::recode(
    as.character(x),
    "OPEN"                           = "Open Procedure",
    "OUTRIGHT_AWARD"                 = "Direct Award",
    "RESTRICTED"                     = "Restricted Procedure",
    "COMPETITIVE_DIALOG"             = "Competitive Dialog",
    "NEGOTIATED"                     = "Negotiated",
    "NEGOTIATED_WITHOUT_PUBLICATION" = "Negotiated without publications",
    "NEGOTIATED_WITH_PUBLICATION"    = "Negotiated with publications",
    "OTHER"                          = "Other Procedures",
    .default                         = "Other Procedures"
  )
}

add_buyer_group <- function(buyer_buyertype) {
  group <- dplyr::case_when(
    grepl("(?i)national",  buyer_buyertype) ~ "National Buyer",
    grepl("(?i)regional",  buyer_buyertype) ~ "Regional Buyer",
    grepl("(?i)utilities", buyer_buyertype) ~ "Utilities",
    grepl("(?i)european",  buyer_buyertype) ~ "EU agency",
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

make_cpv_cluster_legend <- function(market_summary) {
  market_summary %>%
    dplyr::select(cpv_cluster, cpv_category) %>%
    dplyr::distinct() %>%
    dplyr::arrange(cpv_cluster)
}


# ------------------------------------------------------------------------
# 2) Standard feature engineering
# ------------------------------------------------------------------------

add_tender_year <- function(df,
                            date_cols = c(
                              "tender_publications_firstcallfortenderdate",
                              "tender_awarddecisiondate",
                              "tender_biddeadline"
                            )) {
  get_year <- function(x) stringr::str_extract(x, "^\\d{4}")
  
  cols_present <- intersect(date_cols, names(df))
  if (length(cols_present) == 0) {
    df$tender_year <- NA_integer_
    return(df)
  }
  
  year_vec <- purrr::reduce(
    cols_present,
    .init = rep(NA_character_, nrow(df)),
    .f = function(acc, col) dplyr::coalesce(acc, get_year(df[[col]]))
  )
  
  df %>% dplyr::mutate(tender_year = as.integer(year_vec))
}

add_cpv_cluster <- function(df, cpv_col = "lot_productcode", digits = 2) {
  if (!cpv_col %in% names(df)) {
    df$cpv_cluster <- NA_character_
    return(df)
  }
  df %>% dplyr::mutate(cpv_cluster = stringr::str_sub(.data[[cpv_col]], 1, digits))
}

add_single_bid_flag <- function(df, singleb_col = "ind_corr_singleb") {
  if (!singleb_col %in% names(df)) {
    df$single_bid <- NA_real_
    return(df)
  }
  df %>% dplyr::mutate(single_bid = .data[[singleb_col]] / 100)
}

add_price_bins_usd <- function(df, value_col = "bid_priceusd") {
  if (!value_col %in% names(df)) {
    df$price_bin <- NA
    return(df)
  }
  df %>%
    dplyr::mutate(
      price_bin = cut(
        .data[[value_col]],
        breaks = c(0, 5e3, 1e4, 5e4, 1e5, 5e5, 1e6, Inf),
        labels = c("< 5k", "5–10k", "10k–50k", "50k–100k", "100k–500k", "500k–1M", "> 1M"),
        right  = FALSE
      )
    )
}

wrap_strip <- function(x, width = 18) {
  stringr::str_wrap(x, width = width)
}

# ------------------------------------------------------------------------
# 3) CPV lookup (build once, reuse)
# ------------------------------------------------------------------------

build_cpv_lookup <- function(cpv_table, code_col = "CODE", label_col = "EN") {
  stopifnot(code_col %in% names(cpv_table), label_col %in% names(cpv_table))
  
  cpv_core <- sub("-.*", "", cpv_table[[code_col]])
  
  df <- cpv_table %>%
    dplyr::mutate(
      cpv_core = cpv_core,
      cpv_2d = dplyr::if_else(
        stringr::str_sub(cpv_core, 3, 8) == "000000",
        stringr::str_sub(cpv_core, 1, 2),
        NA_character_
      ),
      cpv_3d = dplyr::if_else(
        stringr::str_sub(cpv_core, 3, 3) != "0" & stringr::str_sub(cpv_core, 4, 8) == "00000",
        stringr::str_sub(cpv_core, 1, 3),
        NA_character_
      )
    )
  
  list(
    cpv_2d = df %>%
      dplyr::filter(!is.na(cpv_2d)) %>%
      dplyr::distinct(cpv_2d, .keep_all = TRUE) %>%
      dplyr::transmute(cpv_cluster = cpv_2d, cpv_category = .data[[label_col]]),
    cpv_3d = df %>%
      dplyr::filter(!is.na(cpv_3d)) %>%
      dplyr::distinct(cpv_3d, .keep_all = TRUE) %>%
      dplyr::transmute(cpv_cluster = cpv_3d, cpv_category = .data[[label_col]])
  )
}

attach_cpv_labels <- function(df,
                              cpv_lookup_2d,
                              cluster_col = "cpv_cluster",
                              other_code = "99",
                              other_label = "Other") {
  if (!cluster_col %in% names(df)) return(df)
  
  out <- df %>%
    dplyr::left_join(cpv_lookup_2d, by = setNames("cpv_cluster", cluster_col)) %>%
    dplyr::mutate(
      !!cluster_col := dplyr::if_else(is.na(cpv_category) | cpv_category == "", other_code, .data[[cluster_col]]),
      cpv_category  := dplyr::if_else(.data[[cluster_col]] == other_code, other_label, cpv_category)
    )
  
  out
}


# ------------------------------------------------------------------------
# 3.1) Year set up
# ------------------------------------------------------------------------

year_breaks_rule <- function(years, max_labels = 5) {
  yrs <- sort(unique(stats::na.omit(as.integer(years))))
  if (length(yrs) <= max_labels) return(yrs)
  # if too many years -> every 2nd year (you asked exactly this)
  yrs[seq(1, length(yrs), by = 2)]
}

NETWORK_YEAR_LIMITS <- list(
  GEN = c(NA, NA),
  UY  = c(2014, NA),
  BG  = c(2011, 2018)
)

get_network_year_limits <- function(country_code, default = c(-Inf, Inf)) {
  lim <- NETWORK_YEAR_LIMITS[[country_code]]
  if (is.null(lim) || length(lim) != 2) lim <- default
  
  # Treat NA as open-ended
  lim[1] <- ifelse(is.na(lim[1]), default[1], lim[1])
  lim[2] <- ifelse(is.na(lim[2]), default[2], lim[2])
  lim
}



# ------------------------------------------------------------------------
# 4) Market size summaries + plots
# ------------------------------------------------------------------------
summarise_market_size <- function(df,
                                  cluster_col = "cpv_cluster",
                                  category_col = "cpv_category",
                                  value_col = "bid_priceusd") {
  stopifnot(cluster_col %in% names(df))
  if (!value_col %in% names(df)) df[[value_col]] <- NA_real_
  if (!category_col %in% names(df)) df[[category_col]] <- NA_character_
  
  df %>%
    dplyr::group_by(.data[[cluster_col]], .data[[category_col]]) %>%
    dplyr::summarise(
      total_value = sum(.data[[value_col]], na.rm = TRUE),
      avg_value   = mean(.data[[value_col]], na.rm = TRUE),
      n_contracts = dplyr::n(),
      .groups     = "drop"
    ) %>%
    dplyr::rename(cpv_cluster = 1, cpv_category = 2)
}

plot_market_contract_counts <- function(market_summary) {
  ggplot2::ggplot(market_summary, ggplot2::aes(x = stats::reorder(cpv_category, n_contracts), y = n_contracts)) +
    ggplot2::geom_col(fill = "steelblue") +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::labs(x = "CPV category", y = "Count of contracts") +
    ggplot2::theme_minimal()
}

plot_market_total_value <- function(market_summary) {
  ggplot2::ggplot(market_summary, ggplot2::aes(x = stats::reorder(cpv_category, total_value), y = total_value)) +
    ggplot2::geom_col(fill = "steelblue") +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::labs(x = "CPV category", y = "Total contract value (USD)") +
    ggplot2::theme_minimal()
}

plot_market_bubble <- function(market_summary) {
  ggplot2::ggplot(
    market_summary,
    ggplot2::aes(
      x = n_contracts,
      y = avg_value,
      size = total_value,
      label = cpv_cluster
    )
  ) +
    ggplot2::geom_point(alpha = 0.7, colour = "steelblue") +
    ggrepel::geom_text_repel(size = 3, max.overlaps = 30, colour = "gray20") +
    ggplot2::scale_x_log10(labels = scales::comma) +
    ggplot2::scale_y_log10(labels = scales::label_dollar(prefix = "$")) +
    ggplot2::scale_size_continuous(
      labels = scales::label_number(scale_cut = scales::cut_short_scale()),
      name = "Total market value (USD)"
    ) +
    ggplot2::labs(
      title = "CPV markets: volume vs typical contract size",
      subtitle = "Labels show CPV clusters; bubble size = total value",
      x = "Number of contracts (log10)",
      y = "Average contract value (USD, log10)"
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::guides(size = ggplot2::guide_legend(order = 1))
}

# ------------------------------------------------------------------------
# 5) Supplier entry: new vs repeat suppliers
# ------------------------------------------------------------------------

compute_supplier_entry <- function(df,
                                   supplier_id_col = "bidder_masterid",
                                   cluster_col = "cpv_cluster",
                                   year_col = "tender_year") {
  cols <- c(supplier_id_col, cluster_col, year_col)
  missing <- setdiff(cols, names(df))
  if (length(missing) > 0) stop("Missing required columns: ", paste(missing, collapse = ", "))
  
  df2 <- df %>%
    dplyr::filter(!is.na(.data[[supplier_id_col]]),
                  !is.na(.data[[year_col]]),
                  !is.na(.data[[cluster_col]]))
  
  first_year_tbl <- df2 %>%
    dplyr::group_by(.data[[cluster_col]], .data[[supplier_id_col]]) %>%
    dplyr::summarise(first_year = min(.data[[year_col]], na.rm = TRUE), .groups = "drop")
  
  df2 <- df2 %>%
    dplyr::left_join(first_year_tbl, by = c(cluster_col, supplier_id_col)) %>%
    dplyr::mutate(is_new = .data[[year_col]] == first_year)
  
  df2 %>%
    dplyr::group_by(.data[[cluster_col]], .data[[year_col]]) %>%
    dplyr::summarise(
      n_suppliers        = dplyr::n_distinct(.data[[supplier_id_col]]),
      n_new_suppliers    = dplyr::n_distinct(.data[[supplier_id_col]][is_new]),
      n_repeat_suppliers = n_suppliers - n_new_suppliers,
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      share_new    = n_new_suppliers / n_suppliers,
      share_repeat = n_repeat_suppliers / n_suppliers
    ) %>%
    dplyr::rename(cpv_cluster = 1, tender_year = 2)
}

plot_supplier_shares_facet <- function(supplier_stats) {
  
  years_all <- sort(unique(stats::na.omit(supplier_stats$tender_year)))
  year_breaks <- year_breaks_rule(years_all, max_labels = 5)
  
  plot_data <- supplier_stats %>%
    dplyr::select(cpv_cluster, tender_year, share_new, share_repeat) %>%
    tidyr::pivot_longer(cols = c(share_new, share_repeat),
                        names_to = "type", values_to = "share") %>%
    dplyr::mutate(
      type = dplyr::recode(type, share_new = "New suppliers", share_repeat = "Repeat suppliers"),
      tender_year = factor(tender_year, levels = years_all)
    )
  
  ggplot2::ggplot(plot_data, ggplot2::aes(x = tender_year, y = share, fill = type)) +
    ggplot2::geom_col(position = "stack") +
    ggplot2::facet_wrap(~ cpv_cluster, scales = "free_y") +
    ggplot2::scale_x_discrete(breaks = as.character(year_breaks)) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::labs(
      title = "Share of new vs repeat suppliers by CPV market",
      x = "Year", y = "Share of suppliers", fill = ""
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "bottom",
      strip.text = ggplot2::element_text(face = "bold", size = 10),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
}


plot_unique_suppliers_over_time <- function(df,
                                            supplier_id_col = "bidder_masterid",
                                            cluster_col = "cpv_cluster",
                                            year_col = "tender_year") {
  
  plot_data <- df %>%
    dplyr::filter(!is.na(.data[[supplier_id_col]]),
                  !is.na(.data[[year_col]]),
                  !is.na(.data[[cluster_col]])) %>%
    dplyr::group_by(.data[[cluster_col]], .data[[year_col]]) %>%
    dplyr::summarise(n_suppliers = dplyr::n_distinct(.data[[supplier_id_col]]), .groups = "drop") %>%
    dplyr::rename(cpv_cluster = 1, tender_year = 2)
  
  year_breaks <- year_breaks_rule(plot_data$tender_year, max_labels = 5)
  
  ggplot2::ggplot(plot_data, ggplot2::aes(x = tender_year, y = n_suppliers)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(~ cpv_cluster, scales = "free_y") +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_x_continuous(breaks = year_breaks) +
    ggplot2::labs(
      title = "Number of unique suppliers per market and year",
      x = "Year", y = "Unique suppliers"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      strip.text = ggplot2::element_text(face = "bold", size = 10),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
}

# ------------------------------------------------------------------------
# 6) Buyer–supplier networks (yearly snapshots)
# ------------------------------------------------------------------------

plot_buyer_supplier_networks <- function(df,
                                         cpv_focus,
                                         n_top_buyers = 20,
                                         ncol = 1,
                                         buyer_id_col = "buyer_id",
                                         supplier_id_col = "bidder_masterid",
                                         year_col = "tender_year",
                                         value_col = "bid_priceusd",
                                         country_code = "GEN",
                                         year_limits = NULL) {
  
  required <- c("cpv_cluster", buyer_id_col, supplier_id_col, year_col)
  missing <- setdiff(required, names(df))
  if (length(missing) > 0) stop("Missing required columns: ", paste(missing, collapse = ", "))
  
  if (is.null(year_limits)) year_limits <- get_network_year_limits(country_code)
  
  sub <- df %>%
    dplyr::filter(
      cpv_cluster == cpv_focus,
      !is.na(.data[[buyer_id_col]]),
      !is.na(.data[[supplier_id_col]]),
      !is.na(.data[[year_col]]),
      .data[[year_col]] >= year_limits[1],
      .data[[year_col]] <= year_limits[2]
    )
  
  if (nrow(sub) == 0) stop("No data for cpv_cluster = ", cpv_focus, " in selected year range.")
  
  years <- sort(unique(sub[[year_col]]))
  
  filter_top_buyers <- function(dat, yr) {
    dy <- dat %>% dplyr::filter(.data[[year_col]] == yr)
    if (nrow(dy) == 0) return(dy[0, , drop = FALSE])
    
    if (value_col %in% names(dy)) {
      top <- dy %>%
        dplyr::group_by(.data[[buyer_id_col]]) %>%
        dplyr::summarise(total_value = sum(.data[[value_col]], na.rm = TRUE), .groups = "drop") %>%
        dplyr::slice_max(total_value, n = n_top_buyers, with_ties = FALSE)
      dy %>% dplyr::filter(.data[[buyer_id_col]] %in% top[[buyer_id_col]])
    } else {
      top <- dy %>%
        dplyr::count(.data[[buyer_id_col]], name = "n") %>%
        dplyr::slice_max(n, n = n_top_buyers, with_ties = FALSE)
      dy %>% dplyr::filter(.data[[buyer_id_col]] %in% top[[buyer_id_col]])
    }
  }
  
  make_year_plot <- function(yr) {
    dat <- filter_top_buyers(sub, yr)
    if (nrow(dat) == 0) return(NULL)
    
    edges <- dat %>%
      dplyr::count(.data[[buyer_id_col]], .data[[supplier_id_col]], name = "weight") %>%
      dplyr::transmute(
        from   = paste0("B_", .data[[buyer_id_col]]),
        to     = paste0("S_", .data[[supplier_id_col]]),
        weight = weight
      )
    
    buyers <- dat %>%
      dplyr::distinct(.data[[buyer_id_col]]) %>%
      dplyr::transmute(name = paste0("B_", .data[[buyer_id_col]]), role = "Buyer")
    
    suppliers <- dat %>%
      dplyr::distinct(.data[[supplier_id_col]]) %>%
      dplyr::transmute(name = paste0("S_", .data[[supplier_id_col]]), role = "Supplier")
    
    nodes <- dplyr::bind_rows(buyers, suppliers) %>% dplyr::distinct(name, .keep_all = TRUE)
    
    g <- tidygraph::tbl_graph(nodes = nodes, edges = edges, directed = FALSE) %>%
      tidygraph::activate(nodes) %>%
      dplyr::mutate(degree = tidygraph::centrality_degree())
    
    ggraph::ggraph(g, layout = "stress") +
      ggraph::geom_edge_link(ggplot2::aes(alpha = weight), show.legend = FALSE) +
      ggraph::geom_node_point(ggplot2::aes(color = role, size = degree), show.legend = FALSE) +
      ggraph::geom_node_text(
        ggplot2::aes(label = ifelse(role == "Buyer", name, "")),
        repel = TRUE,
        size = 2.5,
        show.legend = FALSE
      ) +
      ggplot2::scale_color_manual(values = c("Buyer" = "steelblue", "Supplier" = "darkorange")) +
      ggplot2::scale_size_continuous(range = c(2, 6), limits = c(0, NA)) +
      ggplot2::theme_void() +
      ggplot2::ggtitle(paste("Year:", yr)) +
      ggplot2::theme(legend.position = "none")
  }
  
  plots <- purrr::map(years, make_year_plot) %>% purrr::compact()
  if (length(plots) == 0) stop("No non-empty yearly networks for cpv_cluster = ", cpv_focus)
  
  patchwork::wrap_plots(plots, ncol = ncol) +
    patchwork::plot_annotation(
      title = paste("Buyer–Supplier Networks by Year, CPV", cpv_focus),
      subtitle = paste0("Top ", n_top_buyers, " buyers per year (buyers labeled as B_*)"),
      caption  = "Caption: Node color = role (Buyer vs Supplier). Node size = number of unique partners (degree). Edge thickness/opacity reflects number of contracts between buyer–supplier pairs."
    ) &
    ggplot2::theme(legend.position = "none")
}
  
# ------------------------------------------------------------------------
# 7) Relative price diagnostics (BG-style)
# ------------------------------------------------------------------------

add_relative_price <- function(df,
                               contract_price_col = "bid_price",
                               estimated_price_col = "lot_estimatedprice",
                               cap = 5) {
  required <- c(contract_price_col, estimated_price_col)
  missing <- setdiff(required, names(df))
  if (length(missing) > 0) stop("Missing required columns: ", paste(missing, collapse = ", "))
  
  df %>%
    dplyr::mutate(
      relative_price = .data[[contract_price_col]] / .data[[estimated_price_col]],
      relative_price = dplyr::if_else(relative_price > cap | relative_price <= 0, NA_real_, relative_price)
    )
}

plot_relative_price_density <- function(df) {
  ggplot2::ggplot(df, ggplot2::aes(x = relative_price)) +
    ggplot2::geom_density(fill = "steelblue", alpha = 0.4, na.rm = TRUE) +
    ggplot2::labs(
      title = "Density of relative bid prices",
      x = "Relative price (contract / estimated)",
      y = "Density"
    ) +
    ggplot2::theme_minimal()
}

plot_relative_price_by_year <- function(df) {
  ggplot2::ggplot(df, ggplot2::aes(x = as.factor(tender_year), y = relative_price)) +
    ggplot2::geom_boxplot(outlier.alpha = 0.2, na.rm = TRUE) +
    ggplot2::labs(
      title = "Relative prices over time",
      x = "Year",
      y = "Relative price (contract / estimated)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}

top_markets_by_relative_price <- function(df, n = 10) {
  df %>%
    dplyr::group_by(cpv_category) %>%
    dplyr::summarise(mean_rel_price = mean(relative_price, na.rm = TRUE), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(mean_rel_price)) %>%
    dplyr::slice_head(n = n) %>%
    dplyr::pull(cpv_category)
}

plot_top_markets_relative_price <- function(df, top_markets) {
  dat <- df %>% dplyr::filter(cpv_category %in% top_markets)
  ggplot2::ggplot(dat, ggplot2::aes(x = stats::reorder(cpv_category, relative_price, median), y = relative_price)) +
    ggplot2::geom_boxplot(outlier.alpha = 0.2, fill = "steelblue", alpha = 0.6, na.rm = TRUE) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Top markets with highest relative prices",
      x = "Market",
      y = "Relative price (contract / estimated)"
    ) +
    ggplot2::theme_minimal(base_size = 12)
}

top_buyers_by_relative_price <- function(df,
                                         buyer_name_col = "buyer_name",
                                         min_contracts = 10,
                                         n = 20) {
  df %>%
    dplyr::group_by(.data[[buyer_name_col]]) %>%
    dplyr::summarise(
      mean_relative_price = mean(relative_price, na.rm = TRUE),
      n_contracts = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::filter(n_contracts > min_contracts) %>%
    dplyr::arrange(dplyr::desc(mean_relative_price)) %>%
    dplyr::slice_head(n = n) %>%
    dplyr::rename(buyer_name = 1)
}

plot_top_buyers_relative_price <- function(top_buyers_df, label_max_chars = 20) {
  
  dfp <- top_buyers_df %>%
    dplyr::mutate(
      buyer_name_short = stringr::str_trunc(buyer_name, width = label_max_chars, side = "right", ellipsis = "…"),
      buyer_name_short = make.unique(buyer_name_short)  # <-- prevents merged bars
    )
  
  ggplot2::ggplot(dfp, ggplot2::aes(
    x = stats::reorder(buyer_name_short, mean_relative_price),
    y = mean_relative_price
  )) +
    ggplot2::geom_col(fill = "steelblue") +
    ggplot2::geom_text(ggplot2::aes(label = n_contracts), hjust = -0.15, size = 3.5) +
    ggplot2::coord_flip(clip = "off") +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      axis.text.y = ggplot2::element_text(size = 10),
      plot.margin = ggplot2::margin(8, 30, 8, 8)
    ) +
    ggplot2::labs(
      title = "Top buyers by average relative price",
      subtitle = "Numbers on bars indicate number of contracts (filtered to sufficiently frequent buyers)",
      x = "Buyer",
      y = "Average relative price"
    ) +
    ggplot2::expand_limits(y = max(dfp$mean_relative_price, na.rm = TRUE) * 1.15)
}


# ------------------------------------------------------------------------
# 8) Competition diagnostics (single-bid shares)
# ------------------------------------------------------------------------

plot_single_bid_by_procedure <- function(df, proc_col = "tender_proceduretype") {
  if (!proc_col %in% names(df)) stop("Missing required column: ", proc_col)
  
  dat <- df %>%
    dplyr::mutate(procedure_type = recode_procedure_type(.data[[proc_col]])) %>%
    dplyr::filter(!is.na(single_bid), !is.na(procedure_type)) %>%
    dplyr::group_by(procedure_type) %>%
    dplyr::summarise(
      share_single_bid = mean(single_bid, na.rm = TRUE),
      share_multi_bid  = 1 - share_single_bid,
      n = dplyr::n(),
      .groups = "drop"
    ) %>%
    tidyr::pivot_longer(
      cols = c(share_single_bid, share_multi_bid),
      names_to = "bid_type",
      values_to = "share"
    ) %>%
    dplyr::mutate(bid_type = factor(bid_type, levels = c("share_single_bid", "share_multi_bid")))
  
  ggplot2::ggplot(dat, ggplot2::aes(x = procedure_type, y = share, fill = bid_type)) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::geom_text(
      ggplot2::aes(label = scales::percent(share, accuracy = 1)),
      position = ggplot2::position_stack(vjust = 0.5),
      color = "white",
      size = 3.5
    ) +
    ggplot2::scale_y_continuous(labels = scales::percent_format()) +
    ggplot2::scale_fill_manual(
      values = c("share_single_bid" = "#d73027", "share_multi_bid" = "#4575b4"),
      labels = c("Single bid", "Multiple bids")
    ) +
    ggplot2::labs(
      x = "Procedure type",
      y = "Share of tenders",
      fill = "",
      title = "Share of single-bid tenders by procedure type"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}

plot_single_bid_by_price <- function(df) {
  dat <- df %>%
    dplyr::filter(!is.na(price_bin), !is.na(single_bid)) %>%
    dplyr::group_by(price_bin) %>%
    dplyr::summarise(
      share_single_bid = mean(single_bid, na.rm = TRUE),
      n = dplyr::n(),
      .groups = "drop"
    )
  
  ggplot2::ggplot(dat, ggplot2::aes(x = price_bin, y = share_single_bid)) +
    ggplot2::geom_col(fill = "#d73027", width = 0.7) +
    ggplot2::geom_text(
      ggplot2::aes(label = scales::percent(share_single_bid, accuracy = 1)),
      vjust = -0.3,
      size = 3.5
    ) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
    ggplot2::labs(
      x = "Contract value (USD)",
      y = "Share of single-bid tenders",
      title = "Share of single-bid tenders by contract value"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}

plot_single_bid_by_buyer_group <- function(df, buyer_type_col = "buyer_buyertype") {
  if (!buyer_type_col %in% names(df)) stop("Missing required column: ", buyer_type_col)
  
  dat <- df %>%
    dplyr::filter(!is.na(single_bid), !is.na(.data[[buyer_type_col]])) %>%
    dplyr::mutate(buyer_group = add_buyer_group(.data[[buyer_type_col]])) %>%
    dplyr::group_by(buyer_group) %>%
    dplyr::summarise(
      share_single_bid = mean(single_bid, na.rm = TRUE),
      share_multi_bid  = 1 - share_single_bid,
      n_tenders = dplyr::n(),
      .groups = "drop"
    ) %>%
    tidyr::pivot_longer(
      cols = c(share_single_bid, share_multi_bid),
      names_to = "bid_type",
      values_to = "share"
    ) %>%
    dplyr::mutate(bid_type = factor(bid_type, levels = c("share_single_bid", "share_multi_bid")))
  
  ggplot2::ggplot(dat, ggplot2::aes(x = buyer_group, y = share, fill = bid_type)) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::geom_text(
      ggplot2::aes(label = scales::percent(share, accuracy = 1)),
      position = ggplot2::position_stack(vjust = 0.5),
      color = "white",
      size = 3
    ) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
    ggplot2::scale_fill_manual(
      values = c("share_single_bid" = "#d73027", "share_multi_bid" = "#4575b4"),
      labels = c("Single bid", "Multiple bids")
    ) +
    ggplot2::labs(
      x = "Buyer group",
      y = "Share of tenders",
      fill = "",
      title = "Share of single-bid tenders by buyer group"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 20, hjust = 1))
}

plot_single_bid_by_price_and_procedure <- function(df, proc_col = "tender_proceduretype") {
  if (!proc_col %in% names(df)) stop("Missing required column: ", proc_col)
  
  dat <- df %>%
    dplyr::mutate(procedure_type = recode_procedure_type(.data[[proc_col]])) %>%
    dplyr::filter(!is.na(price_bin), !is.na(single_bid), !is.na(procedure_type)) %>%
    dplyr::group_by(price_bin, procedure_type) %>%
    dplyr::summarise(
      share_single_bid = mean(single_bid, na.rm = TRUE),
      n = dplyr::n(),
      .groups = "drop"
    )
  
  ggplot2::ggplot(dat, ggplot2::aes(x = price_bin, y = share_single_bid, group = procedure_type)) +
    ggplot2::geom_line() +
    ggplot2::geom_point(size = 1.8) +
    ggplot2::facet_wrap(~ procedure_type) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
    ggplot2::labs(
      x = "Contract value (USD)",
      y = "Share of single-bid tenders",
      title = "Single-bid incidence by contract value and procedure type"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}

top_markets_by_single_bid <- function(df, top_n = 5) {
  # expects: cpv_cluster, single_bid
  df %>%
    dplyr::filter(!is.na(cpv_cluster), !is.na(single_bid)) %>%
    dplyr::group_by(cpv_cluster) %>%
    dplyr::summarise(
      share_single_bid = mean(single_bid, na.rm = TRUE),
      n = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(share_single_bid)) %>%
    dplyr::slice_head(n = top_n) %>%
    dplyr::pull(cpv_cluster)
}

plot_single_bid_market_procedure_price_top <- function(df,
                                                       top_n_markets = 5,
                                                       proc_col = "tender_proceduretype") {
  required <- c("cpv_cluster", "price_bin", "single_bid", proc_col)
  missing <- setdiff(required, names(df))
  if (length(missing) > 0) stop("Missing required columns: ", paste(missing, collapse = ", "))
  
  top_markets <- top_markets_by_single_bid(df, top_n = top_n_markets)
  
  dat <- df %>%
    dplyr::mutate(procedure_type = recode_procedure_type(.data[[proc_col]])) %>%
    dplyr::filter(
      cpv_cluster %in% top_markets,
      !is.na(price_bin),
      !is.na(single_bid),
      !is.na(procedure_type)
    ) %>%
    dplyr::group_by(cpv_cluster, procedure_type, price_bin) %>%
    dplyr::summarise(
      share_single_bid = mean(single_bid, na.rm = TRUE),
      n = dplyr::n(),
      .groups = "drop"
    )
  
  ggplot2::ggplot(dat, ggplot2::aes(x = price_bin, y = share_single_bid)) +
    ggplot2::geom_col(fill = "#d73027", width = 0.7) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = ifelse(n > 0, scales::percent(share_single_bid, accuracy = 1), "")
      ),
      angle = 45,
      hjust = 0,
      vjust = -0.2,
      size = 2.6
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::percent_format(),
      limits = c(0, 1),
      expand = ggplot2::expansion(mult = c(0, 0.12))
    ) +
    ggplot2::coord_cartesian(ylim = c(0, 1.12), clip = "off") +
    ggplot2::facet_grid(
      rows = ggplot2::vars(cpv_cluster),
      cols = ggplot2::vars(procedure_type),
      scales = "free_x",
      labeller = ggplot2::labeller(
        cpv_cluster    = wrap_strip,
        procedure_type = wrap_strip
      )
    )+
    ggplot2::labs(
      x = "Contract value (USD)",
      y = "Share of single-bid tenders",
      title = "Single-bid shares by market, procedure type, and contract value",
      subtitle = paste0(
        "Top ", top_n_markets,
        " markets with the highest overall single-bid incidence"
      )
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      
      strip.placement = "outside",
      
      # more gap between strip and panel area
      strip.switch.pad.grid = grid::unit(0.35, "lines"),
      strip.switch.pad.wrap = grid::unit(0.35, "lines"),  # helps in some layouts
      
      # add padding inside strip boxes (space around text)
      strip.text.x = ggplot2::element_text(
        size = 9,
        angle = 90,
        hjust = 0,
        margin = ggplot2::margin(t = 6, b = 6)   # was 3/3
      ),
      strip.text.y = ggplot2::element_text(
        size = 9,
        angle = 45,
        hjust = 1,
        margin = ggplot2::margin(l = 6, r = 6)   # was 2/2
      ),
      
      # if you want more space between facet rows/cols:
      panel.spacing = grid::unit(1.0, "lines"),  # was 0.8
      
      plot.margin = ggplot2::margin(t = 14, r = 28, b = 10, l = 12)
    )
  
}

plot_top_buyers_single_bid <- function(df,
                                       buyer_id_col = "buyer_masterid",
                                       buyer_type_col = "buyer_buyertype",
                                       top_n = 20,
                                       min_tenders = 30) {
  if (!buyer_id_col %in% names(df)) stop("Missing required column: ", buyer_id_col)
  
  dat <- df %>%
    dplyr::filter(!is.na(single_bid), !is.na(.data[[buyer_id_col]])) %>%
    dplyr::group_by(.data[[buyer_id_col]]) %>%
    dplyr::summarise(
      share_single_bid = mean(single_bid, na.rm = TRUE),
      n_tenders = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::filter(n_tenders >= min_tenders) %>%
    dplyr::arrange(dplyr::desc(share_single_bid)) %>%
    dplyr::slice_head(n = top_n) %>%
    dplyr::rename(buyer_id = 1)
  
  # Optional: attach buyer group if buyer_type exists and has content
  if (!is.null(buyer_type_col) && buyer_type_col %in% names(df)) {
    buyer_types <- df %>%
      dplyr::filter(!is.na(.data[[buyer_id_col]]), !is.na(.data[[buyer_type_col]])) %>%
      dplyr::distinct(.data[[buyer_id_col]], .data[[buyer_type_col]]) %>%
      dplyr::rename(buyer_id = 1, buyer_type = 2) %>%
      dplyr::group_by(buyer_id) %>%
      dplyr::slice_head(n = 1) %>%
      dplyr::ungroup()
    
    dat <- dat %>%
      dplyr::left_join(buyer_types, by = "buyer_id") %>%
      dplyr::mutate(buyer_group = if ("buyer_type" %in% names(.)) add_buyer_group(buyer_type) else NA)
  } else {
    dat$buyer_group <- NA
  }
  
  ggplot2::ggplot(dat, ggplot2::aes(x = stats::reorder(buyer_id, share_single_bid), y = share_single_bid)) +
    ggplot2::geom_col(fill = "#d73027") +
    ggplot2::geom_text(ggplot2::aes(label = n_tenders), hjust = -0.2, size = 3.3) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
    ggplot2::labs(
      x = "Buyer",
      y = "Share of single-bid tenders",
      title = paste0("Top buyers by single-bid incidence (min ", min_tenders, " tenders)"),
      subtitle = "Numbers on bars indicate number of tenders"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::expand_limits(y = min(1, max(dat$share_single_bid, na.rm = TRUE) * 1.1))
}

# ------------------------------------------------------------------------
# 9) Pipeline: run_economic_efficiency_pipeline
# ------------------------------------------------------------------------

run_economic_efficiency_pipeline <- function(df,
                                             country_code = "GEN",
                                             output_dir,
                                             cpv_lookup = NULL,
                                             cpv_digits = 2,
                                             save_outputs = TRUE,
                                             network_cpv_clusters = character(0),
                                             network_top_buyers = 15,
                                             network_ncol = 2) {
  message("Running economic efficiency pipeline for ", country_code, " ...")
  
  # Keep behavior stable across fread/data.table vs tibble
  df <- tibble::as_tibble(df)
  
  # ----------------------------------------------------------------------
  # A) Standard fields
  # ----------------------------------------------------------------------
  df <- df %>%
    add_tender_year() %>%
    add_cpv_cluster(digits = cpv_digits) %>%
    add_single_bid_flag() %>%
    add_price_bins_usd()
  
  # Attach CPV category labels if lookup provided
  if (!is.null(cpv_lookup) && is.list(cpv_lookup) && "cpv_2d" %in% names(cpv_lookup)) {
    df <- attach_cpv_labels(df, cpv_lookup_2d = cpv_lookup$cpv_2d)
  } else {
    if (!"cpv_category" %in% names(df)) df$cpv_category <- NA_character_
  }
  
  # ----------------------------------------------------------------------
  # SUMMARY STATS BLOCK (returned, not printed)
  # ----------------------------------------------------------------------
  n_obs_per_year <- df %>% dplyr::count(tender_year, name = "n_observations")
  
  n_unique_buyers <- if ("buyer_masterid" %in% names(df)) {
    dplyr::n_distinct(df$buyer_masterid, na.rm = TRUE)
  } else NA_integer_
  
  n_unique_bidders <- if ("bidder_masterid" %in% names(df)) {
    dplyr::n_distinct(df$bidder_masterid, na.rm = TRUE)
  } else NA_integer_
  
  tender_year_tenders <- if ("tender_id" %in% names(df)) {
    df %>%
      dplyr::group_by(tender_year) %>%
      dplyr::summarise(n_unique_tender_id = dplyr::n_distinct(tender_id), .groups = "drop")
  } else NULL
  
  vars_present <- names(df)
  vars_present <- vars_present[!startsWith(vars_present, "ind_")]
  
  summary_stats <- list(
    n_obs_per_year      = n_obs_per_year,
    n_unique_buyers     = n_unique_buyers,
    tender_year_tenders = tender_year_tenders,
    n_unique_bidders    = n_unique_bidders,
    vars_present        = vars_present
  )
  
  # ----------------------------------------------------------------------
  # B) Market sizing
  # ----------------------------------------------------------------------
  market_summary <- summarise_market_size(df)
  
  # CPV cluster -> category legend (for the Rmd)
  cpv_cluster_legend <- NULL
  if (!is.null(market_summary) && all(c("cpv_cluster","cpv_category") %in% names(market_summary))) {
    cpv_cluster_legend <- make_cpv_cluster_legend(market_summary)
  }
  
  market_size_n  <- plot_market_contract_counts(market_summary)
  market_size_v  <- plot_market_total_value(market_summary)
  market_size_av <- plot_market_bubble(market_summary)
  
  
  # ----------------------------------------------------------------------
  # C) Supplier entry
  # ----------------------------------------------------------------------
  supplier_stats <- NULL
  suppliers_entrance <- NULL
  unique_supp <- NULL
  if (all(c("bidder_masterid", "tender_year", "cpv_cluster") %in% names(df))) {
    supplier_stats <- compute_supplier_entry(df)
    suppliers_entrance <- plot_supplier_shares_facet(supplier_stats)
    unique_supp <- plot_unique_suppliers_over_time(df)
  }
  
  # ----------------------------------------------------------------------
  # D) Networks (optional)
  # ----------------------------------------------------------------------
  network_plots <- list()
  if (length(network_cpv_clusters) > 0) {
    for (ccpv in network_cpv_clusters) {
      nm <- paste0("network_cpv_", ccpv)
      network_plots[[nm]] <- tryCatch(
        plot_buyer_supplier_networks(
          df,
          cpv_focus     = ccpv,
          n_top_buyers  = network_top_buyers,
          ncol          = network_ncol,
          country_code  = country_code   # <-- add this
        ),
        error = function(e) NULL
      )
    }
  }
  
  # ----------------------------------------------------------------------
  # E) Relative prices (only if columns exist)
  # ----------------------------------------------------------------------
  rel_tot <- rel_year <- rel_10 <- rel_buy <- NULL
  relative_price_data <- NULL
  top_markets <- NULL
  top_buyers_rel <- NULL
  market_proc_price_plot <- NULL
  
  
  if (all(c("bid_price", "lot_estimatedprice") %in% names(df))) {
    relative_price_data <- df %>% add_relative_price()
    
    rel_tot  <- plot_relative_price_density(relative_price_data)
    
    if ("tender_year" %in% names(relative_price_data)) {
      rel_year <- plot_relative_price_by_year(relative_price_data)
    }
    
    if ("cpv_category" %in% names(relative_price_data)) {
      top_markets <- top_markets_by_relative_price(relative_price_data, n = 10)
      rel_10 <- plot_top_markets_relative_price(relative_price_data, top_markets)
    }
    
    if ("buyer_name" %in% names(relative_price_data)) {
      top_buyers_rel <- top_buyers_by_relative_price(relative_price_data)
      if (nrow(top_buyers_rel) > 0) rel_buy <- plot_top_buyers_relative_price(top_buyers_rel)
    }
  }
  
  # ----------------------------------------------------------------------
  # F) Competition (single-bid)
  # ----------------------------------------------------------------------
  proc_plot <- price_plot <- buyer_group_plot <- price_proc_plot <- top_buyers_plot <- NULL
  
  if ("single_bid" %in% names(df) && any(!is.na(df$single_bid))) {
    
    if ("tender_proceduretype" %in% names(df)) {
      proc_plot <- plot_single_bid_by_procedure(df)
      
      # price × procedure (needs both price_bin and procedure)
      if ("price_bin" %in% names(df)) {
        price_proc_plot <- plot_single_bid_by_price_and_procedure(df)
      }
    }
    
    if ("price_bin" %in% names(df)) {
      price_plot <- plot_single_bid_by_price(df)
    }
    
    if ("buyer_buyertype" %in% names(df)) {
      buyer_group_plot <- plot_single_bid_by_buyer_group(df)
    }
    
    if (all(c("cpv_cluster", "price_bin", "tender_proceduretype") %in% names(df))) {
      market_proc_price_plot <- tryCatch(
        plot_single_bid_market_procedure_price_top(df, top_n_markets = 5),
        error = function(e) NULL
      )
    }
    
    # top buyers (needs buyer id)
    if ("buyer_masterid" %in% names(df)) {
      top_buyers_plot <- tryCatch(
        plot_top_buyers_single_bid(df, buyer_id_col = "buyer_masterid", buyer_type_col = "buyer_buyertype",
                                   top_n = 20, min_tenders = 30),
        error = function(e) NULL
      )
    }
  }
  # ----------------------------------------------------------------------
  # G) Save standard outputs (optional)
  # ----------------------------------------------------------------------
  if (isTRUE(save_outputs)) {
    dir_ensure(output_dir)
    
    save_plot(market_size_n,  output_dir, "market_size_n.png",  width = 10, height = 6)
    save_plot(market_size_v,  output_dir, "market_size_v.png",  width = 10, height = 6)
    save_plot(market_size_av, output_dir, "market_size_av.png", width = 10, height = 7)
    
    if (!is.null(suppliers_entrance)) save_plot(suppliers_entrance, output_dir, "suppliers_entrance.png", width = 10, height = 7)
    if (!is.null(unique_supp))        save_plot(unique_supp,        output_dir, "unique_supp.png",        width = 10, height = 7)
    
    if (!is.null(rel_tot))  save_plot(rel_tot,  output_dir, "rel_tot.png",  width = 10, height = 7)
    if (!is.null(rel_year)) save_plot(rel_year, output_dir, "rel_year.png", width = 10, height = 7)
    if (!is.null(rel_10))   save_plot(rel_10,   output_dir, "rel_10.png",   width = 10, height = 7)
    if (!is.null(rel_buy))  save_plot(rel_buy,  output_dir, "rel_buy.png",  width = 10, height = 7)
    
    if (!is.null(proc_plot))        save_plot(proc_plot,        output_dir, "single_bid_by_procedure.png", width = 10, height = 7)
    if (!is.null(price_plot))       save_plot(price_plot,       output_dir, "single_bid_by_price.png",     width = 10, height = 7)
    if (!is.null(buyer_group_plot)) save_plot(buyer_group_plot, output_dir, "single_bid_by_buyer_group.png", width = 10, height = 7)
    
    if (!is.null(price_proc_plot)) save_plot(price_proc_plot, output_dir, "single_bid_by_price_and_procedure.png", width = 12, height = 8)
    if (!is.null(top_buyers_plot)) save_plot(top_buyers_plot, output_dir, "top_buyers_single_bid.png", width = 10, height = 7)
    
    if (!is.null(market_proc_price_plot)) {
      save_plot(market_proc_price_plot, output_dir, "single_bid_market_procedure_price_top.png", width = 14, height = 10)
    }
    
    if (length(network_plots) > 0) {
      for (nm in names(network_plots)) {
        p <- network_plots[[nm]]
        if (!is.null(p)) save_plot(p, output_dir, paste0(nm, ".png"), width = 10, height = 7)
      }
    }
  }
  
  # ----------------------------------------------------------------------
  # H) Collect outputs (like your admin pipeline)
  # ----------------------------------------------------------------------
  results <- list(
    country_code = country_code,
    summary_stats = summary_stats,
    
    # cleaned/enriched data
    df = df,
    
    # market sizing
    market_summary = market_summary,
    market_size_n  = market_size_n,
    market_size_v  = market_size_v,
    market_size_av = market_size_av,
    
    # supplier dynamics
    supplier_stats     = supplier_stats,
    suppliers_entrance = suppliers_entrance,
    unique_supp        = unique_supp,
    
    # networks
    network_plots = network_plots,
    
    # relative prices
    relative_price_data = relative_price_data,
    top_markets_relative_price = top_markets,
    top_buyers_relative_price  = top_buyers_rel,
    rel_tot  = rel_tot,
    rel_year = rel_year,
    rel_10   = rel_10,
    rel_buy  = rel_buy,
    single_bid_market_procedure_price_top = market_proc_price_plot,
    
    # competition
    single_bid_by_procedure           = proc_plot,
    single_bid_by_price               = price_plot,
    single_bid_by_price_and_procedure = price_proc_plot,
    single_bid_by_buyer_group         = buyer_group_plot,
    top_buyers_single_bid             = top_buyers_plot,
    
    #cpv clusters definition
    cpv_cluster_legend = cpv_cluster_legend
    
    
  )
  
  invisible(results)
}
