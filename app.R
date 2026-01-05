# app.R  (LOCAL: reads big files from disk; multi-country; editable thresholds; plot downloads; render HTML)

# =======================
# 0) Set your local root
# =======================
ROOT_DIR <- "C:/Users/wb589991/OneDrive - WBG/Documents/procuR.github.io"

# Expecting your utils file here (adjust if different)
UTILS_PATH <- file.path(ROOT_DIR, "admin_utils.R")

# Expecting your big exports here (adjust if different)
# Put files like BG_export.csv, UY_export.csv into this folder.
DATA_DIR_DEFAULT <- file.path(ROOT_DIR, "data")

# =======================
# 1) Package bootstrap
# =======================
install_missing_packages <- function(pkgs,
                                     repos = getOption("repos"),
                                     dependencies = TRUE,
                                     quiet = FALSE,
                                     load = FALSE) {
  stopifnot(is.character(pkgs), length(pkgs) > 0)
  
  pkgs <- unique(trimws(pkgs))
  pkgs <- pkgs[nzchar(pkgs)]
  
  installed <- rownames(installed.packages())
  missing <- setdiff(pkgs, installed)
  
  if (length(missing) > 0) {
    if (!quiet) message("Installing missing packages: ", paste(missing, collapse = ", "))
    if (is.null(repos) || is.na(repos["CRAN"]) || repos["CRAN"] == "@CRAN@") {
      repos <- c(CRAN = "https://cloud.r-project.org")
    }
    install.packages(missing, repos = repos, dependencies = dependencies, quiet = quiet)
  } else {
    if (!quiet) message("All required packages are already installed.")
  }
  
  if (isTRUE(load)) {
    invisible(lapply(pkgs, function(p) {
      suppressPackageStartupMessages(require(p, character.only = TRUE))
    }))
  }
  
  invisible(list(
    requested = pkgs,
    missing_installed = missing,
    already_present = intersect(pkgs, installed)
  ))
}

required_pkgs <- c(
  "shiny", "shinyWidgets", "DT",
  "data.table", "dplyr", "tidyr", "stringr", "forcats", "rlang",
  "ggplot2", "patchwork", "scales", "ggrepel",
  "ggeffects", "sandwich", "lmtest",
  "rmarkdown"
)
install_missing_packages(required_pkgs, load = TRUE)

# =======================
# 2) Load your pipeline utils
# =======================
if (!file.exists(UTILS_PATH)) {
  stop("Cannot find admin_utils.R at: ", UTILS_PATH, "\nAdjust ROOT_DIR / UTILS_PATH at the top of app.R.")
}
source(UTILS_PATH)

# =======================
# 3) Helpers
# =======================
`%||%` <- function(a, b) if (!is.null(a)) a else b

safe_country_from_filename <- function(path) {
  fn <- basename(path)
  cc <- toupper(stringr::str_match(fn, "^([A-Za-z]{2,3})_")[, 2])
  ifelse(is.na(cc), "GEN", cc)
}

make_output_dir <- function(tag = "admin_eff") {
  out <- file.path(tempdir(), paste0(tag, "_", as.integer(Sys.time()), "_", sample.int(9999, 1)))
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  out
}

# In your app we will store overrides into admin_threshold_config[[cc]] like:
# list(
#   subm = list(short=, medium=, long=),
#   decp = list(short=, medium=, long=),
#   use_proc_specific = TRUE/FALSE
# )
build_override_cfg <- function(cc, input) {
  list(
    subm = list(
      short  = input[[paste0("subm_short__", cc)]],
      medium = input[[paste0("subm_med__", cc)]],
      long   = input[[paste0("subm_long__", cc)]]
    ),
    decp = list(
      short  = input[[paste0("decp_short__", cc)]],
      medium = input[[paste0("decp_med__", cc)]],
      long   = input[[paste0("decp_long__", cc)]]
    ),
    use_proc_specific = isTRUE(input[[paste0("use_proc__", cc)]])
  )
}

with_threshold_overrides <- function(cc, override_cfg, expr) {
  old <- admin_threshold_config[[cc]] %||% NULL
  on.exit({
    if (is.null(old)) {
      admin_threshold_config[[cc]] <<- NULL
      admin_threshold_config <<- admin_threshold_config[!vapply(admin_threshold_config, is.null, logical(1))]
    } else {
      admin_threshold_config[[cc]] <<- old
    }
  }, add = TRUE)
  
  admin_threshold_config[[cc]] <<- override_cfg
  force(expr)
}

extract_plots <- function(res) {
  # Keep only ggplot / patchwork objects
  candidates <- res
  candidates <- candidates[!names(candidates) %in% c("summary_stats", "models", "coef_plots", "regression")]
  plots <- list()
  for (nm in names(candidates)) {
    obj <- candidates[[nm]]
    if (inherits(obj, "ggplot") || inherits(obj, "patchwork")) {
      plots[[nm]] <- obj
    }
  }
  plots
}

scan_exports <- function(data_dir) {
  files <- list.files(
    data_dir,
    pattern = "_export\\.(csv)$",
    full.names = TRUE
  )
  if (length(files) == 0) return(list(countries = character(0), data_map = list()))
  countries <- unique(vapply(files, safe_country_from_filename, character(1)))
  
  data_map <- setNames(vector("list", length(countries)), countries)
  for (cc in countries) {
    f_cc <- files[vapply(files, safe_country_from_filename, character(1)) == cc]
    data_map[[cc]] <- f_cc[[1]]
  }
  list(countries = countries, data_map = data_map)
}

# =======================
# 4) UI
# =======================
ui <- shiny::navbarPage(
  title = "Administrative Efficiency — Local Folder Mode",
  tabPanel(
    "1) Data & Run",
    sidebarLayout(
      sidebarPanel(
        textInput("data_dir", "Data folder (contains *_export.csv)", value = DATA_DIR_DEFAULT),
        actionButton("scan_btn", "Scan folder"),
        tags$hr(),
        pickerInput("country_pick", "Countries found", choices = character(0), multiple = TRUE),
        actionButton("run_selected_btn", "Run selected", class = "btn-primary"),
        actionButton("run_all_btn", "Run ALL", class = "btn-secondary"),
        actionButton("clear_btn", "Clear results", class = "btn-warning"),
        tags$hr(),
        checkboxInput("auto_run_on_change", "Auto-run when thresholds change", FALSE),
        width = 4
      ),
      mainPanel(
        h4("Folder scan"),
        verbatimTextOutput("scan_status"),
        tags$hr(),
        h4("Run status"),
        uiOutput("run_status_all"),
        tags$hr(),
        h4("Summary preview"),
        pickerInput("country_preview", "Preview country", choices = character(0)),
        DTOutput("contracts_per_year_tbl"),
        uiOutput("unique_counts"),
        DTOutput("unique_tenders_tbl"),
        width = 8
      )
    )
  ),
  
  tabPanel(
    "2) Thresholds",
    fluidRow(
      column(12, uiOutput("thresholds_ui"))
    )
  ),
  
  tabPanel(
    "3) Plots (multi-country)",
    fluidRow(
      column(12, uiOutput("plots_ui"))
    )
  ),
  
  tabPanel(
    "4) Download a plot",
    sidebarLayout(
      sidebarPanel(
        pickerInput("dl_country", "Country", choices = character(0)),
        uiOutput("dl_plot_pick_ui"),
        radioButtons("dl_format", "Format", choices = c("PNG" = "png", "PDF" = "pdf"), inline = TRUE),
        numericInput("dl_width", "Width (inches)", value = 10, min = 4, max = 30, step = 0.5),
        numericInput("dl_height", "Height (inches)", value = 6, min = 3, max = 30, step = 0.5),
        numericInput("dl_dpi", "DPI (PNG only)", value = 300, min = 72, max = 600, step = 25),
        downloadButton("download_plot", "Download selected plot"),
        width = 4
      ),
      mainPanel(
        h4("Preview"),
        plotOutput("dl_preview", height = "520px"),
        width = 8
      )
    )
  ),
  
  tabPanel(
    "5) Render HTML report",
    sidebarLayout(
      sidebarPanel(
        textInput("report_title", "Report title", value = "Administrative Efficiency Report"),
        checkboxInput("report_self_contained", "Self-contained HTML (bigger file)", TRUE),
        actionButton("render_report_btn", "Render HTML report", class = "btn-primary"),
        downloadButton("download_report_html", "Download rendered HTML"),
        width = 4
      ),
      mainPanel(
        uiOutput("report_status"),
        tags$hr(),
        verbatimTextOutput("report_path_txt"),
        width = 8
      )
    )
  )
)

# =======================
# 5) Server
# =======================
server <- function(input, output, session) {
  
  values <- reactiveValues(
    data_map = NULL,
    countries = character(0),
    report_path = NULL
  )
  
  results_store <- reactiveVal(list())
  outdirs_store <- reactiveVal(list())
  
  # ---- Scan folder
  observeEvent(input$scan_btn, {
    req(nzchar(input$data_dir))
    validate(need(dir.exists(input$data_dir), paste0("Folder does not exist: ", input$data_dir)))
    
    s <- scan_exports(input$data_dir)
    values$countries <- s$countries
    values$data_map <- s$data_map
    
    updatePickerInput(session, "country_pick", choices = values$countries, selected = values$countries)
    updatePickerInput(session, "country_preview", choices = values$countries, selected = values$countries[[1]] %||% character(0))
    updatePickerInput(session, "dl_country", choices = values$countries, selected = values$countries[[1]] %||% character(0))
  }, ignoreInit = TRUE)
  
  output$scan_status <- renderText({
    if (length(values$countries) == 0) {
      paste0("No countries loaded. Click 'Scan folder'.\nExpected files like: BG_export.csv, UY_export.csv\nCurrent folder: ", input$data_dir)
    } else {
      paste0("Found ", length(values$countries), " country file(s):\n",
             paste(values$countries, collapse = ", "),
             "\n\nFolder: ", input$data_dir)
    }
  })
  
  # ---- Per-country thresholds UI
  output$thresholds_ui <- renderUI({
    req(length(values$countries) > 0)
    ccs <- values$countries
    
    blocks <- lapply(ccs, function(cc) {
      cfg <- admin_threshold_config[[cc]] %||% list()
      subm <- cfg$subm %||% list(short = NA, medium = NA, long = NA)
      decp <- cfg$decp %||% list(short = NA, medium = NA, long = NA)
      use_proc <- cfg$use_proc_specific %||% TRUE
      
      shiny::tags$div(
        style = "border:1px solid #ddd; border-radius:10px; padding:12px; margin-bottom:12px;",
        shiny::tags$h4(paste0("Thresholds — ", cc)),
        fluidRow(
          column(
            4,
            tags$h5("Submission period (days)"),
            numericInput(paste0("subm_short__", cc), "Short threshold",  value = subm$short  %||% NA, min = 0, step = 1),
            numericInput(paste0("subm_med__", cc),   "Medium threshold", value = subm$medium %||% NA, min = 0, step = 1),
            numericInput(paste0("subm_long__", cc),  "Long threshold",   value = subm$long   %||% NA, min = 0, step = 1)
          ),
          column(
            4,
            tags$h5("Decision period (days)"),
            numericInput(paste0("decp_short__", cc), "Short threshold",  value = decp$short  %||% NA, min = 0, step = 1),
            numericInput(paste0("decp_med__", cc),   "Medium threshold", value = decp$medium %||% NA, min = 0, step = 1),
            numericInput(paste0("decp_long__", cc),  "Long threshold",   value = decp$long   %||% NA, min = 0, step = 1)
          ),
          column(
            4,
            tags$h5("Options"),
            checkboxInput(paste0("use_proc__", cc), "Prefer procedure-specific thresholds (if configured)", value = isTRUE(use_proc)),
            actionButton(paste0("run_one__", cc), paste0("Run ", cc), class = "btn-secondary")
          )
        )
      )
    })
    
    tagList(
      tags$div(style="color:#666; margin-bottom:10px;",
               "These thresholds are applied per country before the pipeline runs. If you leave a threshold as NA, your pipeline’s fallback logic will still apply."),
      tagList(blocks)
    )
  })
  
  # ---- Run pipeline for one country
  run_one_country <- function(cc) {
    req(values$data_map)
    validate(need(!is.null(values$data_map[[cc]]), paste0("No file mapped for country ", cc)))
    
    csv_path <- values$data_map[[cc]]
    validate(need(file.exists(csv_path), paste0("File not found on disk: ", csv_path)))
    
    out_dir <- make_output_dir(paste0("admin_eff_", cc))
    df <- load_data(csv_path)
    
    override_cfg <- build_override_cfg(cc, input)
    
    res <- with_threshold_overrides(cc, override_cfg, {
      run_admin_efficiency_pipeline(df = df, country_code = cc, output_dir = out_dir)
    })
    
    cur <- results_store()
    cur[[cc]] <- res
    results_store(cur)
    
    ods <- outdirs_store()
    ods[[cc]] <- out_dir
    outdirs_store(ods)
  }
  
  # ---- Bind per-country "Run" buttons + optional auto-run
  observe({
    req(length(values$countries) > 0)
    for (cc in values$countries) {
      local({
        cc_local <- cc
        
        observeEvent(input[[paste0("run_one__", cc_local)]], {
          run_one_country(cc_local)
        }, ignoreInit = TRUE)
        
        observeEvent(
          list(
            input[[paste0("subm_short__", cc_local)]],
            input[[paste0("subm_med__", cc_local)]],
            input[[paste0("subm_long__", cc_local)]],
            input[[paste0("decp_short__", cc_local)]],
            input[[paste0("decp_med__", cc_local)]],
            input[[paste0("decp_long__", cc_local)]],
            input[[paste0("use_proc__", cc_local)]]
          ),
          {
            if (isTRUE(input$auto_run_on_change)) run_one_country(cc_local)
          },
          ignoreInit = TRUE
        )
      })
    }
  })
  
  # ---- Run selected / all
  observeEvent(input$run_selected_btn, {
    req(input$country_pick)
    for (cc in input$country_pick) run_one_country(cc)
  }, ignoreInit = TRUE)
  
  observeEvent(input$run_all_btn, {
    req(length(values$countries) > 0)
    for (cc in values$countries) run_one_country(cc)
  }, ignoreInit = TRUE)
  
  observeEvent(input$clear_btn, {
    results_store(list())
    outdirs_store(list())
    values$report_path <- NULL
  }, ignoreInit = TRUE)
  
  # ---- Run status UI
  output$run_status_all <- renderUI({
    ccs <- values$countries %||% character(0)
    rs <- results_store()
    done <- intersect(names(rs), ccs)
    
    if (length(ccs) == 0) return(tags$div(style="color:#666;", "Scan a folder first."))
    
    tags$div(
      tags$div(tags$strong("Completed: "), if (length(done) == 0) "none" else paste(done, collapse = ", ")),
      tags$div(tags$strong("Pending: "), if (length(setdiff(ccs, done)) == 0) "none" else paste(setdiff(ccs, done), collapse = ", "))
    )
  })
  
  # ---- Preview country summaries
  current_results <- reactive({
    req(input$country_preview)
    res <- results_store()[[input$country_preview]]
    validate(need(!is.null(res), "Run the pipeline for this country first (Data & Run → Run selected/ALL or Thresholds → Run country)."))
    res
  })
  
  output$contracts_per_year_tbl <- renderDT({
    res <- current_results()
    summ <- res$summary_stats
    req(summ$n_obs_per_year)
    DT::datatable(as.data.frame(summ$n_obs_per_year), options = list(pageLength = 10), rownames = FALSE)
  })
  
  output$unique_counts <- renderUI({
    res <- current_results()
    summ <- res$summary_stats
    tags$ul(
      tags$li(strong("Unique buyers (buyer_masterid): "), summ$n_unique_buyers %||% NA),
      tags$li(strong("Unique bidders (bidder_masterid): "), summ$n_unique_bidders %||% NA)
    )
  })
  
  output$unique_tenders_tbl <- renderDT({
    res <- current_results()
    summ <- res$summary_stats
    validate(need(!is.null(summ$tender_year_tenders), "No tender_id column found; unique tenders per year not available."))
    DT::datatable(as.data.frame(summ$tender_year_tenders), options = list(pageLength = 10), rownames = FALSE)
  })
  
  # ---- Plots UI (tabs per country)
  output$plots_ui <- renderUI({
    req(length(values$countries) > 0)
    ccs <- values$countries
    
    tabs <- lapply(ccs, function(cc) {
      tabPanel(title = cc, uiOutput(paste0("plots_block__", cc)))
    })
    do.call(tabsetPanel, c(tabs, list(type = "tabs")))
  })
  
  observe({
    req(length(values$countries) > 0)
    for (cc in values$countries) {
      local({
        cc_local <- cc
        
        output[[paste0("plots_block__", cc_local)]] <- renderUI({
          res <- results_store()[[cc_local]]
          if (is.null(res)) return(tags$div(style="color:#666;", paste0("No results yet for ", cc_local, ". Run it first.")))
          
          plots <- extract_plots(res)
          if (length(plots) == 0) return(tags$div(style="color:#666;", "No ggplot/patchwork objects found in results."))
          
          tagList(
            tags$h4(paste0("Plots — ", cc_local)),
            tagList(lapply(names(plots), function(pnm) {
              plotOutput(paste0("plot__", cc_local, "__", pnm), height = "420px")
            }))
          )
        })
        
        observeEvent(results_store(), {
          res <- results_store()[[cc_local]]
          if (is.null(res)) return()
          plots <- extract_plots(res)
          
          for (pnm in names(plots)) {
            local({
              pnm_local <- pnm
              output[[paste0("plot__", cc_local, "__", pnm_local)]] <- renderPlot({
                rr <- results_store()[[cc_local]]
                req(rr)
                pp <- extract_plots(rr)[[pnm_local]]
                req(pp)
                pp
              })
            })
          }
        }, ignoreInit = TRUE)
      })
    }
  })
  
  # ---- Download plot tab
  observeEvent(values$countries, {
    if (length(values$countries) > 0) {
      updatePickerInput(session, "dl_country", choices = values$countries, selected = values$countries[[1]])
    }
  }, ignoreInit = TRUE)
  
  output$dl_plot_pick_ui <- renderUI({
    req(input$dl_country)
    res <- results_store()[[input$dl_country]]
    if (is.null(res)) return(tags$div(style="color:#666;", "Run the selected country first."))
    
    plots <- extract_plots(res)
    if (length(plots) == 0) return(tags$div(style="color:#666;", "No plots available."))
    
    pickerInput("dl_plot_name", "Plot", choices = names(plots), selected = names(plots)[[1]])
  })
  
  output$dl_preview <- renderPlot({
    req(input$dl_country, input$dl_plot_name)
    res <- results_store()[[input$dl_country]]
    req(res)
    pp <- extract_plots(res)[[input$dl_plot_name]]
    req(pp)
    pp
  })
  
  output$download_plot <- downloadHandler(
    filename = function() {
      req(input$dl_country, input$dl_plot_name, input$dl_format)
      paste0("plot_", input$dl_country, "_", input$dl_plot_name, ".", input$dl_format)
    },
    content = function(file) {
      req(input$dl_country, input$dl_plot_name, input$dl_format)
      res <- results_store()[[input$dl_country]]
      req(res)
      pp <- extract_plots(res)[[input$dl_plot_name]]
      req(pp)
      
      if (input$dl_format == "png") {
        ggsave(file, plot = pp, width = input$dl_width, height = input$dl_height, dpi = input$dl_dpi)
      } else {
        ggsave(file, plot = pp, width = input$dl_width, height = input$dl_height)
      }
    }
  )
  
  # ---- Render HTML report
  make_report_rmd <- function(utils_path) {
    c(
      "---",
      "title: !expr params$title",
      "output:",
      "  html_document:",
      "    self_contained: !expr params$self_contained",
      "params:",
      "  title: \"Administrative Efficiency Report\"",
      "  self_contained: true",
      "  csv_map: !r list()",
      "  thresholds: !r list()",
      sprintf("  utils_path: \"%s\"", gsub("\\\\", "/", utils_path)),
      "---",
      "",
      "```{r setup, include=FALSE}",
      "knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE)",
      "library(data.table); library(dplyr); library(tidyr); library(stringr); library(forcats); library(rlang)",
      "library(ggplot2); library(patchwork); library(scales); library(ggeffects); library(sandwich); library(lmtest); library(ggrepel)",
      "source(params$utils_path)",
      "`%||%` <- function(a,b) if (!is.null(a)) a else b",
      "with_threshold_overrides <- function(cc, override_cfg, expr) {",
      "  old <- admin_threshold_config[[cc]] %||% NULL",
      "  on.exit({",
      "    if (is.null(old)) {",
      "      admin_threshold_config[[cc]] <<- NULL",
      "      admin_threshold_config <<- admin_threshold_config[!vapply(admin_threshold_config, is.null, logical(1))]",
      "    } else {",
      "      admin_threshold_config[[cc]] <<- old",
      "    }",
      "  }, add = TRUE)",
      "  admin_threshold_config[[cc]] <<- override_cfg",
      "  force(expr)",
      "}",
      "```",
      "",
      "# Countries included",
      "```{r}",
      "names(params$csv_map)",
      "```",
      "",
      "# Results",
      "```{r}",
      "for (cc in names(params$csv_map)) {",
      "  cat('\\n\\n## ', cc, '\\n')",
      "  df <- load_data(params$csv_map[[cc]])",
      "  out_dir <- tempdir()",
      "  override_cfg <- params$thresholds[[cc]] %||% list()",
      "  res <- with_threshold_overrides(cc, override_cfg, {",
      "    run_admin_efficiency_pipeline(df = df, country_code = cc, output_dir = out_dir)",
      "  })",
      "  cat('\\n### Summary\\n')",
      "  print(res$summary_stats$n_obs_per_year)",
      "  cat('\\nUnique buyers: ', res$summary_stats$n_unique_buyers, '\\n')",
      "  cat('Unique bidders: ', res$summary_stats$n_unique_bidders, '\\n')",
      "  for (nm in names(res)) {",
      "    obj <- res[[nm]]",
      "    if (inherits(obj, 'ggplot') || inherits(obj, 'patchwork')) {",
      "      cat('\\n### ', nm, '\\n')",
      "      print(obj)",
      "    }",
      "  }",
      "}",
      "```"
    )
  }
  
  observeEvent(input$render_report_btn, {
    req(length(values$countries) > 0, values$data_map)
    
    ccs <- values$countries
    csv_map <- values$data_map
    
    thresholds <- lapply(ccs, function(cc) build_override_cfg(cc, input))
    names(thresholds) <- ccs
    
    rmd_path <- file.path(tempdir(), paste0("admin_eff_report_", as.integer(Sys.time()), ".Rmd"))
    writeLines(make_report_rmd(UTILS_PATH), rmd_path)
    
    out_html <- file.path(tempdir(), paste0("admin_eff_report_", as.integer(Sys.time()), ".html"))
    
    rmarkdown::render(
      input = rmd_path,
      output_file = out_html,
      params = list(
        title = input$report_title,
        self_contained = isTRUE(input$report_self_contained),
        csv_map = csv_map,
        thresholds = thresholds,
        utils_path = UTILS_PATH
      ),
      envir = new.env(parent = globalenv()),
      quiet = TRUE
    )
    
    values$report_path <- out_html
  }, ignoreInit = TRUE)
  
  output$report_status <- renderUI({
    if (is.null(values$report_path)) {
      tags$div(style="color:#666;", "No report rendered yet. Click “Render HTML report”.")
    } else {
      tags$div(style="color:darkgreen; font-weight:600;", "Report rendered. Use the download button to save it.")
    }
  })
  
  output$report_path_txt <- renderText({
    values$report_path %||% ""
  })
  
  output$download_report_html <- downloadHandler(
    filename = function() paste0("admin_eff_report_", format(Sys.Date()), ".html"),
    content = function(file) {
      validate(need(!is.null(values$report_path), "Render the report first."))
      file.copy(values$report_path, file, overwrite = TRUE)
    }
  )
}

# =======================
# 6) Run
# =======================
shinyApp(ui, server)

