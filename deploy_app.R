# ========================================================================
# DEPLOYMENT SCRIPT FOR ECONOMIC OUTCOMES SHINY APP
# ========================================================================
# This script helps you deploy your app to Posit Connect
# 
# BEFORE RUNNING:
# 1. Ensure econ_app.R, econ_out_utils.R, and DESCRIPTION are in same folder
# 2. Fix the source path in econ_app.R (line 34) to use "econ_out_utils.R"
# 3. Configure your Posit Connect credentials below
# ========================================================================

library(rsconnect)

# ========================================================================
# CONFIGURATION - UPDATE THESE VALUES
# ========================================================================

# Your app directory (where all files are located)
app_directory <- "path/to/your/app/folder"

# Your Posit Connect details
connect_server <- "your-connect-server-name"  # e.g., "company-connect"
connect_url <- "https://your-connect-url.com"  # Your Posit Connect URL
app_title <- "Economic Outcomes Analysis"

# ========================================================================
# STEP 1: SET UP POSIT CONNECT ACCOUNT (FIRST TIME ONLY)
# ========================================================================
# If this is your first time deploying, uncomment and run this section:

# rsconnect::setAccountInfo(
#   name = connect_server,
#   server = connect_url,
#   token = "YOUR_TOKEN_HERE",      # Get from Posit Connect
#   secret = "YOUR_SECRET_HERE"     # Get from Posit Connect
# )

# To get your token and secret:
# 1. Log into Posit Connect web interface
# 2. Click your name (top right) → "API Keys"
# 3. Click "New API Key"
# 4. Copy the token and secret

# ========================================================================
# STEP 2: VERIFY FILES
# ========================================================================

cat("Checking files in:", app_directory, "\n")

required_files <- c("econ_app.R", "econ_out_utils.R", "DESCRIPTION")

files_exist <- sapply(required_files, function(f) {
  file.exists(file.path(app_directory, f))
})

if (all(files_exist)) {
  cat("✓ All required files found!\n")
  cat("  - econ_app.R\n")
  cat("  - econ_out_utils.R\n")
  cat("  - DESCRIPTION\n\n")
} else {
  cat("✗ Missing files:\n")
  missing <- required_files[!files_exist]
  for (f in missing) {
    cat("  -", f, "\n")
  }
  stop("Please ensure all required files are in the app directory.")
}

# ========================================================================
# STEP 3: CHECK SOURCE PATH IN APP
# ========================================================================

cat("Checking source path in econ_app.R...\n")

app_content <- readLines(file.path(app_directory, "econ_app.R"))
source_line <- grep('source\\(".*econ_out_utils.R"\\)', app_content, value = TRUE)

if (length(source_line) > 0) {
  cat("Found source line:", source_line[1], "\n")
  
  # Check if it's using absolute path
  if (grepl("C:/|/Users/|~|\\.\\.|\\\\/", source_line[1])) {
    warning("WARNING: Detected absolute or relative path in source().")
    cat("  Current: ", source_line[1], "\n")
    cat("  Should be: source(\"econ_out_utils.R\")\n")
    cat("  Please fix this before deploying!\n\n")
    
    response <- readline(prompt = "Continue anyway? (yes/no): ")
    if (tolower(response) != "yes") {
      stop("Deployment cancelled. Please fix the source path first.")
    }
  } else {
    cat("✓ Source path looks good!\n\n")
  }
}

# ========================================================================
# STEP 4: DEPLOY TO POSIT CONNECT
# ========================================================================

cat("========================================\n")
cat("Ready to deploy!\n")
cat("========================================\n")
cat("App directory:", app_directory, "\n")
cat("App title:", app_title, "\n")
cat("Server:", connect_server, "\n")
cat("========================================\n\n")

response <- readline(prompt = "Deploy now? (yes/no): ")

if (tolower(response) == "yes") {
  cat("\nDeploying...\n")
  
  tryCatch({
    rsconnect::deployApp(
      appDir = app_directory,
      appFiles = c("econ_app.R", "econ_out_utils.R", "DESCRIPTION"),
      appTitle = app_title,
      account = connect_server,
      server = connect_url,
      forceUpdate = TRUE,
      launch.browser = TRUE
    )
    
    cat("\n========================================\n")
    cat("✓ Deployment successful!\n")
    cat("========================================\n")
    cat("Your app should open in your browser.\n")
    cat("You can also access it from the Posit Connect dashboard.\n")
    
  }, error = function(e) {
    cat("\n========================================\n")
    cat("✗ Deployment failed!\n")
    cat("========================================\n")
    cat("Error:", e$message, "\n\n")
    cat("Common issues:\n")
    cat("1. Check your credentials are correct\n")
    cat("2. Ensure you have permission to deploy\n")
    cat("3. Verify the server URL is correct\n")
    cat("4. Check that all packages are available on the server\n")
  })
  
} else {
  cat("\nDeployment cancelled.\n")
}

# ========================================================================
# ALTERNATIVE: DEPLOY USING RSTUDIO PUBLISH BUTTON
# ========================================================================
# If you prefer using RStudio's UI:
# 1. Open econ_app.R in RStudio
# 2. Click the blue "Publish" button (top right of editor)
# 3. Select "Posit Connect"
# 4. Choose your account
# 5. Select files to publish:
#    ✓ econ_app.R
#    ✓ econ_out_utils.R
#    ✓ DESCRIPTION
# 6. Click "Publish"
