#==================================================================================
# ULTIMATE META-ANALYSIS SHINY DASHBOARD
# Version 2.0.0+
#==================================================================================

library(shiny)
library(bs4Dash)
library(DT)
library(waiter)

# This standalone app.R allows running the Shiny dashboard directly
# The full implementation is in the cbamm package R/shiny_dashboard.R

# For now, create a simple version that loads cbamm
library(cbamm)

cat("Loading Ultimate Meta-Analysis Dashboard...\n")

# Source the main dashboard function
source(system.file("R", "shiny_dashboard.R", package = "cbamm"))

# Create and run the app
create_meta_dashboard_app()
