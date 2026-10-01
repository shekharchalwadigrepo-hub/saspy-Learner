# Convenience launcher. From the repo root:
#   source("run_app.R")
if (!requireNamespace("shiny", quietly = TRUE)) {
  stop("Install shiny first: install.packages(\"shiny\")", call. = FALSE)
}
shiny::runApp(appDir = ".", launch.browser = TRUE)
