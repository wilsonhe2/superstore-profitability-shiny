#Download required packages
required_pkgs <- c(
  "shiny", "tidyverse", "bslib", "thematic", "shinycssloaders",
  "shinyjs", "DT", "plotly", "readr"
)

missing_pkgs <- required_pkgs[!required_pkgs %in% rownames(installed.packages())]

if (length(missing_pkgs) > 0) {
  install.packages(missing_pkgs, dependencies = TRUE)
}