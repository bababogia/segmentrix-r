# Run this ONCE (in RStudio: open the file and click "Source").
# It installs every package Segmentrix-R needs. It can take 5-15 minutes the first time.

needed <- c(
  "shiny", "bslib",                 # app + modern layout
  "tidyverse",                      # dplyr, ggplot2, tidyr, lubridate, tibble, scales ...
  "data.table",                     # fast CSV reading + aggregation
  "cluster", "factoextra",          # silhouette + PCA plots
  "plotly", "reactable"             # interactive charts + tables
)

missing <- needed[!needed %in% rownames(installed.packages())]

if (length(missing) > 0) {
  message("Installing: ", paste(missing, collapse = ", "))
  install.packages(missing)
} else {
  message("All packages are already installed.")
}

# Version check: Segmentrix-R needs R >= 4.1 (for the |> pipe) and bslib >= 0.6
stopifnot(getRversion() >= "4.1.0")
message("R version OK: ", getRversion())
message("bslib version: ", as.character(packageVersion("bslib")),
        " (needs >= 0.6.0; run install.packages('bslib') to update if lower)")
