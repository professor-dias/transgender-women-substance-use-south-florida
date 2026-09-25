# Run once. Requires R 4.1 or newer.
packages <- c("readxl", "dplyr", "readr", "writexl", "ggplot2",
              "patchwork", "logistf", "ragg", "systemfonts", "scales")
missing_packages <- setdiff(packages, rownames(installed.packages()))
if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}
