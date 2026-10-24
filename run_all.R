# Set your working directory to this publication folder.
# raw.xlsx stays in the parent folder; edit this path if needed.
raw_file <- "../raw.xlsx"
output_folder <- "outputs"
dir.create(output_folder,showWarnings=FALSE)
source("01_prepare_and_table1.R")
source("02_regressions_and_figures.R")
source("03_verify.R")
