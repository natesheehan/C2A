reproducible = FALSE

if (reproducible == FALSE) {
  source("code/utils.r")
  source("code/pkgs.r")
  main_df = readRDS("data/main_df.rds")
} else {
  source("code/utils.r")
  source("code/pkgs.r")
  source("code/owid-data.r")
  source("code/data-wrangle.r")
  source("code/visualise-database-landscape.R")
  source("code/corpus-analysis.R")
}
