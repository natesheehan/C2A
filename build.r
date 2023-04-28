reproducible = FALSE

if (reproducible == FALSE) {
  source("code/utils.r")
  source("code/packages.r")
  main_df = readRDS("data/main_df.rds")
} else {
  source("code/utils.r") # utility functions used throughout the analysis, these functions are well documented are server as a good reference for understanding the nitty gritty details
  source("code/packages.r") # load packages
  source("code/owid-data.r") # fetch and clean our world in data covid cases
  source("code/data-wrangle.r") # merge submission data with epidemiological data
  source("code/submission-plots.r") # plots for submissions
  source("code/summary-stats.r") # summary statistics for submissions
  source("code/regression.r") # perform regression test for each database
  source("code/usage-network.r") # create and visualize adjacency matrices
}
