## ---------------------------
##
## Script name: build.r
##
## Purpose of script: build project entirely or partially
##
## Author: Nathanael Sheehan
##
## Date Created: 2023-04-28
##
##
## ---------------------------
##
## Notes: In order to run reproducible first you must unzip the /data-raw/submissions folder
##
##
## ---------------------------

# packages
package_list = c(
  "ggplot2", # use for paths creation
  "dplyr",
  "janitor", # useful functions for cleaning imported data
  "tidyr", # creating edges
  "igraph", # for creating networks
  "ggraph", # plotting networks
  "stringr",
  "tm",
  "lubridate",
  "countrycode",
  "stringr",
  "dplyr",
  "ggplot2",
  "hrbrthemes",
  "purrr",
  "cowplot",
  "ggrepel",
  "ggpubr",
  "maps"

)

# INSTALL packages
for (p in package_list) {
  if (p %in% installed.packages() == FALSE) {
    install.packages(p, dependencies = TRUE)
  }
  library(p, character.only = TRUE)
}

reproducible = FALSE

if(reproducible == TRUE) {
  source("code/utils.r")
  source("code/data.r")
  source("code/workflow.r")
}
