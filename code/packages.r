## ---------------------------
##
## Script name: packages.r
##
## Purpose of script: load packages
##
## Author: Nathanael Sheehan
##
## Date Created: 2023-04-28
##
##
## ---------------------------
##
## Notes:
##
##
## ---------------------------
#################################################################
##                         Set up vars                         ##
#################################################################


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
  "maps",
  "data.table",
  "rnaturalearth",
  "rnaturalearthdata",
  "sf",
  "viridis"

)

# INSTALL packages
for (p in package_list) {
  if (p %in% installed.packages() == FALSE) {
    install.packages(p, dependencies = TRUE)
  }
  library(p, character.only = TRUE)
}

options(scipen = 999) # Turn off scientific notation
