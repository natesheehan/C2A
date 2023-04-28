## ---------------------------
##
## Script name:
##
## Purpose of script:
##
## Author: Nathanael Sheehan
##
## Date Created: 2023-02-02
##
## Copyleft (c) Nathanael Sheehan, 2023
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

pkgs = c(
  "tidyverse",
  "bibliometrix",
  "tidyr",
  "stringr",
  "tidytext",
  "quanteda",
  "treemapify",
  "stm",
  "lubridate",
  "ggpubr",
  "expm",
  "igraph",
  "sf",
  "maps"
)

options(scipen = 999) # Turn off scientific notation
pacman(pkgs)
rm(pkgs)
