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
