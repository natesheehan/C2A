# packages
package_list = c(
  "ggplot2", # use for paths creation
  "dplyr",
  "janitor", # useful functions for cleaning imported data
  "tidyr", # creating edges
  "igraph", # for creating networks
  "ggraph", # plotting networks
  "stringr",
  "tm"
)
for (p in package_list) {
  if (p %in% installed.packages() == FALSE) {
    install.packages(p, dependencies = TRUE)
  }
  library(p, character.only = TRUE)
}
round_df = function(df, digits) {
  nums = vapply(df, is.numeric, FUN.VALUE = logical(1))

  df[, nums] = round(df[, nums], digits = digits)

  (df)
}
indicies = function(g) {
  res = matrix(0, igraph::vcount(g), 5)
  res[, 1] = V(g)$name
  res[, 2] = as.numeric(igraph::degree(g))
  res[, 3] = as.numeric(igraph::betweenness(g))
  res[, 4] = as.numeric(igraph::closeness(g))
  res[, 5] = as.numeric(igraph::eigen_centrality(g)$vector)
  res = as.data.frame(res)
  res[, c(2:5)] = sapply(res[, c(2:5)], as.numeric)
  res = round_df(res, 3)
  colnames(res) = c("name",
                    "degree",
                    "betweenness",
                    "closeness",
                    "eigen_centrality")
  return(res)
}
library(lubridate)
library(countrycode)
library(dplyr)
library(stringr)
library(ggplot2)
library(hrbrthemes)
library(countrycode)
library(purrr)
library(cowplot)
library(ggrepel)
library(ggpubr)
# data
ddjb = read.csv("../BIB DATA/DDJB/CSV/Dimensions-Publication-2023-08-23_13-20-29.csv",
                skip = 1)#
gisaid = read.csv("../BIB DATA/GISAID/CSV/Dimensions-Publication-2023-08-23_13-20-00.csv",
                skip = 1)
ncbi = read.csv("../BIB DATA/NCBI/CSV/Dimensions-Publication-2023-08-23_13-20-37.csv",
                  skip = 1)
ena = read.csv("../BIB DATA/ENA/CSV/Dimensions-Publication-2023-08-23_13-20-19.csv",
               skip = 1)


ddjb.refs = read.csv("../BIB DATA/ddjb/bib CSV/Dimensions-Publication-2023-08-24_13-58-27.csv",
                     skip = 1)

class = read.csv("../CLASS.csv")

## REPORTS
generate_report(gisaid)
generate_report(ncbi)
generate_report(ena)
generate_report(ddjb)

## PUBLICATIONS
p1= plot_total_publications(gisaid,ena,ddjb,ncbi)
p2 = plot_average_altmetric(gisaid,ena,ddjb,ncbi)
p3 = plot_average_citation(gisaid,ena,ddjb,ncbi)
p4 = plot_total_citations(gisaid,ena,ddjb,ncbi)
ggarrange(p1,p3,p2,p4)

ggsave(
  paste0("imgs/publikcations-plot.png"),
  dpi = 320,
  width = 16,
  height = 18,
  limitsize = FALSE
)

## ACCESS
plot_access("GISAID","ENA","DDJB","NCBI")
ggsave(
  paste0("imgs/access-plot.png"),
  dpi = 320,
  width = 16,
  height = 18,
  limitsize = FALSE
)

## PUBLISHERS
plot_publisher("GISAID", "ENA", "DDJB", "NCBI")
ggsave(
  paste0("imgs/publishers-plot.png"),
  dpi = 320,
  width = 16,
  height = 18,
  limitsize = FALSE
)

## KEYWORDS AND THEMATIC MAPPING
a = plot_keyword_graph(gisaid)
b = plot_keyword_graph(ncbi)
c = plot_keyword_graph(ena)
d = plot_keyword_graph(ddjb)
ggarrange(a,b,c,d)
ggsave(
  paste0("imgs/keywords-plot.png"),
  dpi = 320,
  width = 16,
  height = 18,
  limitsize = FALSE
)

# VARIANTS
j9 = plot_variants(ena)
j10 = plot_variants(gisaid)
j11 = plot_variants(ncbi)
j12 = plot_variants(ddjb)

ggarrange(j10,j11,j9,j12)
ggsave(
  paste0("imgs/variants-plot.png"),
  dpi = 320,
  width = 16,
  height = 18,
  limitsize = FALSE
)


### SCP vs MCP
j = plot_scp_mcp(gisaid)
k = plot_scp_mcp(ncbi)
l = plot_scp_mcp(ddjb)
m = plot_scp_mcp(ena)
ggarrange(j,k,m,l,common.legend = TRUE)
ggsave(
  paste0("imgs/region-colab-plot.png"),
  dpi = 320,
  width = 16,
  height = 18,
  limitsize = FALSE
)

# INCOME COLAB
j1 = plot_income_colab(gisaid)
k1 = plot_income_colab(ncbi)
l1 = plot_income_colab(ddjb)
m1 = plot_income_colab(ena)
ggarrange(j1,k1,m1,l1,common.legend = TRUE)
ggsave(
  paste0("imgs/income-colab-plot.png"),
  dpi = 320,
  width = 16,
  height = 18,
  limitsize = FALSE
)

# PALMA RATIO
gg = plot_palma_ratio(gisaid)
nn = plot_palma_ratio(ncbi)
ee = plot_palma_ratio(ena)
dd = plot_palma_ratio(ddjb)
ggarrange(gg,nn,ee,dd,common.legend = TRUE)
ggsave(
  paste0("imgs/palma-ratio-plot.png"),
  dpi = 320,
  width = 16,
  height = 18,
  limitsize = FALSE
)


### MAP
plot_collaboration_map(ena)
plot_collaboration_map(gisaid)
plot_collaboration_map(ena)
plot_collaboration_map(ena)

plot_collab_network(ena,"Research.Organizations...standardized")


# NETWORKS
data_frames = list(gisaid = gisaid, ncbi = ncbi, ena = ena, ddjb = ddjb)

# Loop through each data frame and generate/save plots
for (data_name in names(data_frames)) {
  generate_and_save_plots(data_frames[[data_name]], data_name)
}




## OLD
### GINI
# plot_gini_collab = function(data) {
#   # Extract countries from the affiliations
#   data_name = deparse(substitute(data))
#   # 1. Break down the country affiliations
#   country_collab = data |>
#     select(Country.of.standardized.research.organization) |>
#     unnest(country = strsplit(as.character(Country.of.standardized.research.organization), ';')) |>
#     dplyr::mutate(country = stringr::str_trim(country)) |>  # Remove whitespaces from both sides
#     dplyr::mutate(country = stringr::str_squish(country)) |>  # Ensure that any multi-space sequences within the name are turned into a single space
#     group_by(country) |>
#     tally() |>
#     arrange(-n)  # Optional: Order by descending counts
#
#   # 2. Compute the number of collaborators per country
#   total_collaborators = sum(country_collab$n)
#
#   # 3. Calculate each country's ratio of collaborators
#   country_collab = country_collab |>
#     mutate(ratio = n / total_collaborators) |>
#     arrange(-n)
#
#   country_collab$Code = countrycode(country_collab$country, "country.name", "iso3c")
#
#   country_collab = inner_join(country_collab,class)
#
#   # 4. Plot
#   # Assuming you've merged and have the 'Income.Group' column in the country_collab dataframe
#   country_collab$Income.group = factor(country_collab$Income.group,
#                                        levels = c("Low income", "Lower middle income", "Upper middle income", "High income"))
#
#   country_collab = country_collab[!is.na(country_collab$Income.group) & country_collab$Income.group != "", ]
#
#   ggplot(country_collab, aes(x=n, y=ratio, label=country)) +
#
#     # Adjusting the geom_point aesthetics
#     geom_point(aes(color=Income.group, size=Income.group), alpha=0.7) +
#
#     # Label for top 5 countries
#     geom_text(data=head(country_collab, 5), aes(label=country), vjust=-1, hjust=1, color="white") +
#
#     # Add some jitter to separate overlapping points
#     geom_jitter() +
#
#     # Titles and labels
#     labs(title = paste(stringr::str_to_upper(data_name)),
#          x = "Number of Collaborators (Log Scale)",
#          y = "Ratio of Total Collaborators") +
#
#     # Log scale for x-axis
#     scale_x_log10() +
#
#     # Minimal theme from hrbrthemes
#     custom_theme_oss() +
#
#     # Position the legend at the bottom
#     theme(legend.position="bottom")
# }
# e = plot_gini_collab(ddjb)
# f = plot_gini_collab(ena)
# h = plot_palma_collab(gisaid)
# i = plot_gini_collab(ncbi)
# ggarrange(e,f,h,i, common.legend = TRUE)
# ggsave(
#   paste0("imgs/gini-colab-plot.png"),
#   dpi = 320,
#   width = 16,
#   height = 18,
#   limitsize = FALSE
# )
