

## OLD
### GINI

# # PALMA RATIO
# gg = plot_palma_ratio(gisaid)
# nn = plot_palma_ratio(ncbi)
# ee = plot_palma_ratio(ena)
# dd = plot_palma_ratio(ddbj)
# ggarrange(gg,nn,ee,dd,common.legend = TRUE)
# ggsave(
#   paste0("imgs/palma-ratio-plot.png"),
#   dpi = 320,
#   width = 16,
#   height = 18,
#   limitsize = FALSE
# )

# ## REPORTS
# generate_report(gisaid)
# generate_report(ncbi)
# generate_report(ena)
# generate_report(ddbj)
### MAP
# plot_collaboration_map(ena)http://127.0.0.1:20839/graphics/plot_zoom_png?width=1129&height=900
# plot_collaboration_map(gisaid)
# plot_collaboration_map(ena)
# plot_collaboration_map(ena)
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
# e = plot_gini_collab(ddbj)
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
