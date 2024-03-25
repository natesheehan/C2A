## ---------------------------
##
## Script name: regression.r
##
## Purpose of script: perform a regression analysis for both databases
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
# Regression between two databases ----------------------------------------
gis_main_df_europe = main_df |>
  filter(wy < "22/38") |>
  group_by(wy,continent) |>
  summarise(sum_gisaid = sum(GISAID.weekly.submissions)) |>
  mutate(percentage_gis = sum_gisaid / sum(sum_gisaid))

c19_main_df_europe = main_df |>
  filter(wy < "22/38") |>
  group_by(wy,continent) |>
  summarise(sum_cd19dp = sum(C19DP.weekly.submissions)) |>
  mutate(percentage_c19 = sum_cd19dp / sum(sum_cd19dp))

sum_gis_c19 = right_join(gis_main_df_europe,c19_main_df_europe)
sum_gis_c19$t = as.numeric(stringr::str_remove(sum_gis_c19$wy, "/"))
sum_gis_c19$f = factor(sum_gis_c19$continent,      # Reordering group factor levels
                       levels = paste(unique(sum_gis_c19$continent)))

regions = as.data.frame(unique(sum_gis_c19$continent))
colnames(regions) = "region"

#
p0 = ggscatter(
  sum_gis_c19 %>% mutate(
    sum_gisaid = log(sum_gisaid),
    sum_c19dp = log(sum_cd19dp)
  ),
  x = "sum_gisaid",
  y = "sum_c19dp",
  add = "reg.line",
  color = "black",
  cor.method = "kendall",
  xlab = "GISAID total submissions",
  ylab = "CV19DP total submissions",
  title = "Global Kendall Correlation"
)  + theme_pubclean()   + stat_cor(colour = "black",label.x = 1, p.accuracy = 0.001, r.accuracy = 0.01) + theme(title = element_text(size=18))

p1 = ggscatter(
  sum_gis_c19 %>% mutate(
    sum_gisaid = log(sum_gisaid),
    sum_c19dp = log(sum_cd19dp)
  ),
  facet.by = "continent",
  x = "sum_gisaid",
  y = "sum_c19dp",
  add = "reg.line",
  color = "black",
  font.label = c(14,"bold"),
  cor.method = "kendall",
  xlab = "GISAID total submissions",
  ylab = "ENA total submissions",
  title = "Regional Kendall Correlation"
) + theme_pubclean()  + stat_cor(colour = "black",label.x = 1, p.accuracy = 0.001, r.accuracy = 0.01) + theme(title = element_text(size=18),strip.text = element_text(size = 9))

ggarrange(p0,p1,widths = c(0.5,1))

ggsave(
  paste0("plots/submission-correlation.png"),
  dpi = 320,
  width = 16,
  height = 16,
  limitsize = FALSE)
