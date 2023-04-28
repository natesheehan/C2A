## ---------------------------
##
## Script name: submission-plots.r
##
## Purpose of script: plot submission data for each database
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

# Treemap -----------------------------------------------------------------

tree_df = main_df |> dplyr::filter(wy == "23/01")

percents_gis = tree_df |> select(GISAID.total.submissions,country) |>
  mutate(percent = GISAID.total.submissions/sum(tree_df$GISAID.total.submissions)*100) |> round_df(3) |>
  dplyr::arrange(desc(percent)) |> head(20)

percents_ena = tree_df |> select(CD19DP.total.submissions,country) |>
  mutate(percent = CD19DP.total.submissions/sum(tree_df$CD19DP.total.submissions)*100) |> round_df(3) |>
  dplyr::arrange(desc(percent)) |> head(20)


a =ggplot(
  tree_df |>
    mutate(percent = GISAID.total.submissions/sum(tree_df$GISAID.total.submissions)*100) |> round_df(3) |>
    dplyr::arrange(desc(percent)),
  aes(
    area = percent,
    fill = percent,
    label = country,
    subgroup = continent
  )
) +
  geom_treemap() +
  geom_treemap_subgroup_border(colour = "white", size = 5) +
  geom_treemap_subgroup_text(
    place = "bottomleft",
    grow = TRUE,
    alpha = 0.25,
    colour = "black",
    fontface = "italic"
  ) +
  geom_treemap_text(
    colour = "white",
    place = "centre",
    size = 15,
    grow = TRUE
  ) +   labs(title = "GISAID") + theme_pubclean()  +
  scale_fill_viridis_c(option = "E")  +
  theme(legend.key.size = unit(1.2, 'cm'),legend.position = "bottom",legend.title = element_text(face = "bold")) + theme(plot.title = element_text(size=22))
b=ggplot(
  tree_df  |>
    mutate(percent = CD19DP.total.submissions/sum(tree_df$CD19DP.total.submissions)*100) |> round_df(3) |>
    dplyr::arrange(desc(percent)),
  aes(
    area = percent,
    fill = percent ,
    label = country,
    subgroup = continent
  )
) +
  geom_treemap() +
  geom_treemap_subgroup_border(colour = "white", size = 5) +
  geom_treemap_subgroup_text(
    place = "bottomleft",
    grow = TRUE,
    alpha = 0.25,
    colour = "black",
    fontface = "italic"
  ) +
  geom_treemap_text(
    colour = "white",
    place = "centre",
    size = 15,
    grow = TRUE
  ) +  labs(title = "CV19DP") +
  theme_pubclean() + scale_fill_viridis_c(option = "G") +
  theme(legend.key.size = unit(1.2, 'cm'),legend.position = "bottom", legend.title = element_text(face = "bold"))+ theme(plot.title = element_text(size=22))
ggarrange(a,b)

ggsave(
  paste0(
    "plots/tree-Landscape.png"),
  dpi = 320,
  width = 8,
  height = 6,
  limitsize = FALSE
)

# Temporal Submissions (Regions) ------------------------------------------

# Sort data into weekly sums
gis_main_df = main_df |>
  filter(wy < "23/01") |>
  group_by(wy,continent) |>
  summarise(sum_gisaid = sum(GISAID.weekly.submissions)) |>
  mutate(percentage_gis = sum_gisaid / sum(sum_gisaid))

c19_main_df = main_df |>
  filter(wy < "23/01") |>
  group_by(wy,continent) |>
  summarise(sum_cd19dp = sum(C19DP.weekly.submissions)) |>
  mutate(percentage_c19 = sum_cd19dp / sum(sum_cd19dp))

sum_gis_c19 = right_join(gis_main_df,c19_main_df)
sum_gis_c19$t = as.numeric(stringr::str_remove(sum_gis_c19$wy, "/"))
sum_gis_c19$f = factor(sum_gis_c19$continent,      # Reordering group factor levels
                       levels = paste(unique(sum_gis_c19$continent)))

# GISAID
b = ggplot(sum_gis_c19, aes(x=t, y=(log10(sum_gisaid)), fill=f)) +
  geom_smooth() + theme_pubclean() + coord_trans() + labs(title = "B)")

# The covid-19 data portal
e = ggplot(sum_gis_c19, aes(x=t, y=(sum_cd19dp), fill=f)) +
  geom_smooth() + theme_landscape() + coord_trans() + labs(title = "E)")

# Plot landscape ----------------------------------------------------------

# GISAID
plot_df = main_df  |>
  filter(wy < "23/01") |>
  filter(wy >= "20/01") |>
  filter(country != is.na(country)) |>
  mutate(iso_code = ifelse(iso_code == "OWID_KOS", "KOS", iso_code)) |>
  filter(continent != is.na(continent)) |>
  # convert state to factor and reverse order of levels
  mutate(country = factor(country, levels = rev(sort(unique(
    country
  ))))) |>
  # dplyr::filter(`Genomes per confirmed cases % (GISAID)` != Inf) |>
  # dplyr::filter(`Genomes per confirmed cases % (GISAID)` < 100) |>
  dplyr::mutate(gpnc_gisaid = ifelse(gpnc_gisaid == Inf, 99998, gpnc_gisaid)) |> # missing case data
  dplyr::mutate(gpnc_gisaid = ifelse(is.na(gpnc_gisaid), 999998, gpnc_gisaid))  |> # missing seqeunce data
  dplyr::mutate(gpnc_gisaid = ifelse(gpnc_gisaid == 0, 9999998, gpnc_gisaid)) |> # missing seqeunce data
  # create a new variable from count
  mutate(countfactor = cut(
    gpnc_gisaid,
    breaks = c(0,
               1,
               2,
               3,
               4,
               5,
               20550,
               99999,
               999999,
               max(gpnc_gisaid, na.rm = F)),
    labels = c(
      "0-1%",
      "1-2%",
      "2-3%",
      "3-4%",
      "4-5%",
      ">5%",
      "Missing Case Data",
      "Missing Case and Sequence Data",
      "Missing Sequence Data"
    )
  )) |>
  # change level order
  mutate(countfactor = factor(as.character(countfactor), levels = rev(levels(countfactor)))) |>
  dplyr::mutate(wy = as.Date(paste0("20",wy,"/1"),format = "%Y/%U/%u"))

plot_df$f = factor(plot_df$continent,      # Reordering group factor levels
                   levels = paste(unique(plot_df$continent)))
c = ggplot(plot_df,
           aes(x = wy, y = iso_code, fill = countfactor)) +
  #add border white colour of line thickness 0.25
  geom_bin_2d(colour = "white", size = 0.2) +
  guides(fill = guide_legend(title = "Percent of sequenced cases \nper epidemiological week")) +
  labs(title = "GISAID",
       x = "Epidemiological Week",
       y = "Country"
  ) +
  scale_y_discrete(expand = c(0, 0)) +
  theme_grey(base_size = 10) +
  scale_fill_manual(
    values = c(
      "gray12",
              "gray47",
              "gray96",
              "#845ec2",
              "#d65db1" ,
              "#ff6f91",
              "#ff9671",
              "#ffc75f",
              "#f9f871",
              "#ddf1da"
    ),
    na.value = "grey90"
  ) + theme_pubclean() +   theme(
    panel.spacing = unit(0, units = "cm"),
    # removes space between panels
    strip.text.y = element_text(angle = 270, face = "bold",size = 14),
    strip.placement = "outside", legend.position = "bottom", legend.title = element_text(face = "bold",size = 18)
  ) + theme_landscape() + theme_pubclean() +   theme(
    panel.spacing = unit(0, units = "cm"),
    # removes space between panels
    strip.text.y = element_text(angle = 270, face = "bold",size = 14),
    strip.placement = "outside",legend.position = "bottom", legend.title = element_text(face = "bold",size = 18)
  ) + theme(axis.text.x=element_text(angle=90, hjust=1,size =16)) +
  theme(plot.title = element_text(size=22)) +
  theme(legend.key.size = unit(1.5, 'cm')) +
  theme(legend.text = element_text(size=18)) +
  theme(axis.line.y = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme(plot.title = element_text(size=22)) +
  theme(axis.text.x=element_text(angle=45, hjust=1,size =16))

# The covid-19 data portal
plot_df = main_df  |>
  filter(wy < "23/01") |>
  filter(wy >= "20/01") |>
  mutate(iso_code = ifelse(iso_code == "OWID_KOS", "KOS", iso_code)) |>
  # convert state to factor and reverse order of levels
  mutate(country = factor(country, levels = rev(sort(unique(
    country
  ))))) |>
  # dplyr::filter(`Genomes per confirmed cases % (GISAID)` != Inf) |>
  # dplyr::filter(`Genomes per confirmed cases % (GISAID)` < 100) |>
  dplyr::mutate(gpnc_embl = ifelse(gpnc_embl == Inf, 99998, gpnc_embl)) |> # missing case data
  dplyr::mutate(gpnc_embl = ifelse(is.na(gpnc_embl), 999998, gpnc_embl))  |> # missing seqeunce data
  dplyr::mutate(gpnc_embl = ifelse(gpnc_embl == 0, 9999998, gpnc_embl)) |> # missing seqeunce data
  # create a new variable from count
  mutate(countfactor = cut(
    gpnc_embl,
    breaks = c(0,
               1,
               2,
               3,
               4,
               5,
               20550,
               99999,
               999999,
               max(gpnc_embl, na.rm = F)),
    labels = c(
      "0-1%",
      "1-2%",
      "2-3%",
      "3-4%",
      "4-5%",
      ">5%",
      "Missing Case Data",
      "Missing Case and Sequence Data",
      "Missing Sequence Data"
    )
  )) |>
  # change level order
  mutate(countfactor = factor(as.character(countfactor), levels = rev(levels(countfactor)))) |>
  dplyr::mutate(wy = as.Date(paste0("20",wy,"/1"),format = "%Y/%U/%u"))

plot_df$f = factor(plot_df$continent,      # Reordering group factor levels
                   levels = paste(unique(plot_df$continent)))

f = ggplot(plot_df , aes(x = wy, y = iso_code, fill = countfactor)) +
  #add border white colour of line thickness 0.25
  geom_bin_2d(colour = "white", size = 0.2) +
  theme(
    panel.spacing = unit(0, units = "cm"),
    # removes space between panels
    strip.background = element_blank(),
    strip.placement = "outside"
  ) +
  facet_grid(rows = vars(f), scales = "free_y", switch = "y", space = "free_y") +
  guides(fill = guide_legend(title = "Percent of sequenced cases \nper epidemiological week")) +
  labs(title = "CV19DP",
       x = "Epidemiological Week",
       y = "Country" ) +
  scale_y_discrete(expand = c(0, 0)) +
  theme_grey(base_size = 10) +
  scale_fill_manual(
    values = c(
      "gray12",
              "gray47",
              "gray96",
              "#845ec2",
              "#d65db1" ,
              "#ff6f91",
              "#ff9671",
              "#ffc75f",
              "#f9f871",
              "#ddf1da"
    ),
    na.value = "grey90"
  ) + theme_landscape() + theme_pubclean() +   theme(
    panel.spacing = unit(0, units = "cm"),
    # removes space between panels
    strip.text.y = element_text(angle = 270, face = "bold",size = 14),
    strip.placement = "outside",legend.position = "bottom", legend.title = element_text(face = "bold",size = 18)) +
  theme(axis.text.x=element_text(angle=45, hjust=1,size =16)) +
  theme(axis.text.y=element_text(angle=90, hjust=1,size =8)) +
  theme(plot.title = element_text(size=22)) +
  theme(legend.key.size = unit(1.5, 'cm')) +
  theme(legend.text = element_text(size=18))  +
  theme(plot.title = element_text(size=22)) +
  labs(x = "")

ggarrange(f,c,common.legend = TRUE, legend="bottom")

# Plot and Save -----------------------------------------------------------
ggsave(
  paste0("plots/",
         " Sequence-Landscape.png"),
  dpi = 320,
  width = 22,
  height = 32,
  limitsize = FALSE
)


rm(plot_df,c19_main_df,gis_main_df,sum_gis_c19,tree_df)
rm(a,b,c,ena,e,f,percents_ena,percents_gis)
