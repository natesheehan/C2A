## ---------------------------
##
## Script name: usage-network.r
##
## Purpose of script: build network for each group per database
##
## Author: Nathanael Sheehan
##
## Date Created: 2023-04-28
##
##
## ---------------------------
##
## Notes:
# Data source: Dimensions API
# Data collection: 15/02/2022
# Data queries:
# # (1) gisaid AND "covid" or "covid-19" or "sars-cov-2" "coronavirus"
# # (2) european nucleotide archive or ena AND "covid" or "covid-19" or "sars-cov-2" "coronavirus"
## ---------------------------


# Build Script ------------------------------------------------------------
# 1. Convert Dimensions biblometric CSV 2 Dataframe -----------------------
if (file.exists("data/CV19DP/ena.rds")) {
  gisaid = readRDS("data/GISAID/gisaid-lit.rds")
  ena = readRDS("data/CV19DP/ena.rds")
} else {
  gisaid = convert_biblo("data-raw/publications/Dimensions-Publication-2023-02-16_00-40-25.csv")
  ena = convert_biblo("data-raw/publications/Dimensions-Publication-2023-02-16_00-40-09.csv")
}

# 2. Filter Data ----------------------------------------------------------
gisaid_clean = tf_data(gisaid)
ena_clean = tf_data(ena)

YT = as.data.frame(gisaid_clean$UT)
colnames(YT) = "UT"
UT = as.data.frame(ena_clean$UT)
colnames(UT) = "UT"
shared = inner_join(YT,UT)
# 3. Calculate Biblio Analysis --------------------------------------------
# # 3.1 Generate list of results

gisaid_res = biblio_analysis(gisaid_clean)
ena_res = biblio_analysis(ena_clean)

p=plot_ss_network(gisaid_clean,ena_clean)

# 4. Build Networks
calculate_networks(gisaid_clean, "data/publications/GISAID/")
calculate_networks(ena_clean, "data/publications/CV19DP/")

# 5. Build Network
a = plot_network(network = readRDS("data/GISAID/funding_country_net.rds"), type = "funding_country", di = "GISAID") # 303.4
b = plot_network(network = readRDS("data/GISAID/country_net.rds"), type = "country", di = "GISAID") # 157.5
c = plot_network(network = readRDS("data/GISAID/institution_net.rds"), type = "institution", di = "GISAID") #7
d = plot_network(network = readRDS("data/GISAID/city_net.rds"), type = "city", di = "GISAID") #22
e = plot_network(network = readRDS("data/CV19DP/funding_country_net.rds"), type = "funding_country", di = "CV19DP") #153
f = plot_network(network = readRDS("data/CV19DP/country_net.rds"), "country", "CV19DP") # 118.92
g = plot_network(network = readRDS("data/CV19DP/institution_net.rds"), type = "institution", di = "CV19DP") #2
h = plot_network(network = readRDS("data/CV19DP/city_net.rds"), type = "city", di = "CV19DP") # 12

ggarrange(a,e,b,f,nrow = 4,ncol = 2,common.legend = TRUE, legend="bottom",
          ggarrange(c,g,d,h,nrow = 4,ncol = 2,common.legend = TRUE, legend="bottom"))

ggsave(
  paste0("plots/network-plot.png"),
  dpi = 320,
  width = 32,
  height = 28,
  limitsize = FALSE
)
