# Data source: Dimensions API
# Data collection: 15/02/2022
# Data queries:
# # (1) gisaid AND "covid" or "covid-19" or "sars-cov-2" "coronavirus"
# # (2) european nucleotide archive or ena AND "covid" or "covid-19" or "sars-cov-2" "coronavirus"
# # (3) national center for biotechnology information AND "covid" or "covid-19" or "sars-cov-2" "coronavirus"


# Functions ---------------------------------------------------------------

#source("code/utils.r")
source("build.r")
# Build Script ------------------------------------------------------------
# 1. Convert Dimensions biblometric CSV 2 Dataframe -----------------------
if (file.exists("publications/NCBI/nbi.rds")) {
  nbi = readRDS("publications/NCBI/nbi.rds")
  gisaid = readRDS("publications/GISAID/gisaid.rds")
  ena = readRDS("publications/CV19DP/ena.rds")
} else {
  nbi = convert_biblo("publications/Dimensions-Publication-2023-02-16_00-37-11.csv")
  gisaid = convert_biblo("publications/Dimensions-Publication-2023-02-16_00-40-25.csv")
  ena = convert_biblo("publications/Dimensions-Publication-2023-02-16_00-40-09.csv")
}

# 2. Filter Data ----------------------------------------------------------
nbi_clean = tf_data(nbi)
gisaid_clean = tf_data(gisaid)
ena_clean = tf_data(ena)

YT = as.data.frame(gisaid_clean$UT)
colnames(YT) = "UT"
UT = as.data.frame(ena_clean$UT)
colnames(UT) = "UT"
shared = inner_join(YT,UT)
# 3. Calculate Biblio Analysis --------------------------------------------
# # 3.1 Generate list of results

nbi_res = biblio_analysis(nbi_clean)
gisaid_res = biblio_analysis(gisaid_clean)
ena_res = biblio_analysis(ena_clean)

plot_ss_network(nbi_clean,gisaid_clean,ena_clean)

# 4. Build Networks
calculate_networks(nbi_clean, "publications/NCBI/")
calculate_networks(gisaid_clean, "publications/GISAID/")
calculate_networks(ena_clean, "publications/CV19DP/")


a = plot_network(network = readRDS("publications/GISAID/funding_country_net.rds"), type = "funding_country", di = "GISAID") # 303.4
b = plot_network(network = readRDS("publications/GISAID/country_net.rds"), type = "country", di = "GISAID") # 157.5
c = plot_network(network = readRDS("publications/GISAID/institution_net.rds"), type = "institution", di = "GISAID") #7
d = plot_network(network = readRDS("publications/GISAID/city_net.rds"), type = "city", di = "GISAID") #22


e = plot_network(network = readRDS("publications/CV19DP/funding_country_net.rds"), type = "funding_country", di = "CV19DP") #153
f = plot_network(network = readRDS("publications/CV19DP/country_net.rds"), "country", "CV19DP") # 118.92
g = plot_network(network = readRDS("publications/CV19DP/institution_net.rds"), type = "institution", di = "CV19DP") #2
h = plot_network(network = readRDS("publications/CV19DP/city_net.rds"), type = "city", di = "CV19DP") # 12

ggarrange(a,e,b,f,c,g,d,h,nrow = 4,ncol = 2)

ggsave(
  paste0("publications/network-plot.png"),
  dpi = 320,
  width = 22,
  height = 22,
  limitsize = FALSE
)
