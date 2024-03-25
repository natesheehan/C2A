#### Functions
convert_biblo = function(file) {
  data = bibliometrix::convert2df(file, dbsource = "dimensions", format = "csv")
}

#### Run Bibliometric Analysis for GISAID and the INSDB


# Read data
gisaid = convert_biblo("../../../Downloads/Dimensions-Pub-GISIAID/Dimensions-Publication-2023-08-23_13-20-00.csv")
ena = convert_biblo("../../../Downloads/Dimensions-Pub-ena/Dimensions-Publication-2023-08-23_13-20-19.csv")
ddjb = convert_biblo("../../../Downloads/Dimensions-Pub-ddjb/Dimensions-Publication-2023-08-23_13-20-29.csv")
ncbi = convert_biblo("../../../Downloads/Dimensions-Pub-ncbi/Dimensions-Publication-2023-08-23_13-20-37.csv")

# Save data so things can run quicker
saveRDS(gisaid,"data/biblio/gisaid.rds")
saveRDS(ena,"data/biblio/ena.rds")
saveRDS(ddjb,"data/biblio/ddjb.rda")
saveRDS(ncbi,"data/biblio/ncbi.rda")

ddjb = readRDS("data/biblio/ddjb.rda")
ena = readRDS("data/biblio/ena.rds")
gisaid = readRDS("data/biblio/gisaid.rds")
ncbi = readRDS("data/biblio/ncbi.rda")

gisaid = gisaid |> dplyr::rename(Publication.Date = Publication.date) |>
  dplyr::rename(Publication.Date..online. = Publication.date..online.) |>
  dplyr::rename(Publication.Date..print. = Publication.date..print.) |>
  dplyr::rename(City.of.Research.organization = City.of.standardized.research.organization) |>
  dplyr::rename(State.of.Research.organization = State.of.standardized.research.organization) |>
  dplyr::rename(Country.of.Research.organization = Country.of.standardized.research.organization) |>
  dplyr::select(-c(Book.editors))

# Simularity

gisaid_clean = tf_data(gisaid)
ena_clean = tf_data(ena)
ddjb_clean = tf_data(ddjb)
ncbi_clean = tf_data(ncbi)

#GISAID
YT = as.data.frame(gisaid_clean$UT)
colnames(YT) = "UT"
# ENA
UT = as.data.frame(ena_clean$UT)
colnames(UT) = "UT"
# DDJB
GT = as.data.frame(ddjb_clean$UT)
colnames(GT) = "UT"
# NCBI
NT = as.data.frame(ncbi_clean$UT)
colnames(UT) = "UT"


shared = inner_join(YT,UT) |>inner_join(GT) |> inner_join(NT)
nrow(shared)


## Biblioanalysis

gisaid_res = bibliometrix::biblioAnalysis(gisaid)
ena_res = biblio_analysis(ena_clean)
ncbi_res = biblio_analysis((ncbi_clean))
ddjb_res = biblio_analysis((ddjb_clean))
