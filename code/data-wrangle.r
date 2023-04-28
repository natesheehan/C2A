## ---------------------------
##
## Script name: data-wrangle.r
##
## Purpose of script: Download and transform metadata from GISAID, Covid-19 Data Portal and NCBI
##
## Author: Nathanael Sheehan
##
## Date Created: 2022-03-21
##
## Copyleft (c) Nathanael Sheehan, 2022
##
## ---------------------------
##
## Notes:
##
##
## ---------------------------


#################################################################
##             Covid-19 Data Platform Monthly Data             ##
#################################################################

CV19DP = readr::read_tsv("data-raw/submissions/cv19dp_variants_statistics.tsv")
CV19DP = CV19DP |>
  select(collection_date, country) |>
  dplyr::mutate(collection_date = paste0(
    substr(collection_date, 1, 4),
    "-",
    substr(collection_date, 5, 6),
    "-",
    substr(collection_date, 7, 8)
  )) |>
  dplyr::mutate(collection_date = lubridate::ymd(collection_date)) |>
  dplyr::filter(collection_date > '2020-01-01') |>
  dplyr::mutate(wy = isodate(collection_date)) |>
  dplyr::filter(country != 0) |>
  complete(country,wy) |>
  group_by(wy, country) |>
  dplyr::select(-c(collection_date)) |>
  dplyr::summarise(Count = n()) |>
  dplyr::mutate(
    country = case_when(
      country == "West Bank" ~ "Palestine",
      country == "Viet Nam" ~ "Vietnam",
      TRUE ~ country
    )
  ) |>
  dplyr::rename(C19DP.weekly.submissions = Count)

CV19DP$CD19DP.total.submissions = ave(CV19DP$C19DP.weekly.submissions,
                                      CV19DP$country,
                                    FUN = cumsum)


write_rds(CV19DP, "data/CV19DP/CV19DP.RDS")



#######################################################################################################
##                                    GISAID Global Monthly Data                                     ##
##  GISAID monthly metadata submissions downloaded from https://www.epicov.org/epi3/frontend#5dc229  ##
#######################################################################################################
gisaid = as.data.frame(
  readr::read_tsv("data-raw/submissions/gisaid_variants_statistics.tsv")  |>
    dplyr::rename(Date = `Week prior to`) |>
    dplyr::rename(country = Country)  |>
    dplyr::mutate(Date = lubridate::ymd(Date)) |>
    dplyr::mutate(wy = isodate(Date)) |>
    dplyr::filter(country != 0) |>
    complete(country,wy) |>
    dplyr::select(c(
      country, wy, `Submission Count`
    )) |>
    dplyr::group_by(country,wy) |>
    dplyr::summarise_if(is.numeric,sum) |>
    dplyr::rename(GISAID.weekly.submissions = `Submission Count`)) |>
  {
    \(.) {
      replace(., is.na(.), 0)
    }
  }()

gisaid$GISAID.total.submissions = ave(gisaid$GISAID.weekly.submissions,
                                      gisaid$country,
                                      FUN = cumsum)

write_rds(gisaid, "data/GISAID/gisaid.RDS")


#################################################################
##                          Join data                          ##
#################################################################

main_df = full_join(gisaid, CV19DP, by = c("country", "wy"))
main_df = main_df |> left_join(owid, by =
                                 c("country", "wy")) |>
  {
    \(.) {
      replace(., is.na(.), 0)
    }
  }()

main_df$gpnc_gisaid = main_df$GISAID.weekly.submissions / main_df$new_cases * 100
main_df$gpnc_embl= main_df$C19DP.weekly.submissions / main_df$new_cases * 100


cc = main_df |> dplyr::select(country,continent) |> unique() |> dplyr::filter(continent != 0)

for (i in 1:nrow(cc)) {
  main_df = main_df |>
    dplyr::mutate(continent = ifelse(country == cc$country[i] & continent == 0, cc$continent[i], continent))
}

dum = main_df |> dplyr::filter(continent == 0)
unique(dum$country)
# [1] "Bonaire"                   "Cabo Verde"                "Canary Islands"            "Crimea"
# [5] "Czech Republic"            "Faroe Islands"             "Micronesia"                "Saint Martin"
# [9] "Sint Eustatius"            "The Bahamas"               "Timor-Leste"               "U.S. Virgin Islands"
# [13] "Wallis and Futuna Islands                    "U.S. Virgin Islands"              "USA"                              "Wallis and Futuna Islands"

main_df = main_df |>
  dplyr::mutate(continent = ifelse(country == "American Samoa" & continent == 0, "Oceania", continent)) |>
  dplyr::mutate(continent = ifelse(country == "Bonaire" & continent == 0, "South America", continent)) |>
  dplyr::mutate(continent = ifelse(country == "Cabo Verde" & continent == 0, "Africa", continent)) |>
  dplyr::mutate(continent = ifelse(country == "Canary Islands" & continent == 0, "Europe", continent)) |>
  dplyr::mutate(continent = ifelse(country == "Crimea" & continent == 0, "Europe", continent)) |>
  dplyr::mutate(continent = ifelse(country == "Czech Republic" & continent == 0, "Europe", continent)) |>
  dplyr::mutate(continent = ifelse(country == "Faroe Islands" & continent == 0, "Europe", continent)) |>
  dplyr::mutate(continent = ifelse(country == "French Guiana" & continent == 0, "South America", continent)) |>
  dplyr::mutate(continent = ifelse(country == "Guadeloupe" & continent == 0, "North America", continent)) |>
  dplyr::mutate(continent = ifelse(country == "Martinique" & continent == 0, "North America", continent)) |>
  dplyr::mutate(continent = ifelse(country == "Mayotte" & continent == 0, "Africa", continent)) |>
  dplyr::mutate(continent = ifelse(country == "Micronesia" & continent == 0, "Oceania", continent)) |>
  dplyr::mutate(continent = ifelse(country == "Reunion" & continent == 0, "Africa", continent)) |>
  dplyr::mutate(continent = ifelse(country == "Saint Barthelemy" & continent == 0, "North America", continent)) |>
  dplyr::mutate(continent = ifelse(country == "Saint Martin" & continent == 0, "North America", continent)) |>
  dplyr::mutate(continent = ifelse(country == "Sint Eustatius" & continent == 0, "North America", continent)) |>
  dplyr::mutate(continent = ifelse(country == "The Bahamas" & continent == 0, "North America", continent)) |>
  dplyr::mutate(continent = ifelse(country == "Timor-Leste" & continent == 0, "Asia", continent)) |>
  dplyr::mutate(continent = ifelse(country == "U.S. Virgin Islands" & continent == 0, "North America", continent)) |>
  dplyr::mutate(continent = ifelse(country == "USA" & continent == 0, "North America", continent)) |>
  dplyr::mutate(continent = ifelse(country == "Wallis and Futuna Islands" & continent == 0, "Oceania", continent))

rm(gisaid,CV19DP,owid)

owid_income_groups = read.csv("data-raw/owid/world-banks-income-groups.csv") %>%
  dplyr::filter(Year == 2021) %>%
  dplyr::select(-c(Year)) %>%
  dplyr::rename("country" = "Entity") %>%
  dplyr::rename("iso_code" = "Code")

main_df = main_df %>% dplyr::filter(wy >= "20/01") %>%
  dplyr::filter(wy <= "23/01") %>%
  dplyr::right_join(owid_income_groups)


rm(owid_income_groups,cc,dum)

write_rds(main_df,"data/main_df.rds")
