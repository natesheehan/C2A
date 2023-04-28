## ---------------------------
##
## Script name: owid-data.r
##
## Purpose of script: Load and transform owid covid data from daily to weekly & dropping unneeded columns.
##
## Author: Nathanael Sheehan
##
## Date Created: 2022-10-23
##
## Copyleft (c) Nathanael Sheehan, 2022
##
## ---------------------------
##
## Notes: data accessed from https://ourworldindata.org/grapher/continents-according-to-our-world-in-data
##
##
## ---------------------------
url = "https://covid.ourworldindata.org/data/owid-covid-data.csv"
download.file(url,"covid.csv")

owid = read.csv("covid.csv")

owid_temp = owid |>
  # remove counties and agggregated data
  dplyr::filter(
    location != "World" |
      location != "Africa" |
      location != "Asia" |
      location != "Europe" |
      location != "North America" |
      location != "South America" |
      location != "Oceania"
  ) |>
  dplyr::select(1:6) |>
  dplyr::select(-c("total_cases")) |>
  dplyr::mutate(date = lubridate::ymd(date)) |>
  dplyr::mutate(wy = isodate(date)) |>
  dplyr::group_by(iso_code, continent, location, wy) |>
  dplyr::summarise_if(is.numeric, sum) |>
  dplyr::mutate(location = ifelse(location == "Congo", "Republic of the Congo", location)) |>
  dplyr::mutate(location = ifelse(location == "Samoa", "American Samoa", location)) |>
  dplyr::mutate(location = ifelse(location == "United States", "USA", location)) |>
  dplyr::mutate(
    location = ifelse(
      location == "Democratic Republic of Congo",
      "Democratic Republic of the Congo",
      location
    )
  ) |>
  dplyr::mutate(location = ifelse(location == "Sint Maarten (Dutch part)", "Sint Maarten", location)) |>
  dplyr::mutate(
    location = ifelse(
      location == "Democratic Republic of Congo",
      "Democratic Republic of the Congo",
      location
    )
  ) |>
  {
    \(.) {
      replace(., is.na(.), 0)
    }
  }()

owid_temp$total_cases = ave(owid_temp$new_cases,
                            owid_temp$location,
                            FUN = cumsum)

owid_temp2 = owid |>
  # remove counties and agggregated data
  dplyr::filter(
    location != "World" |
      location != "Africa" |
      location != "Asia" |
      location != "Europe" |
      location != "North America" |
      location != "South America" |
      location != "Oceania"
  ) |>
  dplyr::select(1:4, 48:67) |>
  dplyr::mutate(date = lubridate::ymd(date)) |>
  dplyr::mutate(wy = isodate(date)) |>
  dplyr::group_by(iso_code, continent, location, wy) |>
  dplyr::summarise_if(is.numeric, mean) |>
  dplyr::mutate(location = ifelse(location == "Congo", "Republic of the Congo", location)) |>
  dplyr::mutate(location = ifelse(location == "Samoa", "American Samoa", location)) |>
  dplyr::mutate(location = ifelse(location == "United States", "USA", location)) |>
  dplyr::mutate(
    location = ifelse(
      location == "Democratic Republic of Congo",
      "Democratic Republic of the Congo",
      location
    )
  ) |>
  dplyr::mutate(location = ifelse(location == "Sint Maarten (Dutch part)", "Sint Maarten", location))



owid = right_join(owid_temp, owid_temp2) |>
  dplyr::rename(country = location)

rm(owid_temp, owid_temp2)
