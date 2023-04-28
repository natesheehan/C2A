## ---------------------------
##
## Script name:
##
## Purpose of script:
##
## Author: Nathanael Sheehan
##
## Date Created: 2023-01-03
##
## Copyleft (c) Nathanael Sheehan, 2023
##
## ---------------------------
##
## Notes:
##
##
## ---------------------------

##################################################################
##                       Dataframe set up                       ##
##################################################################
gisaid = readRDS("data/gisaid.RDS")
embl = readRDS("data/embl.RDS")
source("code/owid-data.r")

df = main_df %>% select(country,Sub.region.Name, Income.group)
ppp=unique(df) %>% tidyr::drop_na()

# GISAID
gis = left_join(gisaid,owid) |>
  select(country,wy,GISAID.weekly.submissions,GISAID.total.submissions,new_cases,population) |>
  {
    \(.) {
      replace(., is.na(.), 0)
    }
  }() |>
  dplyr::mutate(`perc` = GISAID.weekly.submissions/new_cases*100) |>
  inner_join(ppp) |>
  dplyr::filter(wy >= "20/04", wy <=  "22/38") # Between April 2020 to September 2022


# The covid-19 data portal
ena = left_join(embl,owid) |>
  select(country,wy,C19DP.weekly.submissions,CD19DP.total.submissions,new_cases) |>
  {
    \(.) {
      replace(., is.na(.), 0)
    }
  }() |>
  dplyr::mutate(`perc` = C19DP.weekly.submissions/new_cases*100) |>
  inner_join(ppp) |>
  dplyr::filter(wy >= "20/04", wy <=  "22/38") # Between April 2020 to September 2022



##################################################################
##                            GISAID                            ##
##################################################################

# How many unique countries does GISAID have?
nrow(as.data.frame(unique(gis$country))) # 194
#TODO check for missing countries

# What is the percent of the database reaching over 5% prevelance of new cases?
table(gis$perc > 5)
# FALSE  TRUE
# 14409 9694
# 40.21906%

# Who are the leading countries?
gis_main = gis %>% filter(country != "Puerto Rico", country != "Guam", country != "Palau", country != "Tanzania",country != "Tanzania", country != "Gambia",
                          country != "Bhutan", country != "Comoros", country != "Seychelles", country != "Laos", country != "Lesotho", country != "Sierra Leone", country != "Benin", country != "Somalia") %>%
  filter(perc > 5)
table(gis_main$country,exclude = FALSE) |> as.data.frame() |> arrange(desc(Freq)) |> dplyr::mutate(`%` = Freq/140*100)  %>% rename(country = Var1) %>%  inner_join(ppp)
#           country Freq         %           Sub.region.Name        Income.group
# 1           Japan  140 100.00000              Eastern Asia         High income
# 2       Australia  139  99.28571 Australia and New Zealand         High income
# 3  United Kingdom  139  99.28571           Northern Europe         High income
# 4         Denmark  135  96.42857           Northern Europe         High income
# 5          Canada  134  95.71429          Northern America         High income
# 6          Norway  133  95.00000           Northern Europe         High income
# 7     Switzerland  129  92.14286            Western Europe         High income
# 8        Slovenia  128  91.42857           Southern Europe         High income
# 9     New Zealand  125  89.28571 Australia and New Zealand         High income
# 10      Hong Kong  124  88.57143              Eastern Asia         High income
# 11     Luxembourg  124  88.57143            Western Europe         High income
# 12        Finland  122  87.14286           Northern Europe         High income
# 13      Singapore  118  84.28571        South-eastern Asia         High income
# 14    South Korea  116  82.85714              Eastern Asia         High income
# 15        Senegal  114  81.42857        Sub-Saharan Africa Lower middle income
# 16    Netherlands  110  78.57143            Western Europe         High income
# 17        Ireland  109  77.85714           Northern Europe         High income
# 18          Kenya  105  75.00000        Sub-Saharan Africa Lower middle income
# 19        Austria  104  74.28571            Western Europe         High income
# 20        Belgium  102  72.85714            Western Europe         High income


# What is the regional share of sequences?
gis_main_date = gis %>% filter(wy == "22/38")
total = sum(gis_main_date$GISAID.total.submissions)

aggregate(GISAID.total.submissions ~ Sub.region.Name, gis_main_date, sum) |>
  mutate(percent = GISAID.total.submissions/total*100) |>
  arrange(desc(percent))

aggregate(GISAID.total.submissions ~ country, gis_main_date, sum) |>
  mutate(percent = GISAID.total.submissions/total*100) |>
  arrange(desc(percent))

# Sub.region.Name GISAID.total.submissions      percent
# 1                 Northern America                 48891984 34.133164709
# 2                  Northern Europe                 43220404 30.173640908
# 3                   Western Europe                 22323638 15.584894504
# 4                     Eastern Asia                  6902355  4.818769885
# 5                  Southern Europe                  5112263  3.569045490
# 6  Latin America and the Caribbean                  4365727  3.047863199
# 7                     Western Asia                  2643822  1.845742480
# 8        Australia and New Zealand                  2380324  1.661785522
# 9                   Eastern Europe                  2312698  1.614573501
# 10                   Southern Asia                  2058115  1.436840409
# 11              South-eastern Asia                  1847643  1.289902714
# 12              Sub-Saharan Africa                  1023343  0.714430717
# 13                      Micronesia                    55150  0.038502100
# 14                 Northern Africa                    51538  0.035980439
# 15                       Melanesia                    30401  0.021223977
# 16                    Central Asia                    16721  0.011673501
# 17                       Polynesia                     2816  0.001965946

# What is the share between income groups?

aggregate(GISAID.total.submissions ~ Income.group, gis_main_date, sum) |>
  mutate(percent = GISAID.total.submissions/total*100) |>
  arrange(desc(percent))

# Income.group GISAID.total.submissions      percent
# 1         High income                133449947 93.165968093
# 2 Upper middle income                  6376626  4.451740505
# 3 Lower middle income                  3306226  2.308189347
# 4          Low income                    99963  0.069787586
# 5                                         6180  0.004314469


##################################################################
##                   The Covid-19 Data Portal                   ##
##################################################################

# How many unique countries does GISAID have?
nrow(as.data.frame(unique(ena$country))) # 114
#TODO check for missing countries

# What is the percent of the database reaching over 5% prevelance of new cases?
table(ena$perc > 5)
# FALSE  TRUE
# 12701  3259
#  20.4198%

# Who are the leading countries?
ena_main = ena  %>% filter(country != "Puerto Rico", country != "Guam", country != "Palau", country != "Tanzania",country != "Tanzania", country != "Gambia",
                           country != "Bhutan", country != "Comoros", country != "Seychelles", country != "Laos", country != "Lesotho", country != "Sierra Leone", country != "Benin", country != "Somalia") %>%
  filter(perc > 5)
table(ena_main$country) |> as.data.frame() |> arrange(desc(Freq)) |> dplyr::mutate(`%` = Freq/147*100)  %>% head(20) %>% rename(country = Var1) %>%  inner_join(ppp)
#           country Freq        %                 Sub.region.Name        Income.group
# 1   Liechtenstein  107 72.78912                  Western Europe         High income
# 2  United Kingdom   98 66.66667                 Northern Europe         High income
# 3     New Zealand   97 65.98639       Australia and New Zealand         High income
# 4        Djibouti   89 60.54422              Sub-Saharan Africa Lower middle income
# 5         Iceland   80 54.42177                 Northern Europe         High income
# 6         Denmark   69 46.93878                 Northern Europe         High income
# 7     Switzerland   66 44.89796                  Western Europe         High income
# 8        Cambodia   59 40.13605              South-eastern Asia Lower middle income
# 9             USA   58 39.45578                Northern America         High income
# 10      Hong Kong   51 34.69388                    Eastern Asia         High income
# 11       Slovakia   48 32.65306                  Eastern Europe         High income
# 12       Cameroon   44 29.93197              Sub-Saharan Africa Lower middle income
# 13       Mongolia   41 27.89116                    Eastern Asia Lower middle income
# 14      Australia   40 27.21088       Australia and New Zealand         High income
# 15       Thailand   40 27.21088              South-eastern Asia Upper middle income
# 16        Germany   37 25.17007                  Western Europe         High income
# 17          Kenya   37 25.17007              Sub-Saharan Africa Lower middle income
# 18          Gabon   33 22.44898              Sub-Saharan Africa Upper middle income
# 19        Vietnam   32 21.76871              South-eastern Asia Lower middle income
# 20         Belize   31 21.08844 Latin America and the Caribbean Upper middle income

# What is the regional share of sequences?
ena_main_date = ena %>% filter(wy == "22/38")
total = sum(ena_main_date$CD19DP.total.submissions)

aggregate(CD19DP.total.submissions ~ country, ena_main_date, sum) |>
  mutate(percent = CD19DP.total.submissions/total*100) |>
  arrange(desc(percent))

aggregate(CD19DP.total.submissions ~ Sub.region.Name, ena_main_date, sum) |>
  mutate(percent = CD19DP.total.submissions/total*100) |>
  arrange(desc(percent))

# Sub.region.Name CD19DP.total.submissions      percent
# 1                 Northern America                  2875029 46.115206011
# 2                  Northern Europe                  2569191 41.209592059
# 3                   Western Europe                   638310 10.238434864
# 4                   Eastern Europe                    36599  0.587044661
# 5  Latin America and the Caribbean                    27924  0.447898443
# 6        Australia and New Zealand                    19312  0.309762739
# 7               Sub-Saharan Africa                    18802  0.301582385
# 8                     Western Asia                    14878  0.238641779
# 9                     Eastern Asia                     9837  0.157784593
# 10                   Southern Asia                     9320  0.149491960
# 11              South-eastern Asia                     8872  0.142306080
# 12                 Southern Europe                     3743  0.060037383
# 13                 Northern Africa                     1924  0.030860787
# 14                    Central Asia                      396  0.006351804
# 15                      Micronesia                      312  0.005004452

# What is the share between income groups?
aggregate(CD19DP.total.submissions ~ Income.group, ena_main_date, sum) |>
  mutate(percent = CD19DP.total.submissions/total*100) |>
  arrange(desc(percent))

# Income.group CD19DP.total.submissions      percent
# 1         High income                  6166684 98.913055508
# 2 Upper middle income                    38120  0.611441364
# 3 Lower middle income                    26991  0.432933207
# 4          Low income                     2490  0.039939376
# 5                                          164  0.002630545
