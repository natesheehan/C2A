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
gisaid = readRDS("data/GISAID/gisaid.RDS")
embl = readRDS("data/CV19DP/CV19DP.RDS")
source("code/owid-data.r")

df = main_df %>% select(country,continent, Income.classifications..World.Bank..2021..)
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
  dplyr::filter(wy >= "20/01", wy <=  "23/01") # Between April 2020 to September 2022


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
  dplyr::filter(wy >= "20/01", wy <=  "23/01") # Between April 2020 to September 2022



##################################################################
##                            GISAID                            ##
##################################################################

# How many unique countries does GISAID have?
nrow(as.data.frame(unique(gis$country))) # 193
#TODO check for missing countries

# What is the percent of the database reaching over 5% prevalence of new cases?
table(gis$perc > 5)
# FALSE  TRUE
# 14519 12448
# 46.16% achieved above the 5% guideline

# Who are the leading countries?
gis_main = gis %>% filter(country != "Puerto Rico", country != "Guam", country != "Palau", country != "Tanzania",country != "Tanzania", country != "Gambia",
                          country != "Bhutan", country != "Comoros", country != "Seychelles", country != "Laos", country != "Lesotho", country != "Sierra Leone", country != "Benin", country != "Somalia") %>%
  filter(perc > 5)
table(gis_main$country,exclude = FALSE) |> as.data.frame() |>
  arrange(desc(Freq)) |>
  dplyr::mutate(`%` = Freq/157*100)  %>%
  rename(country = Var1) %>%  inner_join(ppp)
# country Freq           %     continent Income.classifications..World.Bank..2021..
# 1                              Japan  157 100.0000000          Asia                                High income
# 2                          Hong Kong  155  98.7261146          Asia                                High income
# 3                          Australia  154  98.0891720       Oceania                                High income
# 4                     United Kingdom  154  98.0891720        Europe                                High income
# 5                             Canada  152  96.8152866 North America                                High income
# 6                             Norway  151  96.1783439        Europe                                High income
# 7                            Denmark  150  95.5414013        Europe                                High income
# 8                        Switzerland  147  93.6305732        Europe                                High income
# 9                        New Zealand  145  92.3566879       Oceania                                High income
# 10                          Slovenia  143  91.0828025        Europe                                High income
# 11                           Finland  142  90.4458599        Europe                                High income
# 12                        Luxembourg  142  90.4458599        Europe                                High income
# 13                         Singapore  141  89.8089172          Asia                                High income
# 14                       South Korea  138  87.8980892          Asia                                High income
# 15                             Kenya  136  86.6242038        Africa                        Lower-middle income
# 16                           Ireland  131  83.4394904        Europe                                High income
# 17                       Netherlands  131  83.4394904        Europe                                High income
# 18                           Belgium  125  79.6178344        Europe                                High income
# 19                            Latvia  125  79.6178344        Europe                                High income
# 20                           Senegal  125  79.6178344        Africa                        Lower-middle income

# What is the regional share of sequences?
gis_main_date = gis %>% filter(wy == "23/01")
total = sum(gis_main_date$GISAID.total.submissions)

aggregate(GISAID.total.submissions ~ continent, gis_main_date, sum) |>
  mutate(percent = GISAID.total.submissions/total*100) |>
  arrange(desc(percent))

# continent GISAID.total.submissions   percent
# 1        Europe                109051388 70.122401
# 2          Asia                 25131759 16.160265
# 3 North America                  9773949  6.284861
# 4 South America                  5436054  3.495500
# 5       Oceania                  4473078  2.876286
# 6        Africa                  1649537  1.060688

aggregate(GISAID.total.submissions ~ country, gis_main_date, sum) |>
  mutate(percent = GISAID.total.submissions/total*100) |>
  arrange(desc(percent))

# country GISAID.total.submissions        percent
# 1                     United Kingdom                 41755536 26.84971263203
# 2                            Germany                 14997570  9.64376183984
# 3                              Japan                 10974890  7.05709160740
# 4                            Denmark                 10447252  6.71780896297
# 5                             France                  9260065  5.95442204847
# 6                             Canada                  7503502  4.82491405293
# 7                            Austria                  4169943  2.68136352607
# 8                          Australia                  3612300  2.32278701777
# 9                             Sweden                  3232211  2.07838157115
# 10                             Spain                  3130419  2.01292711385
# 11                            Brazil                  3083940  1.98304011172
# 12                             India                  2885504  1.85544147244
# 13                            Israel                  2705407  1.73963520676
# 14                           Belgium                  2666439  1.71457794006
# 15                       Netherlands                  2385245  1.53376411710
# 16                       South Korea                  2379194  1.52987319324
# 17                             Italy                  2348147  1.50990930084
# 18                       Switzerland                  1896612  1.21956253117
# 19                           Ireland                  1673937  1.07637769071
# 20                            Poland                  1263573  0.81250476439

# What is the share between income groups?

aggregate(GISAID.total.submissions ~ Income.classifications..World.Bank..2021.., gis_main_date, sum) |>
  mutate(percent = GISAID.total.submissions/total*100) |>
  arrange(desc(percent))

# Income.classifications..World.Bank..2021.. GISAID.total.submissions     percent
# 1                                High income                138055882 88.77291765
# 2                        Upper-middle income                 11819043  7.59990024
# 3                        Lower-middle income                  5476999  3.52182880
# 4                                 Low income                   144143  0.09268707
# 5                            Not categorized                    19698  0.01266624


##################################################################
##                   The Covid-19 Data Portal                   ##
##################################################################

# How many unique countries does GISAID have?
nrow(as.data.frame(unique(ena$country))) # 116
#TODO check for missing countries

# What is the percent of the database reaching over 5% prevelance of new cases?
table(ena$perc > 5)
# FALSE  TRUE
# 14504  3824
#  20.86425%

# Who are the leading countries?
ena_main = ena  %>% filter(country != "Puerto Rico", country != "Guam", country != "Palau", country != "Tanzania",country != "Tanzania", country != "Gambia",
                           country != "Bhutan", country != "Comoros", country != "Seychelles", country != "Laos", country != "Lesotho", country != "Sierra Leone", country != "Benin", country != "Somalia") %>%
  filter(perc > 5)
table(ena_main$country) |> as.data.frame() |> arrange(desc(Freq)) |> dplyr::mutate(`%` = Freq/147*100)  %>% head(20) %>% rename(country = Var1) %>%  inner_join(ppp)
# country Freq        %     continent Income.classifications..World.Bank..2021..
# 1       Hong Kong  158 107.48299          Asia                                High income
# 2          Taiwan  158 107.48299          Asia                                High income
# 3   Liechtenstein  124  84.35374        Europe                                High income
# 4  United Kingdom  122  82.99320        Europe                                High income
# 5     New Zealand  112  76.19048       Oceania                                High income
# 6        Djibouti  110  74.82993        Africa                        Lower-middle income
# 7         Iceland   87  59.18367        Europe                                High income
# 8         Denmark   70  47.61905        Europe                                High income
# 9     Switzerland   70  47.61905        Europe                                High income
# 10       Slovakia   69  46.93878        Europe                                High income
# 11       Cambodia   65  44.21769          Asia                        Lower-middle income
# 12          Kenya   51  34.69388        Africa                        Lower-middle income
# 13          Gabon   46  31.29252        Africa                        Upper-middle income
# 14        Belarus   45  30.61224        Europe                        Upper-middle income
# 15       Mongolia   44  29.93197          Asia                        Lower-middle income
# 16          Syria   43  29.25170          Asia                                 Low income
# 17       Suriname   42  28.57143 South America                        Upper-middle income
# 18       Thailand   42  28.57143          Asia                        Upper-middle income
# 19      Australia   41  27.89116       Oceania                                High income
# 20         Belize   39  26.53061 North America                        Upper-middle income

# What is the regional share of sequences?
ena_main_date = ena %>% filter(wy == "23/01")
total = sum(ena_main_date$CD19DP.total.submissions)

aggregate(CD19DP.total.submissions ~ country, ena_main_date, sum) |>
  mutate(percent = CD19DP.total.submissions/total*100) |>
  arrange(desc(percent))
#           country       CD19DP.total.submissions      percent
# 1       United Kingdom                  2222832 63.764836557
# 2              Germany                   447069 12.824757658
# 3              Denmark                   403059 11.562273378
# 4          Switzerland                   152532  4.375579463
# 5               France                    48011  1.377258186
# 6             Slovakia                    35825  1.027686874
# 7              Iceland                    25539  0.732619542
# 8          New Zealand                    16678  0.478430194
# 9               Mexico                    16049  0.460386508
# 10           Australia                    13035  0.373925985
# 11              Brazil                    12021  0.344838072
# 12             Bahrain                    10645  0.305365716
# 13               Japan                     8114  0.232760678
# 14               Kenya                     7865  0.225617788
# 15        South Africa                     4880  0.139989168
# 16            Thailand                     4761  0.136575498
# 17               India                     4109  0.117872027
# 18             Nigeria                     3234  0.092771510
# 19         Puerto Rico                     2604  0.074699138
# 20             Vietnam                     2503  0.071801821

aggregate(CD19DP.total.submissions ~ continent, ena_main_date, sum) |>
  mutate(percent = CD19DP.total.submissions/total*100) |>
  arrange(desc(percent))

# continent CD19DP.total.submissions    percent
# 1        Europe                  3348366 96.0522481
# 2          Asia                    47210  1.3542805
# 3       Oceania                    30055  0.8621669
# 4        Africa                    24437  0.7010072
# 5 North America                    20554  0.5896183
# 6 South America                    15362  0.4406790

# What is the share between income groups?
aggregate(CD19DP.total.submissions ~ Income.classifications..World.Bank..2021.., ena_main_date, sum) |>
  mutate(percent = CD19DP.total.submissions/total*100) |>
  arrange(desc(percent))

# Income.classifications..World.Bank..2021.. CD19DP.total.submissions      percent
# 1                                High income                  3404837 97.672192414
# 2                        Upper-middle income                    46613  1.337154732
# 3                        Lower-middle income                    31699  0.909327180
# 4                                 Low income                     2656  0.076190826
# 5                            Not categorized                      179  0.005134849
