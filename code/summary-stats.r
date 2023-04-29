## ---------------------------
##
## Script name: summary-stats.r
##
## Purpose of script: conduct summary statistics for the results of submissions
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
nrow(as.data.frame(unique(gis$country))) # 197
#TODO check for missing countries

# What is the percent of the database reaching over 5% prevalence of new cases?
table(gis$perc > 5)
# FALSE  TRUE
# 14795 12861
paste0("True (%): ",round(table(gis$perc > 5)[2]/(table(gis$perc > 5)[1] + table(gis$perc > 5)[2])*100))
# "True (%): 47"

# Who are the leading countries?
gis_main = gis %>% filter(country != "Puerto Rico", country != "Guam", country != "Palau", country != "Tanzania",country != "Tanzania", country != "Gambia",
                          country != "Bhutan", country != "Comoros", country != "Seychelles", country != "Laos", country != "Lesotho", country != "Sierra Leone", country != "Benin", country != "Somalia") %>%
  filter(perc > 5)
table(gis_main$country,exclude = FALSE) |> as.data.frame() |>
  arrange(desc(Freq)) |>
  dplyr::mutate(`%` = Freq/157*100)  %>%
  rename(country = Var1) %>%  inner_join(ppp)
#                               country Freq      %           continent       Income.classifications..World.Bank..2021..

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
# 18                               USA  126  80.2547771 North America                                High income
# 19                           Belgium  125  79.6178344        Europe                                High income
# 20                            Latvia  125  79.6178344        Europe                                High income

# What is the regional share of sequences?
gis_main_date = gis %>% filter(wy == "23/01")
total = sum(gis_main_date$GISAID.total.submissions)

aggregate(GISAID.total.submissions ~ continent, gis_main_date, sum) |>
  mutate(percent = GISAID.total.submissions/total*100) |>
  arrange(desc(percent))

# continent GISAID.total.submissions   percent
# 1        Europe                109051388 48.9576525
# 2 North America                 76980674 34.5597902
# 3          Asia                 25131759 11.2826801
# 4 South America                  5436054  2.4404682
# 5       Oceania                  4479038  2.0108243
# 6        Africa                  1667445  0.7485846

aggregate(GISAID.total.submissions ~ country, gis_main_date, sum) |>
  mutate(percent = GISAID.total.submissions/total*100) |>
  arrange(desc(percent))

#                               country GISAID.total.submissions        percent
# 1                                USA                 67172292 30.15640417340
# 2                     United Kingdom                 41755536 18.74577720368
# 3                            Germany                 14997570  6.73302591102
# 4                              Japan                 10974890  4.92707943624
# 5                            Denmark                 10447252  4.69020104023
# 6                             France                  9260065  4.15722397580
# 7                             Canada                  7503502  3.36863061079
# 8                            Austria                  4169943  1.87205889131
# 9                          Australia                  3612300  1.62171001692
# 10                            Sweden                  3232211  1.45107243459
# 11                             Spain                  3130419  1.40537381985
# 12                            Brazil                  3083940  1.38450748542
# 13                             India                  2885504  1.29542140482
# 14                            Israel                  2705407  1.21456845548
# 15                           Belgium                  2666439  1.19707411782
# 16                       Netherlands                  2385245  1.07083456781
# 17                       South Korea                  2379194  1.06811802508
# 18                             Italy                  2348147  1.05417975005
# 19                       Switzerland                  1896612  0.85146712028
# 20                           Ireland                  1673937  0.75149915582

# What is the share between income groups?

aggregate(GISAID.total.submissions ~ Income.classifications..World.Bank..2021.., gis_main_date, sum) |>
  mutate(percent = GISAID.total.submissions/total*100) |>
  arrange(desc(percent))

# # Income.classifications..World.Bank..2021.. GISAID.total.submissions     percent
# 1                                High income                205262607 92.150825200
# 2                        Upper-middle income                 11819043  5.306054432
# 3                        Lower-middle income                  5490369  2.464852422
# 4                                 Low income                   154641  0.069424704
# 5                            Not categorized                    19698  0.008843242


##################################################################
##                   The Covid-19 Data Portal                   ##
##################################################################

# How many unique countries does GISAID have?
nrow(as.data.frame(unique(ena$country))) # 117
#TODO check for missing countries

# What is the percent of the database reaching over 5% prevelance of new cases?
table(ena$perc > 5)
# FALSE  TRUE
# 14599  3887
paste0("True (%): ",round(table(ena$perc > 5)[2]/(table(ena$perc > 5)[1] + table(ena$perc > 5)[2])*100))
# "True (%): 21"


# Who are the leading countries?
ena_main = ena  %>% filter(country != "Puerto Rico", country != "Guam", country != "Palau", country != "Tanzania",country != "Tanzania", country != "Gambia",
                           country != "Bhutan", country != "Comoros", country != "Seychelles", country != "Laos", country != "Lesotho", country != "Sierra Leone", country != "Benin", country != "Somalia") %>%
  filter(perc > 5)
table(ena_main$country) |> as.data.frame() |> arrange(desc(Freq)) |> dplyr::mutate(`%` = Freq/147*100)  %>% head(20) %>% rename(country = Var1) %>%  inner_join(ppp)
# country Freq        %     continent Income.classifications..World.Bank..2021..
#         Hong Kong  158 107.48299          Asia                                High income
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
# 12            USA   63  42.85714 North America                                High income
# 13          Kenya   51  34.69388        Africa                        Lower-middle income
# 14          Gabon   46  31.29252        Africa                        Upper-middle income
# 15        Belarus   45  30.61224        Europe                        Upper-middle income
# 16       Mongolia   44  29.93197          Asia                        Lower-middle income
# 17          Syria   43  29.25170          Asia                                 Low income
# 18       Suriname   42  28.57143 South America                        Upper-middle income
# 19       Thailand   42  28.57143          Asia                        Upper-middle income
# 20      Australia   41  27.89116       Oceania                                High income

# What is the regional share of sequences?
ena_main_date = ena %>% filter(wy == "23/01")
total = sum(ena_main_date$CD19DP.total.submissions)

aggregate(CD19DP.total.submissions ~ country, ena_main_date, sum) |>
  mutate(percent = CD19DP.total.submissions/total*100) |>
  arrange(desc(percent))
#           country       CD19DP.total.submissions      percent
# 1                  USA                  3020654 46.424190189
# 2       United Kingdom                  2222832 34.162527560
# 3              Germany                   447069  6.870967772
# 4              Denmark                   403059  6.194581595
# 5          Switzerland                   152532  2.344252131
# 6               France                    48011  0.737877226
# 7             Slovakia                    35825  0.550591565
# 8              Iceland                    25539  0.392506852
# 9          New Zealand                    16678  0.256322851
# 10              Mexico                    16049  0.246655800
# 11           Australia                    13035  0.200333874
# 12              Brazil                    12021  0.184749789
# 13             Bahrain                    10645  0.163602155
# 14               Japan                     8114  0.124703418
# 15               Kenya                     7865  0.120876557
# 16        South Africa                     4880  0.075000330
# 17            Thailand                     4761  0.073171429
# 18               India                     4109  0.063150893
# 19             Nigeria                     3234  0.049703088
# 20         Puerto Rico                     2604  0.040020668

aggregate(CD19DP.total.submissions ~ continent, ena_main_date, sum) |>
  mutate(percent = CD19DP.total.submissions/total*100) |>
  arrange(desc(percent))

# continent CD19DP.total.submissions    percent
# 1        Europe                  3348366 51.4607698
# 2 North America                  3041208 46.7400830
# 3          Asia                    47210  0.7255667
# 4       Oceania                    30055  0.4619129
# 5        Africa                    24437  0.3755703
# 6 South America                    15362  0.2360974

# What is the share between income groups?
aggregate(CD19DP.total.submissions ~ Income.classifications..World.Bank..2021.., ena_main_date, sum) |>
  mutate(percent = CD19DP.total.submissions/total*100) |>
  arrange(desc(percent))

# Income.classifications..World.Bank..2021.. CD19DP.total.submissions      percent
# Income.classifications..World.Bank..2021.. CD19DP.total.submissions      percent
# 1                                High income                  6425491 98.752858235
# 2                        Upper-middle income                    46613  0.716391476
# 3                        Lower-middle income                    31699  0.487179400
# 4                                 Low income                     2656  0.040819852
# 5                            Not categorized                      179  0.002751037
