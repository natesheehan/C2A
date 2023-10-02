# DATA
ddbj = read.csv("data-raw/ddjb-BIB/Dimensions-Publication-2023-09-04_13-00-55.csv",
                skip = 1)#
gisaid = read.csv("data-raw/GISAID-BIB/Dimensions-Publication-2023-09-04_12-44-03.csv",
                  skip = 1)
ncbi = read.csv("data-raw/NCBI-BIB/Dimensions-Publication-2023-09-04_12-44-47.csv",
                skip = 1)
ena = read.csv("data-raw/ENA-BIB/Dimensions-Publication-2023-09-04_12-50-57.csv",
               skip = 1)

class = read.csv("data-raw/CLASS.csv")

# Apply function to the Authors column
gisaid$Authors <- sapply(gisaid$Authors, clean_duplicates)
ena$Authors <- sapply(ena$Authors, clean_duplicates)
ddbj$Authors <- sapply(ddbj$Authors, clean_duplicates)
ncbi$Authors <- sapply(ncbi$Authors, clean_duplicates)

gisaid_clean = remove_duplicate(gisaid)
ncbi_clean = remove_duplicate(ncbi)
ddbj_clean = remove_duplicate(ddbj)
ena_clean = remove_duplicate(ena)

gisaid_clean = remove_empty_strings(gisaid_clean)
ncbi_clean = remove_empty_strings(ncbi_clean)
ddbj_clean = remove_empty_strings(ddbj_clean)
ena_clean = remove_empty_strings(ena_clean)

gisaid = gisaid_clean
ena = ena_clean
ncbi = ncbi_clean
ddbj = ddbj_clean

rm(gisaid_clean,ena_clean,ddbj_clean,ncbi_clean)






