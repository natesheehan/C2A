# DATA
ddjb = read.csv("../BIB DATA/DDJB/CSV/Dimensions-Publication-2023-08-23_13-20-29.csv",
                skip = 1)#
gisaid = read.csv("../BIB DATA/GISAID/CSV/Dimensions-Publication-2023-08-23_13-20-00.csv",
                  skip = 1)
ncbi = read.csv("../BIB DATA/NCBI/CSV/Dimensions-Publication-2023-08-23_13-20-37.csv",
                skip = 1)
ena = read.csv("../BIB DATA/ENA/CSV/Dimensions-Publication-2023-08-23_13-20-19.csv",
               skip = 1)


ddjb.refs = read.csv("../BIB DATA/ddjb/bib CSV/Dimensions-Publication-2023-08-24_13-58-27.csv",
                     skip = 1)

class = read.csv("../CLASS.csv")
